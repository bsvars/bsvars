.draw_msh_state <- function(probability) {
  .Call(
    bsvars:::`_bsvars_csample_num1`,
    seq_along(probability),
    probability
  )
}

.draw_finite_msh_path <- function(xi, U, sigma2, PR_TR, pi_0) {
  M <- nrow(PR_TR)
  T <- ncol(xi)
  filtered <- bsvars:::filtering_msh(U, sigma2, PR_TR, pi_0)
  smoothed <- bsvars:::smoothing_msh(U, PR_TR, filtered)

  for (iteration in seq_len(10)) {
    candidate <- matrix(0, 1, T)
    candidate[1, T] <- .draw_msh_state(smoothed[, T]) - 1

    for (t in (T - 1):1) {
      next_state <- candidate[1, t + 1] + 1
      probability <- filtered[, t] * PR_TR[, next_state]
      probability <- probability / sum(probability)
      candidate[1, t] <- .draw_msh_state(probability) - 1
    }

    if (min(tabulate(candidate + 1, nbins = M)) >= 3) {
      return(list(path = candidate, iteration = iteration))
    }
  }

  list(path = xi, iteration = NA_integer_)
}


# B11: filtering preserves likelihood ratios when densities underflow.
U <- matrix(100, 1, 1)
sigma2 <- matrix(c(1, 4), 1, 2)
PR_TR <- diag(2)
pi_0 <- rep(0.5, 2)

filtered <- bsvars:::filtering_msh(U, sigma2, PR_TR, pi_0)
log_weight <- c(
  dnorm(U[1], sd = sqrt(sigma2[1]), log = TRUE),
  dnorm(U[1], sd = sqrt(sigma2[2]), log = TRUE)
) + log(pi_0)
expected <- exp(log_weight - max(log_weight))
expected <- expected / sum(expected)

expect_equal(
  as.numeric(filtered[, 1]),
  expected,
  tolerance = 1e-12,
  info = "filtering_msh: log-scale normalization preserves extreme likelihood ratios."
)


# B02: FFBS output does not depend on the placeholder path.
T <- 8
U <- matrix(c(-2, -1, 0, 1, 2, 1, 0, -1), 1, T)
sigma2 <- matrix(c(1, 4), 1, 2)
PR_TR <- matrix(c(0.9, 0.1, 0.2, 0.8), 2, 2, byrow = TRUE)
pi_0 <- rep(0.5, 2)
xi_1 <- matrix(0, 1, T)
xi_2 <- matrix(1, 1, T)

set.seed(42)
path_1 <- bsvars:::sample_Markov_process_msh(
  xi_1, U, sigma2, PR_TR, pi_0, FALSE
)
set.seed(42)
path_2 <- bsvars:::sample_Markov_process_msh(
  xi_2, U, sigma2, PR_TR, pi_0, FALSE
)

expect_identical(
  path_1,
  path_2,
  info = "sample_Markov_process_msh: FFBS conditions on newly sampled future states."
)


# B07: finite-state acceptance counts observations, not self-transitions.
T <- 6
U <- matrix(0, 1, T)
sigma2 <- matrix(1, 1, 2)
PR_TR <- matrix(c(0, 1, 1, 0), 2, 2, byrow = TRUE)
xi <- matrix(0, 1, T)

set.seed(1)
alternating_path <- bsvars:::sample_Markov_process_msh(
  xi, U, sigma2, PR_TR, pi_0, TRUE
)

expect_identical(
  as.numeric(tabulate(alternating_path + 1, nbins = 2)),
  c(3, 3),
  info = "sample_Markov_process_msh: three non-consecutive observations per regime are accepted."
)


# B08: every retry redraws the full path and a final-attempt success is kept.
PR_TR <- matrix(0.5, 2, 2)
set.seed(45)
expected_retry <- .draw_finite_msh_path(xi, U, sigma2, PR_TR, pi_0)
set.seed(45)
actual_retry <- bsvars:::sample_Markov_process_msh(
  xi, U, sigma2, PR_TR, pi_0, TRUE
)

expect_identical(
  expected_retry$iteration,
  10L,
  info = "finite-state fixture succeeds on the final permitted attempt."
)
expect_identical(
  actual_retry,
  expected_retry$path,
  info = "sample_Markov_process_msh: complete retry path is committed on the final attempt."
)


# B09: accepting one HMSH shock cannot commit another shock's rejected path.
xi_hmsh <- array(0, c(1, T, 2))
xi_hmsh[, , 1] <- rep(0:1, 3)
xi_hmsh[, , 2] <- xi
PR_TR_hmsh <- array(0, c(2, 2, 2))
PR_TR_hmsh[, , 1] <- diag(2)
PR_TR_hmsh[, , 2] <- matrix(c(0, 1, 1, 0), 2, 2, byrow = TRUE)

set.seed(1)
path_hmsh <- bsvars:::sample_Markov_process_hmsh(
  xi_hmsh,
  matrix(0, 2, T),
  matrix(1, 2, 2),
  PR_TR_hmsh,
  matrix(0.5, 2, 2),
  TRUE
)

expect_identical(
  path_hmsh[, , 1],
  xi_hmsh[, , 1],
  info = "sample_Markov_process_hmsh: a rejected shock slice remains unchanged."
)
expect_identical(
  as.numeric(tabulate(path_hmsh[, , 2] + 1, nbins = 2)),
  c(3, 3),
  info = "sample_Markov_process_hmsh: an accepted shock slice is committed independently."
)


# B25 and B26: predecessor uses pi_0 and contributes the initial transition.
PR_TR <- matrix(c(0.85, 0.15, 0.4, 0.6), 2, 2, byrow = TRUE)
pi_0 <- c(0.9, 0.1)
xi <- matrix(c(1, 1, 0, 1, 0), 1)
prior <- list(PR_TR = matrix(1, 2, 2))

set.seed(1)
probability <- pi_0 * PR_TR[, xi[1, 1] + 1]
predecessor <- .draw_msh_state(probability / sum(probability))
transitions <- bsvars:::count_regime_transitions(xi, 2)
transitions[predecessor, xi[1, 1] + 1] <-
  transitions[predecessor, xi[1, 1] + 1] + 1
posterior_alpha <- transitions + prior$PR_TR
expected_PR_TR <- rbind(
  bsvars:::rDirichlet1(posterior_alpha[1, ]),
  bsvars:::rDirichlet1(posterior_alpha[2, ])
)
alpha_0 <- rep(1, 2)
alpha_0[predecessor] <- alpha_0[predecessor] + 1
expected_pi_0 <- as.numeric(bsvars:::rDirichlet1(alpha_0))

set.seed(1)
transition_draw <- bsvars:::sample_transition_probabilities(
  PR_TR, pi_0, xi, prior, TRUE
)

expect_identical(
  predecessor,
  1L,
  info = "transition fixture distinguishes pi_0-weighted predecessor probabilities."
)
expect_equal(
  transition_draw$PR_TR,
  expected_PR_TR,
  tolerance = 1e-12,
  info = "sample_transition_probabilities: initial transition augments the correct row and column."
)
expect_equal(
  as.numeric(transition_draw$pi_0),
  expected_pi_0,
  tolerance = 1e-12,
  info = "sample_transition_probabilities: pi_0 is conditioned on the same predecessor draw."
)
