.draw_msh_state <- function(probability) {
  .Call(
    bsvars:::`_bsvars_csample_num1`,
    seq_along(probability),
    probability
  )
}

.draw_finite_msh_path <- function(xi, U, sigma2, PR_TR, pi_0) {
  M <- nrow(xi)
  T <- ncol(xi)
  filtered <- bsvars:::filtering_msh(U, sigma2, PR_TR, pi_0)
  smoothed <- bsvars:::smoothing_msh(U, PR_TR, filtered)

  for (iteration in seq_len(10)) {
    candidate <- matrix(0, M, T)
    candidate[, T] <- diag(M)[, .draw_msh_state(smoothed[, T])]

    for (t in (T - 1):1) {
      next_state <- which.max(candidate[, t + 1])
      probability <- filtered[, t] * PR_TR[, next_state]
      probability <- probability / sum(probability)
      candidate[, t] <- diag(M)[, .draw_msh_state(probability)]
    }

    if (min(rowSums(candidate)) >= 3) {
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
xi_1 <- diag(2)[, rep(1, T), drop = FALSE]
xi_2 <- diag(2)[, rep(2, T), drop = FALSE]

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
xi <- diag(2)[, rep(1, T), drop = FALSE]

set.seed(1)
alternating_path <- bsvars:::sample_Markov_process_msh(
  xi, U, sigma2, PR_TR, pi_0, TRUE
)

expect_identical(
  as.numeric(rowSums(alternating_path)),
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
