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
