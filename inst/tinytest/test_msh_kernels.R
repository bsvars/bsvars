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
