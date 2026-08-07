N       <- 1L
K       <- 1L
T       <- 4L
M       <- 2L
draws   <- 2L

Y       <- matrix(c(0.5, -1, 1.5, -0.75), N, T)
X       <- matrix(1, K, T)
lambda_path <- c(4, 0.25, 2.25, 1)
lambda  <- array(rep(lambda_path, draws), c(N, T, draws))
lambda1 <- array(1, dim(lambda))
Y1      <- Y / sqrt(lambda_path)

B <- array(1, c(N, N, draws))
A <- array(0, c(N, K, draws))

# Student-t SV ordinates must use u / sqrt(lambda).
h_path <- c(0.2, -0.1, 0.3, 0.4)
posterior_sv <- list(
  B            = B,
  A            = A,
  h            = array(rep(h_path, draws), c(N, T, draws)),
  S            = array(0, c(N, T, draws)),
  sigma2_omega = matrix(1, N, draws),
  s_           = matrix(1, N, draws),
  lambda       = lambda
)
prior_sv <- list(sv_a_ = 1, sv_s_ = 1)

out_sv <- .Call(
  "_bsvars_verify_volatility_sv_cpp",
  posterior_sv, prior_sv, Y, X, TRUE,
  PACKAGE = "bsvars"
)
posterior_sv$lambda <- lambda1
out_sv_reference <- .Call(
  "_bsvars_verify_volatility_sv_cpp",
  posterior_sv, prior_sv, Y1, X, TRUE,
  PACKAGE = "bsvars"
)

expect_equal(
  out_sv$components$log_numerator_s,
  out_sv_reference$components$log_numerator_s,
  tolerance = 1e-12,
  info = "SV verification composes residuals with Student-t scales"
)

# Student-t MSH ordinates must accumulate u^2 / lambda by regime.
xi_draw <- rbind(c(1, 0, 1, 0), c(0, 1, 0, 1))
posterior_msh <- list(
  sigma2 = array(1, c(N, M, draws)),
  B      = B,
  A      = A,
  xi     = array(rep(xi_draw, draws), c(M, T, draws)),
  lambda = lambda
)
prior_msh <- list(sigma_nu = 3, sigma_s = 1)

out_msh <- .Call(
  "_bsvars_verify_volatility_msh_cpp",
  posterior_msh, prior_msh, Y, X,
  PACKAGE = "bsvars"
)
posterior_msh$lambda <- lambda1
out_msh_reference <- .Call(
  "_bsvars_verify_volatility_msh_cpp",
  posterior_msh, prior_msh, Y1, X,
  PACKAGE = "bsvars"
)

expect_equal(
  out_msh$components$log_numerator_s,
  out_msh_reference$components$log_numerator_s,
  tolerance = 1e-12,
  info = "MSH verification composes residuals with Student-t scales"
)

# HMSH uses the same scale composition with shock-specific state paths.
xi_hmsh <- array(xi_draw, c(M, T, N))
posterior_hmsh <- list(
  sigma2 = array(1, c(N, M, draws)),
  B      = B,
  A      = A,
  xi_cpp = structure(rep(list(xi_hmsh), draws), dim = c(draws, 1L)),
  lambda = lambda
)

out_hmsh <- .Call(
  "_bsvars_verify_volatility_hmsh_cpp",
  posterior_hmsh, prior_msh, Y, X,
  PACKAGE = "bsvars"
)
posterior_hmsh$lambda <- lambda1
out_hmsh_reference <- .Call(
  "_bsvars_verify_volatility_hmsh_cpp",
  posterior_hmsh, prior_msh, Y1, X,
  PACKAGE = "bsvars"
)

expect_equal(
  out_hmsh$components$log_numerator_s,
  out_hmsh_reference$components$log_numerator_s,
  tolerance = 1e-12,
  info = "HMSH verification composes residuals with Student-t scales"
)
