data(us_fiscal_lsuw)
set.seed(1)
specifications = suppressMessages(list(
  specify_bsvar$new(us_fiscal_lsuw),
  specify_bsvar_t$new(us_fiscal_lsuw),
  specify_bsvar_exh$new(us_fiscal_lsuw,
    variance_regimes = rep(1:2, length.out = nrow(us_fiscal_lsuw))),
  specify_bsvar_msh$new(us_fiscal_lsuw, M = 2, distribution = "t"),
  specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2),
  specify_bsvar_mix$new(us_fiscal_lsuw, M = 2),
  specify_bsvar_sv$new(us_fiscal_lsuw)
))

for (specification in specifications) {
  set.seed(1)
  posterior = estimate(specification, S = 3, show_progress = FALSE)
  covariance = compute_conditional_covariance(posterior)
  csd = suppressMessages(compute_conditional_sd(posterior))
  model = class(posterior)[1]
  N = dim(csd)[1]
  T = dim(csd)[2]
  S = dim(csd)[3]

  expect_true(inherits(covariance, "PosteriorCovariance"), info = model)
  expect_identical(dim(covariance), c(N, N, T, S), info = model)
  expect_identical(dimnames(covariance),
    c(dimnames(csd)[1], dimnames(csd)), info = model)
  if (model %in% c("PosteriorBSVART", "PosteriorBSVARMSH")) {
    expect_true(any(posterior$posterior$lambda != 1), info = model)
  }

  structural = expected = unclass(covariance)
  for (s in 1:S) for (t in 1:T) {
    B = posterior$posterior$B[,,s]
    structural[,,t,s] = B %*% covariance[,,t,s] %*% t(B)
    expected[,,t,s] = diag(csd[,t,s]^2)
  }
  expect_equal(structural, expected, info = model)
}

# Independent hand calculation, with a non-triangular B and one saved draw.
set.seed(1)
specification = suppressMessages(specify_bsvar_sv$new(us_fiscal_lsuw[, 1:2]))
posterior = estimate(specification, S = 2, thin = 2, show_progress = FALSE)
posterior$posterior$B[,,1] = matrix(c(1, -.25, .5, 1), 2, 2)
posterior$posterior$sigma[,1,1] = c(3, 2)
before = unserialize(serialize(posterior$posterior, NULL))
covariance = compute_conditional_covariance(posterior)
expect_identical(posterior$posterior, before)
expect_equal(unname(covariance[,,1,1]), matrix(c(640, 16, 16, 292), 2, 2) / 81)
expect_identical(dim(covariance), c(2L, 2L, nrow(us_fiscal_lsuw) - 1L, 1L))
