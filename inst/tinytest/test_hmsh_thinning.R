data(us_fiscal_lsuw)
T = nrow(us_fiscal_lsuw) - 1

set.seed(141)
specification = suppressMessages(
  specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
)
posterior = estimate(specification, S = 4, thin = 2, show_progress = FALSE)

expect_equal(
  dim(posterior$posterior$PR_TR),
  c(2, 2, 3, 2),
  info = "HMSH transition output contains only retained draws after thinning."
)
expect_equal(
  dim(posterior$posterior$xi),
  c(1, T, 3, 2),
  info = "HMSH state output contains only retained draws after thinning."
)
expect_false(
  anyNA(posterior$posterior$PR_TR) || anyNA(posterior$posterior$xi),
  info = "HMSH retained state output contains no unfilled slices."
)
expect_true(
  all(posterior$posterior$xi %in% 0:1),
  info = "HMSH retained state output contains valid zero-based indices."
)

continued = estimate(posterior, S = 4, thin = 2, show_progress = FALSE)

expect_equal(
  dim(continued$posterior$PR_TR),
  c(2, 2, 3, 2),
  info = "Continuation reshapes only retained HMSH transition draws."
)
expect_equal(
  dim(continued$posterior$xi),
  c(1, T, 3, 2),
  info = "Continuation reshapes only retained HMSH state draws."
)
expect_false(
  anyNA(continued$posterior$PR_TR) || anyNA(continued$posterior$xi),
  info = "Continuation retained state output contains no unfilled slices."
)
