
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate_bsvar(specification_no1, 3, 1, show_progress = FALSE)
fevd                <- compute_variance_decompositions(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  fevd2               <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate_bsvar(S = 3, thin = 1, show_progress = FALSE) |>
    compute_variance_decompositions(horizon = 2)
)

expect_error(
  compute_variance_decompositions(run_no1),
  info = "compute_variance_decompositions: specify horizon."
)

expect_identical(
  sum(fevd[1,,1,1]), 100,
  info = "compute_variance_decompositions: sum tp 100%."
)

expect_identical(
  fevd[3,3,3,3], fevd2[3,3,3,3],
  info = "compute_variance_decompositions: identical for normal and pipe workflow."
)
