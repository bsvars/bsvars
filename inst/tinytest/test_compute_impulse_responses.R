
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
irf                 <- compute_impulse_responses(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  irf2              <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_impulse_responses(horizon = 2)
)


expect_error(
  compute_impulse_responses(run_no1),
  info = "compute_impulse_responses: specify horizon."
)

expect_identical(
  irf[3,3,3,3], irf2[3,3,3,3],
  info = "compute_impulse_responses: identical for normal and pipe workflow."
)


set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
irf                 <- compute_impulse_responses(run_no1, horizon = 2, standardise = TRUE)

set.seed(1)
suppressMessages(
  irf2              <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_impulse_responses(horizon = 2, standardise = TRUE)
)



expect_equal(
  irf[1,1,1,1], 1,
  info = "compute_impulse_responses: unit own shock at 0 horizon."
)

expect_error(
  compute_impulse_responses(run_no1),
  info = "compute_impulse_responses: specify horizon."
)

expect_identical(
  irf[3,3,3,3], irf2[3,3,3,3],
  info = "compute_impulse_responses: identical for normal and pipe workflow."
)
