
data(us_fiscal_lsuw)
data(us_fiscal_ex)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar$new(us_fiscal_lsuw, exogenous = us_fiscal_ex)
)
run_no2             <- estimate(specification_no2, 3, 1, show_progress = FALSE)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no2$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar with ex: the last_draw(s) of two runs to be identical."
)

expect_identical(
  run_no1$posterior$B[1,1,1],
  run_no2$posterior$B[1,1,1],
  info = "estimate_bsvar with ex: the first draws of two runs to be identical."
)


exx = us_fiscal_ex
exx[1,1] = NA
expect_error(
  specify_bsvar$new(us_fiscal_lsuw, exogenous = exx),
  pattern = "missing",
  info = "Missing value in exogenous."
)


expect_error(
  forecast(run_no1, 2),
  pattern = "exogenous",
  info = "No exogenous in forecast."
)


expect_true(
  class(forecast(run_no1, 2, exogenous_forecast = matrix(0, 2, 3))) == "Forecasts",
  info = "Exogenous in forecast."
)


exx = us_fiscal_ex
exx[,1] = 1
expect_error(
  specify_bsvar$new(us_fiscal_lsuw, exogenous = exx),
  pattern = "constant",
  info = "Constant term in exogenous."
)
