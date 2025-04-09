
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)


H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
H0[1,3]        = 0
sddr           = verify_autoregression(run_no1, H0)


expect_true(
  is.numeric(sddr$logSDDR),
  info = "verify_autoregressive: SDDR is numeric"
)

expect_true(
  is.numeric(sddr$log_SDDR_se),
  info = "verify_autoregressive: SDDR_se is numeric"
)

expect_true(
  is.list(sddr$components),
  info = "verify_autoregressive: components is a list"
)


H0[1,3]        = "*"
expect_error(
  verify_autoregression(run_no1, H0),
  pattern = "*numeric*",
  info = "verify_autoregressive: H0 is not numeric"
)

# models with exclusion restrictions on autoregressive terms
A = matrix(TRUE, 3, 4)
A[1,3] = FALSE

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar$new(us_fiscal_lsuw, A = A)
)
run_no2             <- estimate(specification_no2, 60, 1, show_progress = FALSE)

H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
H0[1,3]        = 0
expect_error(
  verify_autoregression(run_no2, H0),
  pattern = "restricted",
  info = "verify_autoregressive: H0 verifies autoregressive restrictions"
)
