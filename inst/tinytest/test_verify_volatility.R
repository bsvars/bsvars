
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_message(
  verify_volatility(run_no1),
  pattern = "*homoskedastic.",
  info = "verify_volatility: the model to be homoskedastic"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw, centred_sv = TRUE)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_message(
  verify_volatility(run_no1),
  pattern = "*known*",
  info = "verify_volatility: for centred SV"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_volatility(run_no1)$logSDDR),
  info = "verify_volatility: for non-centred SV; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_volatility(run_no1)$logSDDR),
  info = "verify_volatility: for MSH model; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_volatility(run_no1)$logSDDR),
  info = "verify_volatility: for MIX model; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, finiteM = FALSE)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_volatility(run_no1)$logSDDR),
  info = "verify_volatility: for sparse MSH model; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, finiteM = FALSE)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_volatility(run_no1)$logSDDR),
  info = "verify_volatility: for sparse  MIX model; logSDDR is numeric"
)

