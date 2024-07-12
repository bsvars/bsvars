
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_message(
  verify_identification(run_no1),
  pattern = "*homoskedastic.",
  info = "verify_identification: the model to be homoskedastic"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw, centred_sv = TRUE)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_message(
  verify_identification(run_no1),
  pattern = "*known*",
  info = "verify_identification: for centred SV"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_identification(run_no1)$logSDDR),
  info = "verify_identification: for non-centred SV; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_identification(run_no1)$logSDDR),
  info = "verify_identification: for MSH model; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_identification(run_no1)$logSDDR),
  info = "verify_identification: for MIX model; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, finiteM = FALSE)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_identification(run_no1)$logSDDR),
  info = "verify_identification: for sparse MSH model; logSDDR is numeric"
)

#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, finiteM = FALSE)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_identification(run_no1)$logSDDR),
  info = "verify_identification: for sparse  MIX model; logSDDR is numeric"
)


#############################################
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 60, 1, show_progress = FALSE)

expect_true(
  is.numeric(verify_identification(run_no1)$SDDR),
  info = "verify_identification: for T model; SDDR is numeric"
)

