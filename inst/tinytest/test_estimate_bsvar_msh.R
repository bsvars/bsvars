
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate_bsvar_msh(3, specification_no1, 1, show_progress = FALSE)

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no2             <- estimate_bsvar_msh(3, specification_no2, 1, show_progress = FALSE)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no2$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar_msh: the last_draw(s) of two runs to be identical."
)

expect_identical(
  run_no1$posterior$B[1,1,1],
  run_no2$posterior$B[1,1,1],
  info = "estimate_bsvar_msh: the first draws of two runs to be identical."
)
