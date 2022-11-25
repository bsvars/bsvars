
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no2             <- estimate(specification_no2, 3, 1, show_progress = FALSE)

set.seed(1)
run_no3             <- us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 3, thin = 1, show_progress = FALSE)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no2$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar_sv: the last_draw(s) of two runs to be identical."
)

expect_identical(
  run_no1$posterior$B[1,1,1],
  run_no2$posterior$B[1,1,1],
  info = "estimate_bsvar_sv: the first draws of two runs to be identical."
)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no3$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar_sv: the last_draw(s) of a normal and pipe run to be identical."
)


set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw, centred_sv = TRUE)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar_sv$new(us_fiscal_lsuw, centred_sv = TRUE)
)
run_no2             <- estimate(specification_no2, 3, 1, show_progress = FALSE)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no2$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar_sv centred: the last_draw(s) of two runs to be identical."
)
