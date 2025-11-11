
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no2             <- estimate(specification_no2, 3, 1, show_progress = FALSE)

set.seed(1)
run_no3             <- us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 3, thin = 1, show_progress = FALSE)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no2$last_draw$starting_values$B[1,1],
  info = "normalise bsvar: the last_draw(s) of two runs to be identical."
)

expect_identical(
  run_no1$posterior$B[1,1,1],
  run_no2$posterior$B[1,1,1],
  info = "estimate_bsvar: the first draws of two runs to be identical."
)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no3$last_draw$starting_values$B[1,1],
  info = "normalise bsvar: the last_draw(s) of two runs to be identical."
)

BB = matrix(1, 4, 3)
expect_error(
  normalise(run_no1, BB),
  info = "normalise bsvar: Not square B_benchmark."
)

BB = matrix(1, 3, 3)
BB[1,1] = NA
expect_error(
  normalise(run_no1, BB),
  info = "normalise bsvar: B_benchmark with missing values."
)

# checks model with restrictions on the A matrix
expect_true(
  run_no1$is_normalised(),
  info = "normalise bsvar: posterior output is not normalised by default."
)

# checks model with restrictions on the A matrix
expect_true(
  run_no1$last_draw$starting_values$B[1,1] > 0,
  info = "normalise bsvar: the last_draw(s) of two runs to be identical."
)
