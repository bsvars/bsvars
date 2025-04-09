
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)

set.seed(1)
suppressMessages(
  specification_no2 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no2             <- estimate(specification_no2, 3, 1, show_progress = FALSE)

set.seed(1)
run_no3             <- us_fiscal_lsuw |>
  specify_bsvar_t$new() |>
  estimate(S = 3, thin = 1, show_progress = FALSE)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no2$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar_t: the last_draw(s) of two runs to be identical."
)

expect_identical(
  run_no1$posterior$B[1,1,1],
  run_no2$posterior$B[1,1,1],
  info = "estimate_bsvar_t: the first draws of two runs to be identical."
)

expect_identical(
  run_no1$last_draw$starting_values$B[1,1],
  run_no3$last_draw$starting_values$B[1,1],
  info = "estimate_bsvar_t: the last_draw(s) of a normal and pipe run to be identical."
)

expect_identical(
  run_no1$last_draw$starting_values$df,
  run_no3$last_draw$starting_values$df,
  info = "estimate_bsvar_t: the last_draw(s) of a normal and pipe run to be identical."
)


# a test of a good setting of S and thin
expect_error(
  estimate(specification_no1, 3, 2, show_progress = FALSE),
  info = "Argument S is not a positive integer multiplication of argument thin."
)

expect_error(
  estimate(specification_no1, 2, 3, show_progress = FALSE),
  info = "Argument S is not a positive integer multiplication of argument thin."
)

# checks model with restrictions on the A matrix
A       = matrix(TRUE, 3, 4)
A[1, 3] = FALSE
suppressMessages(
  specification_no4 <- specify_bsvar_t$new(us_fiscal_lsuw, A = A)
)
run_no4             <- estimate(specification_no4, 3, 1, show_progress = FALSE)
expect_true(
  all(run_no4$posterior$A[1, 3, ] == 0),
  info = "estimate_bsvar_t: the A matrix is restricted correctly."
)
