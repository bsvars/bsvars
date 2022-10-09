
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
fv                  <- compute_fitted_values(run_no1)

set.seed(1)
suppressMessages(
  fv2               <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_fitted_values()
)



expect_true(
  all(dim(fv) == dim(fv2)),
  info = "compute_fitted_values: same output dimentions for normal and pipe workflow."
)

expect_identical(
  fv[1,1,1], fv2[1,1,1],
  info = "compute_fitted_values: identical for normal and pipe workflow."
)
