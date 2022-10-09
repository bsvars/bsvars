
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ss                  <- compute_structural_shocks(run_no1)

set.seed(1)
suppressMessages(
  ss2               <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_structural_shocks()
)



expect_equal(
  length(dim(ss)), length(dim(ss2)),
  info = "compute_structural_shocks: same output dimentions for normal and pipe workflow."
)

expect_identical(
  ss[1,1,1], ss2[1,1,1],
  info = "compute_structural_shocks: identical for normal and pipe workflow."
)
