
data(us_fiscal_lsuw)

# for homoskedastic
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
  run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
suppressMessages(  
    sd                  <- compute_conditional_sd(run_no1)
)

set.seed(1)
suppressMessages(
  sd2               <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_conditional_sd()
)

expect_true(
  all(dim(sd) == dim(sd2)),
  info = "compute_conditional_sd: same output dimentions for normal and pipe workflow."
)

expect_true(
  all(sd > 0 ),
  info = "compute_conditional_sd: only positive sds."
)

expect_identical(
  sd, sd2,
  info = "compute_conditional_sd: identical for normal and pipe workflow."
)

expect_message(
  compute_conditional_sd(run_no1),
  pattern = "homoskedastic",
  info = "compute_conditional_sd: message for homoskedastic model."
)

# for heteroskedastic
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
  run_no1           <- estimate(specification_no1, 3, 1, show_progress = FALSE)
suppressMessages(
  sd                <- compute_conditional_sd(run_no1)
)

set.seed(1)
suppressMessages(
  sd2               <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_conditional_sd()
)

expect_true(
  all(dim(sd) == dim(sd2)),
  info = "compute_conditional_sd: heteroskedastic: same output dimentions for normal and pipe workflow."
)

expect_true(
  all(sd > 0 ),
  info = "compute_conditional_sd: heteroskedastic: only positive sds."
)

expect_identical(
  sd, sd2,
  info = "compute_conditional_sd: heteroskedastic: identical for normal and pipe workflow."
)





# for t
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no1           <- estimate(specification_no1, 3, 1, show_progress = FALSE)
suppressMessages(
  sd                <- compute_conditional_sd(run_no1)
)

set.seed(1)
suppressMessages(
  sd2               <- us_fiscal_lsuw |>
    specify_bsvar_t$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_conditional_sd()
)

expect_true(
  all(dim(sd) == dim(sd2)),
  info = "compute_conditional_sd: t: same output dimentions for normal and pipe workflow."
)

expect_true(
  all(sd > 0 ),
  info = "compute_conditional_sd: t: only positive sds."
)

expect_identical(
  sd, sd2,
  info = "compute_conditional_sd: t: identical for normal and pipe workflow."
)
