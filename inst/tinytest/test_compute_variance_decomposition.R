
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
fevd                <- compute_variance_decompositions(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  fevd2               <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_variance_decompositions(horizon = 2)
)

expect_error(
  compute_variance_decompositions(run_no1),
  info = "compute_variance_decompositions: specify horizon."
)

expect_equal(
  sum(fevd[1,,1,1]), 100,
  info = "compute_variance_decompositions: sum to 100%."
)

expect_identical(
  fevd[3,3,3,3], fevd2[3,3,3,3],
  info = "compute_variance_decompositions: identical for normal and pipe workflow."
)





set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
fevd                <- compute_variance_decompositions(run_no1, horizon = 2)

suppressMessages({
  set.seed(1)
  fevd2               <- us_fiscal_lsuw |>
    specify_bsvar_mix$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_variance_decompositions(horizon = 2)
})

expect_error(
  compute_variance_decompositions(run_no1),
  info = "compute_variance_decompositions in an msh model: specify horizon."
)

expect_equal(
  sum(fevd[1,,1,1]), 100,
  info = "compute_variance_decompositions in an msh model: sum to 100%."
)





set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
fevd                <- compute_variance_decompositions(run_no1, horizon = 2)

suppressMessages({
  set.seed(1)
  fevd2               <- us_fiscal_lsuw |>
    specify_bsvar_msh$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_variance_decompositions(horizon = 2)
})

expect_error(
  compute_variance_decompositions(run_no1),
  info = "compute_variance_decompositions in an msh model: specify horizon."
)

expect_equal(
  sum(fevd[1,,1,1]), 100,
  info = "compute_variance_decompositions in an msh model: sum to 100%."
)






set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
fevd                <- compute_variance_decompositions(run_no1, horizon = 2)

suppressMessages({
  set.seed(1)
  fevd2               <- us_fiscal_lsuw |>
    specify_bsvar_sv$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_variance_decompositions(horizon = 2)
})

expect_error(
  compute_variance_decompositions(run_no1),
  info = "compute_variance_decompositions in a sv model: specify horizon."
)

expect_equal(
  sum(fevd[1,,1,1]), 100,
  info = "compute_variance_decompositions in a sv model: sum to 100%."
)





set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
fevd                <- compute_variance_decompositions(run_no1, horizon = 2)

suppressMessages({
  set.seed(1)
  fevd2               <- us_fiscal_lsuw |>
    specify_bsvar_t$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_variance_decompositions(horizon = 2)
})

expect_error(
  compute_variance_decompositions(run_no1),
  info = "compute_variance_decompositions in a sv model: specify horizon."
)

expect_equal(
  sum(fevd[1,,1,1]), 100,
  info = "compute_variance_decompositions in a sv model: sum to 100%."
)
