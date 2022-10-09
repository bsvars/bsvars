
data(us_fiscal_lsuw)

# for bsvar_msh
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
rp                  <- compute_regime_probabilities(run_no1)

set.seed(1)
suppressMessages(
  rp2               <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities()
)

expect_true(
  all(dim(rp) == dim(rp2)),
  info = "compute_regime_probabilities: MSH: same output dimentions for normal and pipe workflow."
)

expect_identical(
  rp[1,1,1], rp2[1,1,1],
  info = "compute_regime_probabilities: MSH: identical for normal and pipe workflow."
)


# for bsvar_mix
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, M=2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
rp                  <- compute_regime_probabilities(run_no1)

set.seed(1)
suppressMessages(
  rp2               <- us_fiscal_lsuw |>
    specify_bsvar_mix$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities()
)

expect_true(
  all(dim(rp) == dim(rp2)),
  info = "compute_regime_probabilities: MIX: same output dimentions for normal and pipe workflow."
)

expect_identical(
  rp[1,1,1], rp2[1,1,1],
  info = "compute_regime_probabilities: MIX: identical for normal and pipe workflow."
)


# for filtered
set.seed(1)
suppressMessages(
  rp                <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities(type = "filtered")
)

expect_true(
  all(rp >= 0 & rp <= 1),
  info = "compute_regime_probabilities: filtered: all within [0,1]."
)


# for forecasted
set.seed(1)
suppressMessages(
  rp                <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities(type = "forecasted")
)

expect_true(
  all(rp >= 0 & rp <= 1),
  info = "compute_regime_probabilities: forecasted: all within [0,1]."
)


# for smoothed
set.seed(1)
suppressMessages(
  rp                <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities(type = "smoothed")
)

expect_true(
  all(rp >= 0 & rp <= 1),
  info = "compute_regime_probabilities: smoothed: all within [0,1]."
)



# for bsvar
set.seed(1)
suppressMessages(
  rp2               <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE)
)

expect_error(
  compute_regime_probabilities(rp2),
  info = "compute_regime_probabilities: BSVAR: wrong posterior provided."
)

