
data(us_fiscal_lsuw)

# for bsvar_msh
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
rp                  <- compute_regime_probabilities(run_no1)

expect_equal(
  dim(rp),
  dim(run_no1$posterior$xi),
  info = "compute_regime_probabilities: MSH: realized regimes retain compact storage."
)
expect_identical(
  attr(rp, "type"),
  "realized",
  info = "compute_regime_probabilities: MSH: output records compact realized-regime semantics."
)
expect_identical(
  names(summary(rp)$MarkovProcess1),
  "regime",
  info = "summary.PosteriorRegimePr: compact realized regimes are summarized as indices."
)

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

expect_equal(
  dim(rp),
  dim(run_no1$posterior$xi),
  info = "compute_regime_probabilities: MIX: realized regimes retain compact storage."
)

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
expect_equal(
  dim(rp)[1],
  2,
  info = "compute_regime_probabilities: filtered probabilities retain one row per regime."
)
expect_identical(
  attr(rp, "type"),
  "filtered",
  info = "compute_regime_probabilities: filtered output records probability semantics."
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




# for bsvar_hmsh
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
rp                  <- compute_regime_probabilities(run_no1)

expect_equal(
  dim(rp),
  dim(run_no1$posterior$xi),
  info = "compute_regime_probabilities: HMSH: realized regimes retain compact storage."
)

set.seed(1)
suppressMessages(
  rp2               <- us_fiscal_lsuw |>
    specify_bsvar_hmsh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities()
)

expect_true(
  all(dim(rp) == dim(rp2)),
  info = "compute_regime_probabilities: HMSH: same output dimentions for normal and pipe workflow."
)

expect_identical(
  rp[1,1,1,1], rp2[1,1,1,1],
  info = "compute_regime_probabilities: HMSH: identical for normal and pipe workflow."
)






# for bsvar_exh
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_exh$new(us_fiscal_lsuw, 
                                             variance_regimes = sample(1:2, nrow(us_fiscal_lsuw), replace = TRUE))
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
rp                  <- compute_regime_probabilities(run_no1)

expect_equal(
  dim(rp),
  dim(run_no1$posterior$xi),
  info = "compute_regime_probabilities: exH: realized regimes retain compact storage."
)

set.seed(1)
suppressMessages(
  rp2               <- us_fiscal_lsuw |>
    specify_bsvar_exh$new(variance_regimes = sample(1:2, nrow(us_fiscal_lsuw), replace = TRUE)) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    compute_regime_probabilities()
)

expect_true(
  all(dim(rp) == dim(rp2)),
  info = "compute_regime_probabilities: exH: same output dimentions for normal and pipe workflow."
)

expect_identical(
  rp[1,1,1], rp2[1,1,1],
  info = "compute_regime_probabilities: exH: identical for normal and pipe workflow."
)
