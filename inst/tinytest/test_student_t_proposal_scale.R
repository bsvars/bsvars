data(us_fiscal_lsuw)
data = us_fiscal_lsuw[1:40, 1:2]
T = nrow(data) - 1
proposal_sd = sqrt(abs(
  (0.25 * T * psigamma(15, deriv = 1) - T * 29 * 28^-2 - 2 * 29^-2)^-1
))

specifications = list(
  BSVAR = function() specify_bsvar$new(data, distribution = "t"),
  T = function() specify_bsvar_t$new(data),
  SV = function() specify_bsvar_sv$new(data, distribution = "t"),
  MSH = function() specify_bsvar_msh$new(data, M = 2, distribution = "t"),
  HMSH = function() specify_bsvar_hmsh$new(data, M = 2, distribution = "t"),
  EXH = function() specify_bsvar_exh$new(
    data,
    distribution = "t",
    variance_regimes = rep(1:2, length.out = nrow(data))
  )
)

for (model in names(specifications)) {
  specification = suppressMessages(specifications[[model]]())
  starting_values = specification$starting_values$get_starting_values()

  set.seed(373)
  expected_df = .Call(
    "_bsvars_sample_df",
    starting_values$df * 1,
    rep(proposal_sd, nrow(data)),
    starting_values$lambda * 1,
    0L,
    c(0.44, 0.6),
    PACKAGE = "bsvars"
  )$aux_df

  set.seed(373)
  posterior = estimate(specification, S = 2, show_progress = FALSE)

  expect_equal(
    as.numeric(posterior$posterior$df[, 1]),
    as.numeric(expected_df),
    info = paste(model, "initialises the Student-t proposal with a standard deviation, not a variance.")
  )
}
