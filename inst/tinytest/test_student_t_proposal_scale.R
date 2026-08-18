data(us_fiscal_lsuw)
data = us_fiscal_lsuw[1:40, 1:2]
T = nrow(data) - 1
df_reference = 30
negative_hessian =
  0.25 * T * psigamma(0.5 * df_reference, deriv = 1) -
  0.5 * T * (df_reference - 4) * (df_reference - 2)^-2 -
  2 * (df_reference - 1)^-2
proposal_sd = 1 / sqrt(negative_hessian)

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
    rep(proposal_sd, length(starting_values$df)),
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
    info = paste(model, "uses the correctly derived Hessian-based initial proposal standard deviation.")
  )
}
