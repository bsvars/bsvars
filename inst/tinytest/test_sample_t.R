log_kernel_df_r = function(df, lambda) {
  T = length(lambda)
  -T * lgamma(0.5 * df) +
    0.5 * T * df * log(0.5 * (df - 2)) -
    0.5 * (df + 2) * sum(log(lambda)) -
    0.5 * (df - 2) * sum(lambda^-1) -
    2 * log(df - 1)
}

df = 2.05
proposal_sd = 0.5
lambda = matrix(0.1, 1, 5)

set.seed(42)
df_star = RcppTN::rtn(df, proposal_sd, 2, Inf)
acceptance_probability = min(
  1,
  exp(log_kernel_df_r(df_star, lambda) - log_kernel_df_r(df, lambda)) *
    RcppTN::dtn(df, df_star, proposal_sd, 2, Inf) /
    RcppTN::dtn(df_star, df, proposal_sd, 2, Inf)
)
expected_df = if (runif(1) < acceptance_probability) df_star else df

set.seed(42)
draw = .Call(
  "_bsvars_sample_df",
  df,
  proposal_sd,
  lambda,
  0L,
  c(0.44, 0.6),
  PACKAGE = "bsvars"
)

expect_equal(
  as.numeric(draw$aux_df),
  expected_df,
  info = "The truncated-normal Hastings ratio uses reverse over forward proposal density."
)
