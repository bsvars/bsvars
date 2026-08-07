data(us_fiscal_lsuw)
data = us_fiscal_lsuw[1:40, 1:2]
T = nrow(data) - 1
variance_regimes = rep(1:2, length.out = nrow(data))

normal_specification = suppressMessages(
  specify_bsvar_exh$new(
    data,
    distribution = "norm",
    variance_regimes = variance_regimes
  )
)
normal_start = normal_specification$starting_values$get_starting_values()
normal_data = normal_specification$data_matrices$get_data_matrices()

set.seed(627)
normal_draw = .Call(
  "_bsvars_bsvar_exh_cpp",
  1L,
  normal_data$Y,
  normal_data$X,
  normal_specification$prior$get_prior(),
  normal_specification$identification$VB,
  normal_specification$identification$VA,
  normal_start,
  TRUE,
  1L,
  FALSE,
  PACKAGE = "bsvars"
)

expect_false(
  identical(normal_draw$posterior$sigma2[, , 1], normal_start$sigma2),
  info = "The first normal EXH sweep samples variances from structural residuals."
)

student_specification = suppressMessages(
  specify_bsvar_exh$new(
    data,
    distribution = "t",
    variance_regimes = variance_regimes
  )
)
student_start = student_specification$starting_values$get_starting_values()
student_data = student_specification$data_matrices$get_data_matrices()
student_prior = student_specification$prior$get_prior()
N = nrow(student_data$Y)
proposal_scale = abs(
  (0.25 * T * psigamma(15, deriv = 1) - T * 29 * 28^-2 - 2 * 29^-2)^-1
)

set.seed(628)
df_draw = .Call(
  "_bsvars_sample_df",
  student_start$df * 1,
  rep(proposal_scale, N),
  student_start$lambda * 1,
  0L,
  c(0.44, 0.6),
  PACKAGE = "bsvars"
)$aux_df
standardised_residuals = student_start$B %*%
  (student_data$Y - student_start$A %*% student_data$X)
lambda_draw = .Call(
  "_bsvars_sample_lambda",
  df_draw,
  standardised_residuals,
  PACKAGE = "bsvars"
)
expected_sigma2 = bsvars:::sample_variances_msh(
  student_start$sigma2 * 1,
  standardised_residuals / sqrt(lambda_draw),
  student_start$xi,
  student_prior
)

set.seed(628)
student_draw = .Call(
  "_bsvars_bsvar_exh_cpp",
  1L,
  student_data$Y,
  student_data$X,
  student_prior,
  student_specification$identification$VB,
  student_specification$identification$VA,
  student_start,
  FALSE,
  1L,
  FALSE,
  PACKAGE = "bsvars"
)

expect_equal(
  student_draw$posterior$sigma2[, , 1],
  expected_sigma2,
  info = "Student-t EXH variances use residuals scaled by the newly drawn latent scales."
)
