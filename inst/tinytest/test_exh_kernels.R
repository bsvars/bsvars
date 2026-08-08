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
