data(us_fiscal_lsuw)

forced_transition = matrix(
  c(1e-12, 1 - 1e-12, 1e-12, 1 - 1e-12),
  2,
  2,
  byrow = TRUE
)

msh = suppressMessages(specify_bsvar_msh$new(us_fiscal_lsuw, M = 2))
msh_start = msh$starting_values$get_starting_values()
msh_start$PR_TR = forced_transition
msh_start$pi_0 = c(0.5, 0.5)
msh_start$sigma2[, 1] = 1e-12
msh_start$sigma2[, 2] = 1
msh_start$xi = rbind(
  rep(1, ncol(msh_start$xi)),
  rep(0, ncol(msh_start$xi))
)
msh_data = msh$data_matrices$get_data_matrices()

set.seed(171)
msh_draw = .Call(
  "_bsvars_bsvar_msh_cpp",
  1L,
  msh_data$Y,
  msh_data$X,
  msh$prior$get_prior(),
  msh$identification$VB,
  msh$identification$VA,
  msh_start,
  TRUE,
  1L,
  FALSE,
  TRUE,
  "test",
  FALSE,
  PACKAGE = "bsvars"
)

expect_true(
  all(msh_draw$posterior$xi[2, , 1] == 1),
  info = "The MSH driver passes structural residuals, not an empty or volatility-standardised matrix, to the first state draw."
)

hmsh = suppressMessages(specify_bsvar_hmsh$new(us_fiscal_lsuw, M = 2))
hmsh_start = hmsh$starting_values$get_starting_values()
hmsh_start$PR_TR = array(0, c(2, 2, 3))
for (n in 1:3) hmsh_start$PR_TR[, , n] = forced_transition
hmsh_start$pi_0 = matrix(0.5, 2, 3)
hmsh_start$sigma2[, 1] = 1e-12
hmsh_start$sigma2[, 2] = 1
hmsh_start$xi = array(0, dim(hmsh_start$xi))
hmsh_start$xi[1, , ] = 1
hmsh_data = hmsh$data_matrices$get_data_matrices()

set.seed(172)
hmsh_draw = .Call(
  "_bsvars_bsvar_hmsh_cpp",
  1L,
  hmsh_data$Y,
  hmsh_data$X,
  hmsh$prior$get_prior(),
  hmsh$identification$VB,
  hmsh$identification$VA,
  hmsh_start,
  TRUE,
  1L,
  FALSE,
  TRUE,
  "test",
  FALSE,
  PACKAGE = "bsvars"
)

expect_true(
  all(hmsh_draw$posterior$xi_cpp[[1]][2, , ] == 1),
  info = "The HMSH driver passes structural residuals, not an empty or volatility-standardised matrix, to the first state draw."
)
