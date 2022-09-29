
data(us_fiscal_lsuw)

set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate_bsvar(3, specification_no1, 1, show_progress = FALSE)
BB                  <- run_no1$last_draw$starting_values$B
B_hat               <- diag(sign(diag(BB))) %*% BB
normalise_posterior(run_no1, B_hat)
irf                 <- compute_impulse_responses(run_no1, horizon = 2)

expect_equal(
  irf[1,1,1,1], 1,
  info = "compute_impulse_responses: unit own shock at 0 horizon."
)

expect_error(
  compute_impulse_responses(run_no1),
  info = "compute_impulse_responses: specify horizon."
)