data(us_fiscal_lsuw)
data = us_fiscal_lsuw[1:40, 1:2]

specifications = list(
  BSVAR = function() specify_bsvar$new(data, distribution = "t"),
  T = function() specify_bsvar_t$new(data),
  SV = function() specify_bsvar_sv$new(data, distribution = "t"),
  MSH = function() specify_bsvar_msh$new(data, M = 2, distribution = "t"),
  MIX = function() specify_bsvar_mix$new(data, M = 2, distribution = "t"),
  HMSH = function() specify_bsvar_hmsh$new(data, M = 2, distribution = "t"),
  EXH = function() specify_bsvar_exh$new(
    data,
    distribution = "t",
    variance_regimes = rep(1:2, length.out = nrow(data))
  )
)

for (model in names(specifications)) {
  specification = suppressMessages(specifications[[model]]())
  initial = specification$starting_values$get_starting_values()

  expect_identical(
    initial$adaptation_iteration,
    0L,
    info = paste(model, "initialises the Student-t adaptation iteration at zero.")
  )
  expect_equal(
    length(initial$adaptive_scale),
    ncol(data),
    info = paste(model, "provides one Student-t adaptive scale per equation.")
  )

  set.seed(800 + match(model, names(specifications)))
  first_chunk = estimate(specification, S = 3, show_progress = FALSE)
  first_state = first_chunk$last_draw$starting_values$get_starting_values()

  expect_identical(
    first_state$adaptation_iteration,
    3L,
    info = paste(model, "records completed Student-t sampler iterations.")
  )

  continuation_seed = 900 + match(model, names(specifications))
  set.seed(continuation_seed)
  expected = .Call(
    "_bsvars_sample_df",
    first_state$df * 1,
    first_state$adaptive_scale * 1,
    first_state$lambda * 1,
    first_state$adaptation_iteration,
    c(0.44, 0.6),
    PACKAGE = "bsvars"
  )

  set.seed(continuation_seed)
  estimate_method = getS3method("estimate", class(first_chunk)[1])
  continued = estimate_method(first_chunk, S = 1, show_progress = FALSE)
  continued_state = continued$last_draw$starting_values$get_starting_values()

  expect_equal(
    continued_state$df,
    expected$aux_df,
    info = paste(model, "continues the Student-t degrees-of-freedom chain.")
  )
  expect_equal(
    continued_state$adaptive_scale,
    expected$adaptive_scale,
    info = paste(model, "adapts with the cumulative Student-t iteration.")
  )
  expect_identical(
    continued_state$adaptation_iteration,
    4L,
    info = paste(model, "increments the cumulative Student-t iteration.")
  )
}
