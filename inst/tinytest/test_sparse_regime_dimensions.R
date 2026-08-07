data(us_fiscal_lsuw)

constructors = list(
  MSH = specify_bsvar_msh,
  HMSH = specify_bsvar_hmsh,
  MIX = specify_bsvar_mix
)

for (model in names(constructors)) {
  specification = suppressMessages(
    constructors[[model]]$new(us_fiscal_lsuw, M = 25, finiteM = FALSE)
  )
  prior = specification$prior$get_prior()
  starting_values = specification$starting_values$get_starting_values()

  expect_equal(
    nrow(prior$PR_TR),
    25,
    info = paste(model, "sparse prior retains the requested effective regime count.")
  )
  expect_equal(
    nrow(starting_values$PR_TR),
    25,
    info = paste(model, "sparse starting values use the same regime count as the prior.")
  )
  expect_equal(
    nrow(starting_values$xi),
    25,
    info = paste(model, "sparse state indicators use the same regime count as the prior.")
  )
}
