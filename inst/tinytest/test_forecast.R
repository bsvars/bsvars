
data(us_fiscal_lsuw)

# for bsvar
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "forecast: forecast identical for normal and pipe workflow."
)

expect_true(
  is.numeric(ff$forecasts) & is.array(ff$forecasts),
  info = "forecast: returns numeric array."
)


expect_error(
  specify_bsvar$new(us_fiscal_lsuw) |> forecast(horizon = 3),
  info = "forecast: wrong input provided."
)

expect_error(
  forecast(run_no1, horizon = 1.5),
  info = "forecast: specify horizon as integer."
)




# for bsvar_msh
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "forecast: msh: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "forecast: msh: sigma forecast identical for normal and pipe workflow."
)


expect_true(
  is.numeric(ff$forecasts) & is.array(ff$forecasts),
  info = "forecast: msh: returns numeric array."
)

expect_true(
  is.numeric(ff$forecasts_sigma) & is.array(ff$forecasts_sigma),
  info = "forecast: msh: volatility: returns numeric array."
)

expect_error(
  specify_bsvar_msh$new(us_fiscal_lsuw) |> forecast(horizon = 3),
  info = "forecast: msh: wrong input provided."
)

expect_error(
  forecast(run_no1, horizon = 1.5),
  info = "forecast: msh: specify horizon as integer."
)


# for bsvar_mix
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_mix$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "forecast: mix: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "forecast: mix: sigma forecast identical for normal and pipe workflow."
)


expect_true(
  is.numeric(ff$forecasts) & is.array(ff$forecasts),
  info = "forecast: mix: returns numeric array."
)

expect_true(
  is.numeric(ff$forecasts_sigma) & is.array(ff$forecasts_sigma),
  info = "forecast: mix: volatility: returns numeric array."
)

expect_error(
  specify_bsvar_msh$new(us_fiscal_lsuw) |> forecast(horizon = 3),
  info = "forecast: mix: wrong input provided."
)

expect_error(
  forecast(run_no1, horizon = 1.5),
  info = "forecast: mix: specify horizon as integer."
)


# for bsvar_sv
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_sv$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "forecast: sv: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "forecast: sv: sigma forecast identical for normal and pipe workflow."
)


expect_true(
  is.numeric(ff$forecasts) & is.array(ff$forecasts),
  info = "forecast: sv: returns numeric array."
)

expect_true(
  is.numeric(ff$forecasts_sigma) & is.array(ff$forecasts_sigma),
  info = "forecast: sv: volatility: returns numeric array."
)

expect_error(
  specify_bsvar_msh$new(us_fiscal_lsuw) |> forecast(horizon = 3),
  info = "forecast: sv: wrong input provided."
)

expect_error(
  forecast(run_no1, horizon = 1.5),
  info = "forecast: sv: specify horizon as integer."
)


# for bsvar_sv centred
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw, centred_sv = TRUE)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_sv$new(centred_sv = TRUE) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "forecast: sv centred: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "forecast: sv centred: sigma forecast identical for normal and pipe workflow."
)






# for bsvar_t
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_t$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "forecast: t: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "forecast: t: sigma forecast identical for normal and pipe workflow."
)




# conditional forecasting
################################
cf        = matrix(NA , 2, 3)
cf[,3]    = tail(us_fiscal_lsuw, 1)[3]   # conditional forecasts equal to the last gdp observation

# for bsvar
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2, conditional_forecast = cf)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2, conditional_forecast = cf)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "conditonal forecast: forecast identical for normal and pipe workflow."
)

expect_true(
  is.numeric(ff$forecasts) & is.array(ff$forecasts),
  info = "conditonal forecast: returns numeric array."
)

expect_error(
  forecast(run_no1, horizon = 3, conditional_forecast = cf),
  info = "conditonal forecast: wrong value of horizon."
)


# for bsvar_msh
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2, conditional_forecast = cf)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_msh$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2, conditional_forecast = cf)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "conditonal forecast: msh: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "conditonal forecast: msh: sigma forecast identical for normal and pipe workflow."
)




# for bsvar_mix
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2, conditional_forecast = cf)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_mix$new(M = 2) |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2, conditional_forecast = cf)
)

expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "conditonal forecast: mix: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "conditonal forecast: mix: sigma forecast identical for normal and pipe workflow."
)

expect_true(
  is.numeric(ff$forecasts) & is.array(ff$forecasts),
  info = "conditonal forecast: mix: returns numeric array."
)







# for bsvar_sv
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_sv$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2, conditional_forecast = cf)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_sv$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2, conditional_forecast = cf)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "conditonal forecast: sv: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "conditonal forecast: sv: sigma forecast identical for normal and pipe workflow."
)




# for bsvar_t
set.seed(1)
suppressMessages(
  specification_no1 <- specify_bsvar_t$new(us_fiscal_lsuw)
)
run_no1             <- estimate(specification_no1, 3, 1, show_progress = FALSE)
ff                  <- forecast(run_no1, horizon = 2, conditional_forecast = cf)

set.seed(1)
suppressMessages(
  ff2              <- us_fiscal_lsuw |>
    specify_bsvar_t$new() |>
    estimate(S = 3, thin = 1, show_progress = FALSE) |>
    forecast(horizon = 2, conditional_forecast = cf)
)


expect_identical(
  ff$forecasts[1,1,1], ff2$forecasts[1,1,1],
  info = "conditonal forecast: t: forecast identical for normal and pipe workflow."
)

expect_identical(
  ff$forecasts_sigma[1,1,1], ff2$forecasts_sigma[1,1,1],
  info = "conditonal forecast: t: sigma forecast identical for normal and pipe workflow."
)

