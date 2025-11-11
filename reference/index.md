# Package index

## bsvars: Bayesian Estimation of Structural Vector Autoregressive Models

Browse package information

- [`bsvars-package`](https://bsvars.org/bsvars/reference/bsvars-package.md)
  [`bsvars`](https://bsvars.org/bsvars/reference/bsvars-package.md) :
  Bayesian Estimation of Structural Vector Autoregressive Models

## Data

Upload sample data set

- [`us_fiscal_lsuw`](https://bsvars.org/bsvars/reference/us_fiscal_lsuw.md)
  : A 3-variable US fiscal system for the period 1948 Q1 – 2024 Q2
- [`us_fiscal_ex`](https://bsvars.org/bsvars/reference/us_fiscal_ex.md)
  : A 3-variable system of exogenous variables for the US fiscal model
  for the period 1948 Q1 – 2024 Q2

## Model specification

Choose a model to work with

- [`specify_bsvar`](https://bsvars.org/bsvars/reference/specify_bsvar.md)
  : R6 Class representing the specification of the homoskedastic BSVAR
  model
- [`specify_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_bsvar_exh.md)
  : R6 Class representing the specification of the BSVAREXH model with
  exogenous heteroskedastic regime change.
- [`specify_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_bsvar_hmsh.md)
  : R6 Class representing the specification of the BSVARHMSH model with
  Heterogeneous Markov Switching Heteroskedasticity.
- [`specify_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_bsvar_mix.md)
  : R6 Class representing the specification of the BSVAR model with a
  zero-mean mixture of normals model for structural shocks.
- [`specify_bsvar_msh`](https://bsvars.org/bsvars/reference/specify_bsvar_msh.md)
  : R6 Class representing the specification of the BSVAR model with
  Markov Switching Heteroskedasticity.
- [`specify_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_bsvar_sv.md)
  : R6 Class representing the specification of the BSVAR model with
  Stochastic Volatility heteroskedasticity.
- [`specify_bsvar_t`](https://bsvars.org/bsvars/reference/specify_bsvar_t.md)
  : R6 Class representing the specification of the BSVAR model with
  t-distributed structural shocks.

## More detailed model specification

Adjust or inspect the specified model

- [`specify_data_matrices`](https://bsvars.org/bsvars/reference/specify_data_matrices.md)
  : R6 Class Representing DataMatricesBSVAR
- [`specify_identification_bsvars`](https://bsvars.org/bsvars/reference/specify_identification_bsvars.md)
  : R6 Class Representing IdentificationBSVARs
- [`specify_prior_bsvar`](https://bsvars.org/bsvars/reference/specify_prior_bsvar.md)
  : R6 Class Representing PriorBSVAR
- [`specify_prior_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_prior_bsvar_exh.md)
  : R6 Class Representing PriorBSVAREXH
- [`specify_prior_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_prior_bsvar_mix.md)
  : R6 Class Representing PriorBSVARMIX
- [`specify_prior_bsvar_msh`](https://bsvars.org/bsvars/reference/specify_prior_bsvar_msh.md)
  : R6 Class Representing PriorBSVARMSH
- [`specify_prior_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_prior_bsvar_sv.md)
  : R6 Class Representing PriorBSVARSV
- [`specify_prior_bsvar_t`](https://bsvars.org/bsvars/reference/specify_prior_bsvar_t.md)
  : R6 Class Representing PriorBSVART
- [`specify_starting_values_bsvar`](https://bsvars.org/bsvars/reference/specify_starting_values_bsvar.md)
  : R6 Class Representing StartingValuesBSVAR
- [`specify_starting_values_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_starting_values_bsvar_exh.md)
  : R6 Class Representing StartingValuesBSVAREXH
- [`specify_starting_values_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_starting_values_bsvar_hmsh.md)
  : R6 Class Representing StartingValuesBSVAHMSH
- [`specify_starting_values_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_starting_values_bsvar_mix.md)
  : R6 Class Representing StartingValuesBSVARMIX
- [`specify_starting_values_bsvar_msh`](https://bsvars.org/bsvars/reference/specify_starting_values_bsvar_msh.md)
  : R6 Class Representing StartingValuesBSVARMSH
- [`specify_starting_values_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_starting_values_bsvar_sv.md)
  : R6 Class Representing StartingValuesBSVARSV

## Estimation

Run Bayesian estimation of your model and inspect the outputs

- [`estimate(`*`<BSVAR>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVAR.md)
  : Bayesian estimation of a homoskedastic Structural Vector
  Autoregression via Gibbs sampler
- [`estimate(`*`<BSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVAREXH.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  exogenous heteroskedastic regime changes via Gibbs sampler
- [`estimate(`*`<BSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVARHMSH.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  Heterogeneous Markov-switching heteroskedasticity via Gibbs sampler
- [`estimate(`*`<BSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVARMIX.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  shocks following a finite mixture of normal components via Gibbs
  sampler
- [`estimate(`*`<BSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVARMSH.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  Markov-switching heteroskedasticity via Gibbs sampler
- [`estimate(`*`<BSVARSV>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVARSV.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  Stochastic Volatility heteroskedasticity via Gibbs sampler
- [`estimate(`*`<BSVART>`*`)`](https://bsvars.org/bsvars/reference/estimate.BSVART.md)
  : Bayesian estimation of a homoskedastic Structural Vector
  Autoregression with t-distributed structural shocks via Gibbs sampler
- [`estimate(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVAR.md)
  : Bayesian estimation of a homoskedastic Structural Vector
  Autoregression via Gibbs sampler
- [`estimate(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVAREXH.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  exogenous heteroskedastic regime changes via Gibbs sampler
- [`estimate(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVARHMSH.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  Heterogeneous Markov-switching heteroskedasticity via Gibbs sampler
- [`estimate(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVARMIX.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  shocks following a finite mixture of normal components via Gibbs
  sampler
- [`estimate(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVARMSH.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  Markov-switching heteroskedasticity via Gibbs sampler
- [`estimate(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVARSV.md)
  : Bayesian estimation of a Structural Vector Autoregression with
  Stochastic Volatility heteroskedasticity via Gibbs sampler
- [`estimate(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/estimate.PosteriorBSVART.md)
  : Bayesian estimation of a homoskedastic Structural Vector
  Autoregression with t-distributed structural shocks via Gibbs sampler
- [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md) :
  Bayesian estimation of Structural Vector Autoregressions via Gibbs
  sampler
- [`normalise(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVAR.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVAREXH.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVARHMSH.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVARMIX.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVARMSH.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVARSV.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/normalise.PosteriorBSVART.md)
  : Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`normalise()`](https://bsvars.org/bsvars/reference/normalise.md) :
  Waggoner & Zha (2003) row signs normalisation of the posterior draws
  for the structural matrix \\B\\
- [`specify_posterior_bsvar`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar.md)
  : R6 Class Representing PosteriorBSVAR
- [`specify_posterior_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_exh.md)
  : R6 Class Representing PosteriorBSVAREXH
- [`specify_posterior_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_hmsh.md)
  : R6 Class Representing PosteriorBSVARHMSH
- [`specify_posterior_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_mix.md)
  : R6 Class Representing PosteriorBSVARMIX
- [`specify_posterior_bsvar_msh`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_msh.md)
  : R6 Class Representing PosteriorBSVARMSH
- [`specify_posterior_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_sv.md)
  : R6 Class Representing PosteriorBSVARSV
- [`specify_posterior_bsvar_t`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_t.md)
  : R6 Class Representing PosteriorBSVART

## Posterior summaries

Analyse the posterior summaries of the posterior estimation outcomes

- [`summary(`*`<Forecasts>`*`)`](https://bsvars.org/bsvars/reference/summary.Forecasts.md)
  : Provides posterior summary of Forecasts
- [`summary(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVAR.md)
  : Provides posterior summary of homoskedastic Structural VAR
  estimation
- [`summary(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVAREXH.md)
  : Provides posterior summary of heteroskedastic Structural VAR
  estimation
- [`summary(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVARHMSH.md)
  : Provides posterior summary of heteroskedastic Structural VAR
  estimation
- [`summary(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVARMIX.md)
  : Provides posterior summary of non-normal Structural VAR estimation
- [`summary(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVARMSH.md)
  : Provides posterior summary of heteroskedastic Structural VAR
  estimation
- [`summary(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVARSV.md)
  : Provides posterior summary of heteroskedastic Structural VAR
  estimation
- [`summary(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorBSVART.md)
  : Provides posterior summary of Structural VAR with t-distributed
  shocks estimation
- [`summary(`*`<PosteriorFEVD>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorFEVD.md)
  : Provides posterior summary of forecast error variance decompositions
- [`summary(`*`<PosteriorFitted>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorFitted.md)
  : Provides posterior summary of variables' fitted values
- [`summary(`*`<PosteriorHD>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorHD.md)
  : Provides posterior summary of historical decompositions
- [`summary(`*`<PosteriorIR>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorIR.md)
  : Provides posterior summary of impulse responses
- [`summary(`*`<PosteriorRegimePr>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorRegimePr.md)
  : Provides posterior summary of regime probabilities
- [`summary(`*`<PosteriorShocks>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorShocks.md)
  : Provides posterior summary of structural shocks
- [`summary(`*`<PosteriorSigma>`*`)`](https://bsvars.org/bsvars/reference/summary.PosteriorSigma.md)
  : Provides posterior summary of structural shocks' conditional
  standard deviations
- [`summary(`*`<SDDRautoregression>`*`)`](https://bsvars.org/bsvars/reference/summary.SDDRautoregression.md)
  : Provides summary of verifying hypotheses about autoregressive
  parameters
- [`summary(`*`<SDDRidMIX>`*`)`](https://bsvars.org/bsvars/reference/summary.SDDRidMIX.md)
  : Provides summary of verifying shocks' normality
- [`summary(`*`<SDDRidMSH>`*`)`](https://bsvars.org/bsvars/reference/summary.SDDRidMSH.md)
  : Provides summary of verifying homoskedasticity
- [`summary(`*`<SDDRidSV>`*`)`](https://bsvars.org/bsvars/reference/summary.SDDRidSV.md)
  : Provides summary of verifying homoskedasticity
- [`summary(`*`<SDDRidT>`*`)`](https://bsvars.org/bsvars/reference/summary.SDDRidT.md)
  : Provides summary of verifying shocks' normality
- [`summary(`*`<SDDRvolatility>`*`)`](https://bsvars.org/bsvars/reference/summary.SDDRvolatility.md)
  : Provides summary of verifying homoskedasticity

## Forecasting

Predict future values of your variables

- [`forecast(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVAR.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`forecast(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVAREXH.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`forecast(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVARHMSH.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`forecast(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVARMIX.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`forecast(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVARMSH.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`forecast(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVARSV.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`forecast(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/forecast.PosteriorBSVART.md)
  : Forecasting using Bayesian Structural Vector Autoregression

- [`us_fiscal_cond_forecasts`](https://bsvars.org/bsvars/reference/us_fiscal_cond_forecasts.md)
  :

  A matrix to be used in a conditional forecasting example including the
  projected values of total tax revenue that are projected to increase
  at an average quarterly sample growth rate. The other two columns are
  filled with `NA` values, which implies that the future values of the
  corresponding endogenous variables, namely government spending and
  GDP, will be forecasted given the provided projected values of total
  tax revenue. The matrix includes future values for the forecast
  horizon of two years for the US fiscal model for the period 2024 Q3 –
  2026 Q2.

- [`us_fiscal_ex_forecasts`](https://bsvars.org/bsvars/reference/us_fiscal_ex_forecasts.md)
  : A 3-variable system of exogenous variables' future values for the
  forecast horizon of two years for the US fiscal model for the period
  2024 Q3 – 2026 Q2

## Structural analyses

Compute interpretable outcomes

- [`compute_conditional_sd(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVAR.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVAREXH.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVARHMSH.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVARMIX.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVARMSH.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVARSV.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/compute_conditional_sd.PosteriorBSVART.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_conditional_sd()`](https://bsvars.org/bsvars/reference/compute_conditional_sd.md)
  : Computes posterior draws of structural shock conditional standard
  deviations
- [`compute_fitted_values(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVAR.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVAREXH.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVARHMSH.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVARMIX.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVARMSH.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVARSV.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/compute_fitted_values.PosteriorBSVART.md)
  : Computes posterior draws from data predictive density
- [`compute_fitted_values()`](https://bsvars.org/bsvars/reference/compute_fitted_values.md)
  : Computes posterior draws from data predictive density
- [`compute_historical_decompositions(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVAR.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVAREXH.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVARHMSH.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVARMIX.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVARMSH.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVARSV.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.PosteriorBSVART.md)
  : Computes posterior draws of historical decompositions
- [`compute_historical_decompositions()`](https://bsvars.org/bsvars/reference/compute_historical_decompositions.md)
  : Computes posterior draws of historical decompositions
- [`compute_impulse_responses(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVAR.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVAREXH.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVARHMSH.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVARMIX.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVARMSH.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVARSV.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/compute_impulse_responses.PosteriorBSVART.md)
  : Computes posterior draws of impulse responses
- [`compute_impulse_responses()`](https://bsvars.org/bsvars/reference/compute_impulse_responses.md)
  : Computes posterior draws of impulse responses
- [`compute_regime_probabilities(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.PosteriorBSVAREXH.md)
  : Computes posterior draws of regime probabilities
- [`compute_regime_probabilities(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.PosteriorBSVARHMSH.md)
  : Computes posterior draws of regime probabilities
- [`compute_regime_probabilities(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.PosteriorBSVARMIX.md)
  : Computes posterior draws of regime probabilities
- [`compute_regime_probabilities(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.PosteriorBSVARMSH.md)
  : Computes posterior draws of regime probabilities
- [`compute_regime_probabilities()`](https://bsvars.org/bsvars/reference/compute_regime_probabilities.md)
  : Computes posterior draws of regime probabilities
- [`compute_structural_shocks(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVAR.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVAREXH.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVARHMSH.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVARMIX.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVARMSH.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVARSV.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/compute_structural_shocks.PosteriorBSVART.md)
  : Computes posterior draws of structural shocks
- [`compute_structural_shocks()`](https://bsvars.org/bsvars/reference/compute_structural_shocks.md)
  : Computes posterior draws of structural shocks
- [`compute_variance_decompositions(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVAR.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVAREXH.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVARHMSH.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVARMIX.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVARMSH.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVARSV.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.PosteriorBSVART.md)
  : Computes posterior draws of the forecast error variance
  decomposition
- [`compute_variance_decompositions()`](https://bsvars.org/bsvars/reference/compute_variance_decompositions.md)
  : Computes posterior draws of the forecast error variance
  decomposition

## Model diagnostics

Verify heteroskedasticity and autoregressive parameters (in preparation:
structural identification)

- [`verify_autoregression(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVAR.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVAREXH.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVARHMSH.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVARMIX.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVARMSH.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVARSV.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/verify_autoregression.PosteriorBSVART.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_autoregression()`](https://bsvars.org/bsvars/reference/verify_autoregression.md)
  : Verifies hypotheses involving autoregressive parameters
- [`verify_identification(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVAR.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVAREXH.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARHMSH.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARMIX.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARMSH.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARSV.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification(`*`<PosteriorBSVART>`*`)`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVART.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_identification()`](https://bsvars.org/bsvars/reference/verify_identification.md)
  : Verifies identification through heteroskedasticity or non-normality
  of of structural shocks
- [`verify_normality(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/verify_normality.PosteriorBSVAREXH.md)
  : Verifies normality of structural shocks equation by equation
- [`verify_normality(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_normality.PosteriorBSVARHMSH.md)
  : Verifies normality of structural shocks equation by equation
- [`verify_normality(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/verify_normality.PosteriorBSVARMIX.md)
  : Verifies normality of structural shocks equation by equation
- [`verify_normality(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_normality.PosteriorBSVARMSH.md)
  : Verifies normality of structural shocks equation by equation
- [`verify_normality(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/verify_normality.PosteriorBSVARSV.md)
  : Verifies normality of structural shocks equation by equation
- [`verify_normality()`](https://bsvars.org/bsvars/reference/verify_normality.md)
  : Verifies normality of structural shocks equation by equation
- [`verify_volatility(`*`<PosteriorBSVAR>`*`)`](https://bsvars.org/bsvars/reference/verify_volatility.PosteriorBSVAR.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation
- [`verify_volatility(`*`<PosteriorBSVAREXH>`*`)`](https://bsvars.org/bsvars/reference/verify_volatility.PosteriorBSVAREXH.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation
- [`verify_volatility(`*`<PosteriorBSVARHMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_volatility.PosteriorBSVARHMSH.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation
- [`verify_volatility(`*`<PosteriorBSVARMIX>`*`)`](https://bsvars.org/bsvars/reference/verify_volatility.PosteriorBSVARMIX.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation
- [`verify_volatility(`*`<PosteriorBSVARMSH>`*`)`](https://bsvars.org/bsvars/reference/verify_volatility.PosteriorBSVARMSH.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation
- [`verify_volatility(`*`<PosteriorBSVARSV>`*`)`](https://bsvars.org/bsvars/reference/verify_volatility.PosteriorBSVARSV.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation
- [`verify_volatility()`](https://bsvars.org/bsvars/reference/verify_volatility.md)
  : Verifies heteroskedasticity of structural shocks equation by
  equation

## Plot your results

Prepare beautiful and informative plots for your analyses

- [`plot(`*`<Forecasts>`*`)`](https://bsvars.org/bsvars/reference/plot.Forecasts.md)
  : Plots fitted values of dependent variables

- [`plot(`*`<PosteriorFEVD>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorFEVD.md)
  : Plots forecast error variance decompositions

- [`plot(`*`<PosteriorFitted>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorFitted.md)
  : Plots fitted values of dependent variables

- [`plot(`*`<PosteriorHD>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorHD.md)
  : Plots historical decompositions

- [`plot(`*`<PosteriorIR>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorIR.md)
  : Plots impulse responses

- [`plot(`*`<PosteriorRegimePr>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorRegimePr.md)
  : Plots estimated regime probabilities

- [`plot(`*`<PosteriorShocks>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorShocks.md)
  : Plots structural shocks

- [`plot(`*`<PosteriorSigma>`*`)`](https://bsvars.org/bsvars/reference/plot.PosteriorSigma.md)
  : Plots structural shocks' conditional standard deviations

- [`plot_ribbon()`](https://bsvars.org/bsvars/reference/plot_ribbon.md)
  :

  Plots the median and an interval between two specified percentiles for
  a sequence of `K` random variables
