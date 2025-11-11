# Verifies heteroskedasticity of structural shocks equation by equation

This function will be deprecated starting from version 4.0. It is
replaced by
[`verify_identification`](https://bsvars.org/bsvars/reference/verify_identification.md)
function.

Computes the logarithm of Bayes factor for the homoskedasticity
hypothesis for each of the structural shocks via Savage-Dickey Density
Ration (SDDR). The hypothesis of homoskedasticity, \\H_0\\, is
represented by model-specific restrictions. Consult help files for
individual classes of models for details. The logarithm of Bayes factor
for this hypothesis can be computed using the SDDR as the difference of
logarithms of the marginal posterior distribution ordinate at the
restriction less the marginal prior distribution ordinate at the same
point: \$\$log p(H_0 \| data) - log p(H_0)\$\$ Therefore, a negative
value of the difference is the evidence against homoskedasticity of the
structural shock. The estimation of both elements of the difference
requires numerical integration.

## Usage

``` r
verify_volatility(posterior)
```

## Arguments

- posterior:

  the `posterior` element of the list from the estimation outcome

## Value

An object of class `SDDRvolatility` that is a list of three components:

`logSDDR` an `N`-vector with values of the logarithm of the Bayes
factors for the homoskedasticity hypothesis for each of the shocks

`log_SDDR_se` an `N`-vector with estimation standard errors of the
logarithm of the Bayes factors reported in output element `logSDDR` that
are computed based on 30 random sub-samples of the log-ordinates of the
marginal posterior and prior distributions.

`components` a list of three components for the computation of the Bayes
factor

- log_denominator:

  an `N`-vector with values of the logarithm of the Bayes factor
  denominators

- log_numerator:

  an `N`-vector with values of the logarithm of the Bayes factor
  numerators

- log_numerator_s:

  an `NxS` matrix of the log-full conditional posterior density
  ordinates computed to estimate the numerator

- se_components:

  an `Nx30` matrix containing the log-Bayes factors on the basis of
  which the standard errors are computed

## References

Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural
Vector Autoregressions Identified by Markov-Switching
Heteroskedasticity. *Journal of Economic Dynamics and Control* **113**,
103862,
[doi:10.1016/j.jedc.2020.103862](https://doi.org/10.1016/j.jedc.2020.103862)
.

Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial
Identification of Heteroskedastic Structural VARs: Theory and Bayesian
Inference. *University of Melbourne Working Paper*, 1–57,
[doi:10.48550/arXiv.2404.11057](https://doi.org/10.48550/arXiv.2404.11057)
.

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
# specify the model 
specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#> The identification is set to the default option of lower-triangular structural matrix.

# estimate the model
posterior      = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# verify heteroskedasticity
sddr           = verify_volatility(posterior)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 1) |>
  estimate(S = 10) |> 
  verify_volatility() -> sddr
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
```
