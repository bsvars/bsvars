# Verifies normality of structural shocks equation by equation

Computes the logarithm of Bayes factor for the normality hypothesis for
each of the structural shocks via Savage-Dickey Density Ration (SDDR).
The hypothesis of normality, \\H_0\\, is represented by restriction that
the equation-specific degrees of freedom parameter is equal to infinity,
\\\nu_n\rightarrow\infty\\. The logarithm of Bayes factor for this
hypothesis can be computed using the SDDR as the difference of
logarithms of the marginal posterior distribution ordinate at the
restriction less the marginal prior distribution ordinate at the same
point: \$\$log p(H_0 \| data) - log p(H_0)\$\$ Therefore, a negative
value of the difference is the evidence against normality of the
structural shock. The estimation of th first element relies on kernel
density estimation of the marginal posterior density, whereas the second
element is equal to the log of value 1.

## Usage

``` r
verify_normality(posterior)
```

## Arguments

- posterior:

  the `posterior` element of the list from the estimation outcome

## Value

An object of class `SDDRnormality` that is a list of three components:

`logSDDR` an `N`-vector with values of the logarithm of the Bayes
factors for the normality hypothesis for each of the shocks

`log_SDDR_se` an `N`-vector with estimation standard errors of the
logarithm of the Bayes factors reported in output element `logSDDR` that
are computed based on 30 random sub-samples of the log-ordinates of the
marginal posterior and prior distributions.

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
specification  = specify_bsvar_sv$new(us_fiscal_lsuw, distribution = "t")
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
sddr           = verify_normality(posterior)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(distribution = "t") |>
  estimate(S = 10) |> 
  verify_normality() -> sddr
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
