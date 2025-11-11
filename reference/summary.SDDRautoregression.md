# Provides summary of verifying hypotheses about autoregressive parameters

Provides summary of the Savage-Dickey density ratios for verification of
hypotheses about autoregressive parameters.

## Usage

``` r
# S3 method for class 'SDDRautoregression'
summary(object, ...)
```

## Arguments

- object:

  an object of class `SDDRautoregression` obtained using the
  [`verify_autoregression()`](https://bsvars.org/bsvars/reference/verify_autoregression.md)
  function.

- ...:

  additional arguments affecting the summary produced.

## Value

A table reporting the logarithm of Bayes factors of the restriction
against no restriction posterior odds in `"log(SDDR)"`, its numerical
standard error `"NSE"`, and the implied posterior probability of the
restriction holding or not hypothesis, `"Pr[H0|data]"` and
`"Pr[H1|data]"` respectively.

## See also

[`verify_autoregression`](https://bsvars.org/bsvars/reference/verify_autoregression.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#> The identification is set to the default option of lower-triangular structural matrix.
set.seed(123)

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

# verify autoregression
H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
sddr           = verify_autoregression(posterior, H0)
summary(sddr)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of hypothesis verification              |
#>       for autoregressive parameters                |
#>  **************************************************|
#>  log(SDDR) NSE Pr[H0|data] Pr[H1|data]
#>  -1.174152   0   0.2361054   0.7638946

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 1) |>
  estimate(S = 10) |> 
  verify_autoregression(hypothesis = H0) |> 
  summary() -> sddr_summary
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
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of hypothesis verification              |
#>       for autoregressive parameters                |
#>  **************************************************|
```
