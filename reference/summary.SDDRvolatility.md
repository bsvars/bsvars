# Provides summary of verifying homoskedasticity

Provides summary of the Savage-Dickey density ratios for verification of
structural shocks homoskedasticity.

## Usage

``` r
# S3 method for class 'SDDRvolatility'
summary(object, ...)
```

## Arguments

- object:

  an object of class `SDDRvolatility` obtained using the
  [`verify_volatility()`](https://bsvars.org/bsvars/reference/verify_volatility.md)
  function.

- ...:

  additional arguments affecting the summary produced.

## Value

A table reporting the logarithm of Bayes factors of homoskedastic to
heteroskedastic posterior odds `"log(SDDR)"` for each structural shock,
their numerical standard errors `"NSE"`, and the implied posterior
probability of the homoskedasticity and heteroskedasticity hypothesis,
`"Pr[homoskedasticity|data]"` and `"Pr[heteroskedasticity|data]"`
respectively.

## See also

[`verify_volatility`](https://bsvars.org/bsvars/reference/verify_volatility.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.
set.seed(123)

# estimate the model
posterior      = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# verify heteroskedasticity
sddr           = verify_volatility(posterior)
summary(sddr)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of structural shocks                    |
#>       homoskedasticity verification                |
#>  **************************************************|
#>          log(SDDR) NSE Pr[homoskedasticity|data] Pr[heteroskedasticity|data]
#> shock 1  2.0562402   0               0.886576637                   0.1134234
#> shock 2 -0.2450886   0               0.439032718                   0.5609673
#> shock 3 -6.0582599   0               0.002333011                   0.9976670

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_msh$new(p = 1, M = 2) |>
  estimate(S = 10) |> 
  verify_volatility() |> 
  summary() -> sddr_summary
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of structural shocks                    |
#>       homoskedasticity verification                |
#>  **************************************************|
```
