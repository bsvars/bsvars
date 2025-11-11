# Provides summary of verifying shocks' normality

Provides summary of the Savage-Dickey density ratios for verification of
structural shocks normality. The outcomes can be used to make
probabilistic statements about identification through non-normality.

## Usage

``` r
# S3 method for class 'SDDRidMIX'
summary(object, ...)
```

## Arguments

- object:

  an object of class `SDDRidMIX` obtained using the
  [`verify_identification.PosteriorBSVARMIX`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARMIX.md)
  function.

- ...:

  additional arguments affecting the summary produced.

## Value

A table reporting the logarithm of Bayes factors of normal to non-normal
shocks posterior odds `"log(SDDR)"` for each structural shock, their
numerical standard errors `"NSE"`, and the implied posterior probability
of the normality and non-normality hypothesis, `"Pr[normal|data]"` and
`"Pr[non-normal|data]"` respectively.

## See also

[`verify_identification.PosteriorBSVARMIX`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARMIX.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.
set.seed(123)

# estimate the model
posterior      = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# verify heteroskedasticity
sddr           = verify_identification(posterior)
summary(sddr)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of identification verification          |
#>    H0: s^2_nm  = 1 for all m  [normal]             |
#>    H1: s^2_nm != 1 for some m [non-normal]         |
#>  **************************************************|
#>          log(SDDR) NSE Pr[H0|data] Pr[H1|data]
#> shock 1  2.0468239   0  0.88562630   0.1143737
#> shock 2 -0.6828998   0  0.33561440   0.6643856
#> shock 3 -2.5054192   0  0.07547915   0.9245209

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_mix$new(M = 2) |>
  estimate(S = 10) |> 
  verify_identification() |> 
  summary() -> sddr_summary
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of identification verification          |
#>    H0: s^2_nm  = 1 for all m  [normal]             |
#>    H1: s^2_nm != 1 for some m [non-normal]         |
#>  **************************************************|
```
