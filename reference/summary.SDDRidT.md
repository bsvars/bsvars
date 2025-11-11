# Provides summary of verifying shocks' normality

Provides summary of the Savage-Dickey density ratios for verification of
structural shocks normality. The outcomes can be used to make
probabilistic statements about identification through non-normality.

## Usage

``` r
# S3 method for class 'SDDRidT'
summary(object, ...)
```

## Arguments

- object:

  an object of class `SDDRidT` obtained using the
  [`verify_identification.PosteriorBSVART`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVART.md)
  function.

- ...:

  additional arguments affecting the summary produced.

## Value

A table reporting the Bayes factor of normal to Student-t shocks
posterior odds `"SDDR"` as well as its logarithm `"log(SDDR)"`for each
structural shock, and the implied posterior probability of the normality
and Student-t hypothesis, `"Pr[normal|data]"` and `"Pr[Student-t|data]"`
respectively.

## See also

[`verify_identification.PosteriorBSVART`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVART.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
set.seed(123)

# estimate the model
posterior      = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>    H0: df = Inf    [normal shocks]                 |
#>    H1: df != Inf   [Student-t shocks]              |
#>  **************************************************|
#>         log(SDDR)     SDDR Pr[H0|data] Pr[H1|data]
#> shock 1 -1.210447 0.298064   0.2296219   0.7703781
#> shock 2 -1.210447 0.298064   0.2296219   0.7703781
#> shock 3 -1.210447 0.298064   0.2296219   0.7703781

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_t$new() |>
  estimate(S = 10) |> 
  verify_identification() |> 
  summary() -> sddr_summary
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of identification verification          |
#>    H0: df = Inf    [normal shocks]                 |
#>    H1: df != Inf   [Student-t shocks]              |
#>  **************************************************|
```
