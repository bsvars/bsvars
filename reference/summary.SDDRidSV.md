# Provides summary of verifying homoskedasticity

Provides summary of the Savage-Dickey density ratios for verification of
structural shocks homoskedasticity. The outcomes can be used to make
probabilistic statements about identification through heteroskedasticity
following Lütkepohl, Shang, Uzeda & Woźniak (2024).

## Usage

``` r
# S3 method for class 'SDDRidSV'
summary(object, ...)
```

## Arguments

- object:

  an object of class `SDDRidSV` obtained using the
  [`verify_identification.PosteriorBSVARSV`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARSV.md)
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

## References

Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial
Identification of Heteroskedastic Structural VARs: Theory and Bayesian
Inference. *University of Melbourne Working Paper*, 1–57,
[doi:10.48550/arXiv.2404.11057](https://doi.org/10.48550/arXiv.2404.11057)
.

## See also

[`verify_identification.PosteriorBSVARSV`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARSV.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
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

# verify heteroskedasticity
sddr           = verify_identification(posterior)
summary(sddr)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Summary of identification verification          |
#>      H0: omega_n = 0  [homoskedasticity]           |
#>      H1: omega_n != 0 [heteroskedasticity]         |
#>  **************************************************|
#>          log(SDDR) NSE Pr[H0|data] Pr[H1|data]
#> shock 1 -2.2328462   0 0.096839419   0.9031606
#> shock 2 -6.6594539   0 0.001280205   0.9987198
#> shock 3  0.5896183   0 0.643277567   0.3567224

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 10) |> 
  verify_identification() |> 
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
#>    Summary of identification verification          |
#>      H0: omega_n = 0  [homoskedasticity]           |
#>      H1: omega_n != 0 [heteroskedasticity]         |
#>  **************************************************|
```
