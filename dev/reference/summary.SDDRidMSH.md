# Provides summary of verifying homoskedasticity

Provides summary of the Savage-Dickey density ratios for verification of
structural shocks homoskedasticity. The outcomes can be used to make
probabilistic statements about identification through heteroskedasticity
closely following ideas by Lütkepohl& Woźniak (2020).

## Usage

``` r
# S3 method for class 'SDDRidMSH'
summary(object, ...)
```

## Arguments

- object:

  an object of class `SDDRidMSH` obtained using the
  [`verify_identification.PosteriorBSVARMSH`](https://bsvars.org/bsvars/dev/reference/verify_identification.PosteriorBSVARMSH.md)
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

Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural
Vector Autoregressions Identified by Markov-Switching
Heteroskedasticity. *Journal of Economic Dynamics and Control* **113**,
103862,
[doi:10.1016/j.jedc.2020.103862](https://doi.org/10.1016/j.jedc.2020.103862)
.

## See also

[`verify_identification.PosteriorBSVARMSH`](https://bsvars.org/bsvars/dev/reference/verify_identification.PosteriorBSVARMSH.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
specification  = specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.
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
sddr           = verify_identification(posterior)
summary(sddr)
#>          log(SDDR) NSE Pr[H0|data] Pr[H1|data]
#> shock 1  1.8915555   0   0.8689328   0.1310672
#> shock 2 -0.4249424   0   0.3953347   0.6046653
#> shock 3 -0.2986372   0   0.4258907   0.5741093

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new(M = 2) |>
  estimate(S = 10) |> 
  verify_identification() |> 
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
```
