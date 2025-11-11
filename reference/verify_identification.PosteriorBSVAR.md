# Verifies identification through heteroskedasticity or non-normality of of structural shocks

Displays information that the model is homoskedastic and with normal
shocks.

## Usage

``` r
# S3 method for class 'PosteriorBSVAR'
verify_identification(posterior)
```

## Arguments

- posterior:

  the estimation outcome obtained using
  [`estimate`](https://bsvars.org/bsvars/reference/estimate.md) function

## Value

Nothing. Just displays a message.

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

## See also

`verify_identification.PosteriorBSVAR`,
[`verify_identification.PosteriorBSVARSV`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARSV.md),
[`verify_identification.PosteriorBSVARMIX`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARMIX.md),
[`verify_identification.PosteriorBSVARMSH`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVARMSH.md),
[`verify_identification.PosteriorBSVART`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVART.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
# specify the model
specification  = specify_bsvar$new(us_fiscal_lsuw, p = 1)
#> The identification is set to the default option of lower-triangular structural matrix.

# estimate the model
posterior      = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# verify heteroskedasticity
sddr           = verify_identification(posterior)
#> The model is homoskedastic with normal shocks.

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 10) |> 
  verify_identification() -> sddr
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> The model is homoskedastic with normal shocks.
  
```
