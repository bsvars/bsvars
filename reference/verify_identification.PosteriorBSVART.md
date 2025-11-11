# Verifies identification through heteroskedasticity or non-normality of of structural shocks

Computes the logarithm of Bayes factor for the hypothesis of normality
of the joint conditional distribution of the structural shocks via
Savage-Dickey Density Ration (SDDR). The hypothesis of normality in this
t-distributed shocks model is represented by restriction setting the
degrees-of-freedom parameter \\\nu\\ to infinity: \$\$H_0: \nu =
\infty\$\$ The logarithm of Bayes factor for this hypothesis can be
computed using the SDDR as the difference of logarithms of the marginal
posterior distribution ordinate at the restriction less the marginal
prior distribution ordinate at the same point: \$\$log p(H_0 \| data) -
log p(H_0)\$\$ Therefore, a negative value of the difference is the
evidence against homoskedasticity of the structural shock. The
estimation of the marginal posterior ordinate is done using truncated
Gaussian kernel smoothing.

## Usage

``` r
# S3 method for class 'PosteriorBSVART'
verify_identification(posterior)
```

## Arguments

- posterior:

  the estimation outcome obtained using
  [`estimate`](https://bsvars.org/bsvars/reference/estimate.md) function

## Value

An object of class `SDDRidT` that is a list with components:

`logSDDR` the value of the logarithm of the Bayes factor

`SDDR` the value of the Bayes factor

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

[`specify_bsvar_t`](https://bsvars.org/bsvars/reference/specify_bsvar_t.md),
[`estimate`](https://bsvars.org/bsvars/reference/estimate.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
# specify the model
specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

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

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_t$new() |>
  estimate(S = 10) |> 
  verify_identification() -> sddr
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
  
```
