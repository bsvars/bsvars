# Bayesian estimation of a homoskedastic Structural Vector Autoregression via Gibbs sampler

Estimates the homoskedastic SVAR using the Gibbs sampler proposed by
Waggoner & Zha (2003) for the structural matrix \\B\\ and the
equation-by-equation sampler by Chan, Koop, & Yu (2024) for the
autoregressive slope parameters \\A\\. Additionally, the parameter
matrices \\A\\ and \\B\\ follow a Minnesota prior and generalised-normal
prior distributions respectively with the matrix-specific overall
shrinkage parameters estimated using a hierarchical prior distribution
as in Lütkepohl, Shang, Uzeda, and Woźniak (2025). See section
**Details** for the model equations.

## Usage

``` r
# S3 method for class 'BSVAR'
estimate(specification, S, thin = 1, show_progress = TRUE)
```

## Arguments

- specification:

  an object of class BSVAR generated using the `specify_bsvar$new()`
  function.

- S:

  a positive integer, the number of posterior draws to be generated

- thin:

  a positive integer, specifying the frequency of MCMC output thinning

- show_progress:

  a logical value, if `TRUE` the estimation progress bar is visible

## Value

An object of class PosteriorBSVAR containing the Bayesian estimation
output and containing two elements:

`posterior` a list with a collection of `S` draws from the posterior
distribution generated via Gibbs sampler containing:

- A:

  an `NxKxS` array with the posterior draws for matrix \\A\\

- B:

  an `NxNxS` array with the posterior draws for matrix \\B\\

- hyper:

  a `5xS` matrix with the posterior draws for the hyper-parameters of
  the hierarchical prior distribution

`last_draw` an object of class BSVAR with the last draw of the current
MCMC run as the starting value to be passed to the continuation of the
MCMC estimation using
[`estimate()`](https://bsvars.org/bsvars/reference/estimate.md).

## Details

The homoskedastic SVAR model is given by the reduced form equation:
\$\$Y = AX + E\$\$ where \\Y\\ is an `NxT` matrix of dependent
variables, \\X\\ is a `KxT` matrix of explanatory variables, \\E\\ is an
`NxT` matrix of reduced form error terms, and \\A\\ is an `NxK` matrix
of autoregressive slope coefficients and parameters on deterministic
terms in \\X\\.

The structural equation is given by \$\$BE = U\$\$ where \\U\\ is an
`NxT` matrix of structural form error terms, and \\B\\ is an `NxN`
matrix of contemporaneous relationships.

Finally, the structural shocks, `U`, are temporally and
contemporaneously independent and jointly distributed with zero mean and
unit variances. The structural shocks can be either normally or
Student-t distributed, where in the latter case the shock-specific
degrees of freedom parameters are estimated.

## References

Chan, J.C.C., Koop, G, and Yu, X. (2024) Large Order-Invariant Bayesian
VARs with Stochastic Volatility. *Journal of Business & Economic
Statistics*, **42**,
[doi:10.1080/07350015.2023.2252039](https://doi.org/10.1080/07350015.2023.2252039)
.

Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2025) Partial
identification of structural vector autoregressions with non-centred
stochastic volatility. *Journal of Econometrics*, 1–18,
[doi:10.1016/j.jeconom.2025.106107](https://doi.org/10.1016/j.jeconom.2025.106107)
.

Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural
vector autoregressions. *Journal of Economic Dynamics and Control*,
**28**, 349–366,
[doi:10.1016/S0165-1889(02)00168-9](https://doi.org/10.1016/S0165-1889%2802%2900168-9)
.

## See also

[`specify_bsvar`](https://bsvars.org/bsvars/reference/specify_bsvar.md),
[`specify_posterior_bsvar`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar.md),
[`normalise`](https://bsvars.org/bsvars/reference/normalise.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
specification  = specify_bsvar$new(us_fiscal_lsuw, p = 4)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar$new(p = 1) |>
  estimate(S = 5) |> 
  estimate(S = 5) -> post
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
```
