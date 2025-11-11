# Bayesian estimation of a Structural Vector Autoregression with exogenous heteroskedastic regime changes via Gibbs sampler

Estimates the SVAR with exogenous heteroskedastic regime changes with
`M` regimes (MS(M)) proposed by Woźniak & Droumaguet (2022). Implements
the Gibbs sampler proposed by Waggoner & Zha (2003) for the structural
matrix \\B\\ and the equation-by-equation sampler by Chan, Koop, & Yu
(2024) for the autoregressive slope parameters \\A\\. Additionally, the
parameter matrices \\A\\ and \\B\\ follow a Minnesota prior and
generalised-normal prior distributions respectively with the
matrix-specific overall shrinkage parameters estimated thanks to a
hierarchical prior distribution. The MS model is estimated using the
prior distributions and algorithms proposed by Woźniak & Droumaguet
(2024), Lütkepohl & Woźniak (2020), and Song & Woźniak (2021). See
section **Details** for the model equations.

## Usage

``` r
# S3 method for class 'BSVAREXH'
estimate(specification, S, thin = 1, show_progress = TRUE)
```

## Arguments

- specification:

  an object of class BSVAREXH generated using the
  `specify_bsvar_exh$new()` function.

- S:

  a positive integer, the number of posterior draws to be generated

- thin:

  a positive integer, specifying the frequency of MCMC output thinning

- show_progress:

  a logical value, if `TRUE` the estimation progress bar is visible

## Value

An object of class PosteriorBSVAREXH containing the Bayesian estimation
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

- sigma2:

  an `NxMxS` array with the posterior draws for the structural shocks
  conditional variances

- xi:

  an `MxTxS` array with the exogenous regime allocation matrix.

- sigma:

  an `NxTxS` array with the posterior draws for the structural shocks
  conditional standard deviations' series over the sample period

`last_draw` an object of class BSVAREXH with the last draw of the
current MCMC run as the starting value to be passed to the continuation
of the MCMC estimation using
[`estimate()`](https://bsvars.org/bsvars/reference/estimate.md).

## Details

The heteroskedastic SVAR model is given by the reduced form equation:
\$\$Y = AX + E\$\$ where \\Y\\ is an `NxT` matrix of dependent
variables, \\X\\ is a `KxT` matrix of explanatory variables, \\E\\ is an
`NxT` matrix of reduced form error terms, and \\A\\ is an `NxK` matrix
of autoregressive slope coefficients and parameters on deterministic
terms in `X`.

The structural equation is given by \$\$BE = U\$\$ where \\U\\ is an
`NxT` matrix of structural form error terms, and \\B\\ is an `NxN`
matrix of contemporaneous relationships.

Finally, the structural shocks, \\U\\, are temporally and
contemporaneously independent and jointly distributed with zero mean.
The structural shocks can be either normally or Student-t distributed,
where in the latter case the shock-specific degrees of freedom
parameters are estimated. The conditional variance of the `n`th shock at
time `t` is given by: \$\$Var\_{t-1}\[u\_{n.t}\] = s^2\_{n.s_t}\$\$
where \\s_t\\ is an exogenous process driving the time-variability of
the regime-specific conditional variances of structural shocks
\\s^2\_{n.s_t}\\. In this model, the variances of each of the structural
shocks sum to `M`. The model selection also with this respect is made
using function
[`specify_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_bsvar_exh.md).

## References

Chan, J.C.C., Koop, G, and Yu, X. (2024) Large Order-Invariant Bayesian
VARs with Stochastic Volatility. *Journal of Business & Economic
Statistics*, **42**,
[doi:10.1080/07350015.2023.2252039](https://doi.org/10.1080/07350015.2023.2252039)
.

Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural
Vector Autoregressions Identified by Markov-Switching
Heteroskedasticity. *Journal of Economic Dynamics and Control* **113**,
103862,
[doi:10.1016/j.jedc.2020.103862](https://doi.org/10.1016/j.jedc.2020.103862)
.

Song, Y., and Woźniak, T., (2021) Markov Switching. *Oxford Research
Encyclopedia of Economics and Finance*, Oxford University Press,
[doi:10.1093/acrefore/9780190625979.013.174](https://doi.org/10.1093/acrefore/9780190625979.013.174)
.

Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural
vector autoregressions. *Journal of Economic Dynamics and Control*,
**28**, 349–366,
[doi:10.1016/S0165-1889(02)00168-9](https://doi.org/10.1016/S0165-1889%2802%2900168-9)
.

Woźniak, T., and Droumaguet, M., (2024) Bayesian Assessment of
Identifying Restrictions for Heteroskedastic Structural VARs

## See also

[`specify_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_bsvar_exh.md),
[`specify_posterior_bsvar_exh`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_exh.md),
[`normalise`](https://bsvars.org/bsvars/reference/normalise.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
spec  = specify_bsvar_exh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn  = estimate(spec, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
post  = estimate(burn, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_exh$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) -> post
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-exH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
```
