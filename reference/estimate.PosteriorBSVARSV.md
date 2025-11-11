# Bayesian estimation of a Structural Vector Autoregression with Stochastic Volatility heteroskedasticity via Gibbs sampler

Estimates the SVAR with Stochastic Volatility (SV) heteroskedasticity
proposed by Lütkepohl, Shang, Uzeda, and Woźniak (2025). Implements the
Gibbs sampler proposed by Waggoner & Zha (2003) for the structural
matrix \\B\\ and the equation-by-equation sampler by Chan, Koop, & Yu
(2024) for the autoregressive slope parameters \\A\\. Additionally, the
parameter matrices \\A\\ and \\B\\ follow a Minnesota prior and
generalised-normal prior distributions respectively with the
matrix-specific overall shrinkage parameters estimated thanks to a
hierarchical prior distribution. The SV model is estimated using a range
of techniques including: simulation smoother, auxiliary mixture,
ancillarity-sufficiency interweaving strategy, and generalised inverse
Gaussian distribution summarised by Kastner & Frühwirth-Schnatter
(2014). See section **Details** for the model equations.

## Usage

``` r
# S3 method for class 'PosteriorBSVARSV'
estimate(specification, S, thin = 1, show_progress = TRUE)
```

## Arguments

- specification:

  an object of class PosteriorBSVARSV generated using the
  [`estimate.BSVAR()`](https://bsvars.org/bsvars/reference/estimate.BSVAR.md)
  function. This setup facilitates the continuation of the MCMC sampling
  starting from the last draw of the previous run.

- S:

  a positive integer, the number of posterior draws to be generated

- thin:

  a positive integer, specifying the frequency of MCMC output thinning

- show_progress:

  a logical value, if `TRUE` the estimation progress bar is visible

## Value

An object of class PosteriorBSVARSV containing the Bayesian estimation
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

- h:

  an `NxTxS` array with the posterior draws of the log-volatility
  processes

- rho:

  an `NxS` matrix with the posterior draws of SV autoregressive
  parameters

- omega:

  an `NxS` matrix with the posterior draws of SV process conditional
  standard deviations

- S:

  an `NxTxS` array with the posterior draws of the auxiliary mixture
  component indicators

- sigma2_omega:

  an `NxS` matrix with the posterior draws of the variances of the
  zero-mean normal prior for `omega`

- s\_:

  an `S`-vector with the posterior draws of the scale of the gamma prior
  of the hierarchical prior for `sigma2_omega`

`last_draw` an object of class BSVARSV with the last draw of the current
MCMC run as the starting value to be passed to the continuation of the
MCMC estimation using
[`estimate()`](https://bsvars.org/bsvars/reference/estimate.md).

## Details

The heteroskedastic SVAR model is given by the reduced form equation:
\$\$Y = AX + E\$\$ where \\Y\\ is an `NxT` matrix of dependent
variables, \\X\\ is a `KxT` matrix of explanatory variables, \\E\\ is an
`NxT` matrix of reduced form error terms, and \\A\\ is an `NxK` matrix
of autoregressive slope coefficients and parameters on deterministic
terms in \\X\\.

The structural equation is given by \$\$BE = U\$\$ where \\U\\ is an
`NxT` matrix of structural form error terms, and \\B\\ is an `NxN`
matrix of contemporaneous relationships. Finally, the structural shocks,
\\U\\, are temporally and contemporaneously independent and jointly
distributed with zero mean. The structural shocks can be either normally
or Student-t distributed, where in the latter case the shock-specific
degrees of freedom parameters are estimated.

Two alternative specifications of the conditional variance of the `n`th
shock at time `t` can be estimated: non-centred Stochastic Volatility by
Lütkepohl, Shang, Uzeda, and Woźniak (2022) or centred Stochastic
Volatility by Chan, Koop, & Yu (2021).

The non-centred Stochastic Volatility by Lütkepohl, Shang, Uzeda, and
Woźniak (2022) is selected by setting argument `centred_sv` of function
`specify_bsvar_sv$new()` to value `FALSE`. It has the conditional
variances given by: \$\$Var\_{t-1}\[u\_{n.t}\] = exp(w_n h\_{n.t})\$\$
where \\w_n\\ is the estimated conditional standard deviation of the
log-conditional variance and the log-volatility process \\h\_{n.t}\\
follows an autoregressive process: \$\$h\_{n.t} = g_n h\_{n.t-1} +
v\_{n.t}\$\$ where \\h\_{n.0}=0\\, \\g_n\\ is an autoregressive
parameter and \\v\_{n.t}\\ is a standard normal error term.

The centred Stochastic Volatility by Chan, Koop, & Yu (2021) is selected
by setting argument `centred_sv` of function `specify_bsvar_sv$new()` to
value `TRUE`. Its conditional variances are given by:
\$\$Var\_{t-1}\[u\_{n.t}\] = exp(h\_{n.t})\$\$ where the log-conditional
variances \\h\_{n.t}\\ follow an autoregressive process: \$\$h\_{n.t} =
g_n h\_{n.t-1} + v\_{n.t}\$\$ where \\h\_{n.0}=0\\, \\g_n\\ is an
autoregressive parameter and \\v\_{n.t}\\ is a zero-mean normal error
term with variance \\s\_{v.n}^2\\.

## References

Chan, J.C.C., Koop, G, and Yu, X. (2024) Large Order-Invariant Bayesian
VARs with Stochastic Volatility. *Journal of Business & Economic
Statistics*, **42**,
[doi:10.1080/07350015.2023.2252039](https://doi.org/10.1080/07350015.2023.2252039)
.

Kastner, G. and Frühwirth-Schnatter, S. (2014) Ancillarity-Sufficiency
Interweaving Strategy (ASIS) for Boosting MCMC Estimation of Stochastic
Volatility Models. *Computational Statistics & Data Analysis*, **76**,
408–423,
[doi:10.1016/j.csda.2013.01.002](https://doi.org/10.1016/j.csda.2013.01.002)
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

[`specify_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_bsvar_sv.md),
[`specify_posterior_bsvar_sv`](https://bsvars.org/bsvars/reference/specify_posterior_bsvar_sv.md),
[`normalise`](https://bsvars.org/bsvars/reference/normalise.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.
burn_in        = estimate(specification, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
posterior      = estimate(burn_in, 5)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new() |>
  estimate(S = 5) |> 
  estimate(S = 5) -> post
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-SV model              |
#>    Non-centred SV model is estimated              |
#> **************************************************|
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
```
