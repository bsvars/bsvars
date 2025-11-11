# Verifies hypotheses involving autoregressive parameters

Computes the logarithm of Bayes factor for the joint hypothesis,
\\H_0\\, possibly for many autoregressive parameters represented by
argument `hypothesis` via Savage-Dickey Density Ration (SDDR). The
logarithm of Bayes factor for this hypothesis can be computed using the
SDDR as the difference of logarithms of the marginal posterior
distribution ordinate at the restriction less the marginal prior
distribution ordinate at the same point: \$\$log p(H_0 \| data) - log
p(H_0)\$\$ Therefore, a negative value of the difference is the evidence
against hypothesis. The estimation of both elements of the difference
requires numerical integration.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
verify_autoregression(posterior, hypothesis)
```

## Arguments

- posterior:

  the `posterior` element of the list from the estimation outcome

- hypothesis:

  an `NxK` matrix of the same dimension as the autoregressive matrix
  \\A\\ with numeric values for the parameters to be verified, in which
  case the values represent the joint hypothesis, and missing value `NA`
  for these parameters that are not tested

## Value

An object of class `SDDRautoregression` that is a list of three
components:

`logSDDR` a scalar with values of the logarithm of the Bayes factors for
the autoregressive hypothesis for each of the shocks

`log_SDDR_se` an `N`-vector with estimation standard errors of the
logarithm of the Bayes factors reported in output element `logSDDR` that
are computed based on 30 random sub-samples of the log-ordinates of the
marginal posterior and prior distributions.

`components` a list of three components for the computation of the Bayes
factor

- log_denominator:

  an `N`-vector with values of the logarithm of the Bayes factor
  denominators

- log_numerator:

  an `N`-vector with values of the logarithm of the Bayes factor
  numerators

- log_numerator_s:

  an `NxS` matrix of the log-full conditional posterior density
  ordinates computed to estimate the numerator

- log_denominator_s:

  an `NxS` matrix of the log-full conditional posterior density
  ordinates computed to estimate the denominator

- se_components:

  a `30`-vector containing the log-Bayes factors on the basis of which
  the standard errors are computed

## References

Woźniak, T., and Droumaguet, M., (2024) Bayesian Assessment of
Identifying Restrictions for Heteroskedastic Structural VARs

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# simple workflow
############################################################
# specify the model
specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 1, M = 2)
#> The identification is set to the default option of lower-triangular structural matrix.

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

# verify autoregression
H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
sddr           = verify_autoregression(posterior, H0)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_mix$new(p = 1, M = 2) |>
  estimate(S = 10) |> 
  verify_autoregression(hypothesis = H0) -> sddr
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
```
