# Verifies identification through heteroskedasticity or non-normality of of structural shocks

Computes the logarithm of Bayes factor(s) for the hypothesis in which
the model is not identified through heteroskedasticity of non-normality
using Savage-Dickey Density Ration (SDDR). The hypothesis of no such
identification, \\H_0\\, is represented by model-specific
restrictions.Consult help files for individual classes of models for
details. The logarithm of Bayes factor for this hypothesis can be
computed using the SDDR as the difference of the logarithm of the
marginal posterior distribution ordinate at the restriction less the
log-marginal prior distribution ordinate at the same point: \$\$log
p(H_0 \| data) - log p(H_0)\$\$ Therefore, a negative value of the
difference is the evidence against the lack of identification of the
structural shock through heteroskedasticity or non-normality.

## Usage

``` r
verify_identification(posterior)
```

## Arguments

- posterior:

  the estimation outcome obtained using
  [`estimate`](https://bsvars.org/bsvars/reference/estimate.md) function

## Value

An object of class `SDDRid*` that is a list with components:

`logSDDR` a vector with values of the logarithm of the Bayes factors

`log_SDDR_se` a vector with numerical standard errors of the logarithm
of the Bayes factors reported in output element `logSDDR` that are
computed based on 30 random sub-samples of the log-ordinates of the
marginal posterior and prior distributions.

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

[`verify_identification.PosteriorBSVAR`](https://bsvars.org/bsvars/reference/verify_identification.PosteriorBSVAR.md),
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
specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#> The identification is set to the default option of lower-triangular structural matrix.

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

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 1) |>
  estimate(S = 10) |> 
  verify_identification() -> sddr
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
```
