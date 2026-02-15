# Bayesian Estimation of Structural Vector Autoregressive Models

Provides fast and efficient procedures for Bayesian analysis of
Structural Vector Autoregressions. This package estimates a wide range
of models, including homo-, heteroskedastic and non-normal
specifications. Structural models can be identified by adjustable
exclusion restrictions, time-varying volatility, or non-normality, and
include exclusion restrictions on autoregressive parameters. They all
include a flexible three-level equation-specific local-global
hierarchical prior distribution for the estimated level of shrinkage for
autoregressive and structural parameters. Additionally, the package
facilitates predictive and structural analyses such as impulse
responses, forecast error variance and historical decompositions,
forecasting, verification of heteroskedasticity and hypotheses on
autoregressive parameters, and analyses of structural shocks,
volatilities, and fitted values. Beautiful plots, informative summary
functions, and extensive documentation including the vignette by Woźniak
(2024) \<doi:10.48550/arXiv.2410.15090\> complement all this. The
implemented techniques align closely with those presented in Lütkepohl,
Shang, Uzeda, & Woźniak (2025) \<doi:10.1016/j.jeconom.2025.106107\>,
Lütkepohl & Woźniak (2020) \<doi:10.1016/j.jedc.2020.103862\>, Song &
Woźniak (2021) \<doi:10.1093/acrefore/9780190625979.013.174\>, and
Woźniak & Droumaguet (2015) \<doi:10.13140/RG.2.2.19492.55687\>. The
'bsvars' package is aligned regarding objects, workflows, and code
structure with the R package 'bsvarSIGNs' by Wang & Woźniak (2024)
\<doi:10.32614/CRAN.package.bsvarSIGNs\>, and they constitute an
integrated toolset.

## Details

**Models.** All the SVAR models in this package are specified by two
equations, including the reduced form equation: \$\$Y = AX + E\$\$ where
\\Y\\ is an `NxT` matrix of dependent variables, \\X\\ is a `KxT` matrix
of explanatory variables, \\E\\ is an `NxT` matrix of reduced form error
terms, and \\A\\ is an `NxK` matrix of autoregressive slope coefficients
and parameters on deterministic terms in \\X\\.

The structural equation is given by: \$\$BE = U\$\$ where \\U\\ is an
`NxT` matrix of structural form error terms, and \\B\\ is an `NxN`
matrix of contemporaneous relationships.

Finally, all of the models share assumptions regarding the structural
shocks `U`, namely, temporal and contemporaneous independence. They
imply zero correlations and autocorrelations.

The various SVAR models estimated differ by the specification of
structural shocks variances. The different models include:

- homoskedastic model with unit variances

- heteroskedastic model with non-centred Stochastic Volatility process
  for variances

- heteroskedastic model with centred Stochastic Volatility process for
  variances

- heteroskedastic model with stationary Markov switching in the
  variances

- heteroskedastic model with sparse Markov switching in the variances
  where the number of heteroskedastic components is estimated

- heteroskedastic model with stationary heterogeneous Markov switching
  in the variances, where each shock volatility has its own Markov
  process

- heteroskedastic model with sparse heterogeneous Markov switching in
  the variances where the number of heteroskedastic components is
  estimated

- heteroskedastic model with exogenous heteroskedastic regime changes in
  the variances

- a model with Student-t distributed structural shocks with estimated
  equation-specific degrees-of-freedom parameter

- non-normal model with a finite mixture of normal components and
  component-specific variances

- non-normal model with a sparse mixture of normal components and
  component-specific variances where the number of heteroskedastic
  components is estimated

The structural shocks can be either normally or Student-t distributed,
where in the latter case the shock-specific degrees of freedom
parameters are estimated.

**Prior distributions.** All the models feature a Minnesota prior for
autoregressive parameters in matrix \\A\\ and a generalised-normal
distribution for the structural matrix \\B\\. Both of these
distributions feature a 3-level equation-specific local-global
hierarchical prior that make the shrinkage estimation flexible improving
the model fit and its forecasting performance.

**Estimation algorithm.** The models are estimated using frontier
numerical methods making the Gibbs sampler fast and efficient. The
estimation follows closely Lütkepohl, Shang, Uzeda, & Woźniak (2025).
The sampler of the structural matrix follows Waggoner & Zha (2003),
whereas that for autoregressive parameters follows Chan, Koop, Yu
(2022). The specification of Markov switching heteroskedasticity is
inspired by Song & Woźniak (2021), and that of Stochastic Volatility
model by Kastner & Frühwirth-Schnatter (2014). The identification
problems are considered in Lütkepohl, Shang, Uzeda, & Woźniak (2025) and
Lütkepohl & Woźniak (2020).

**Identification verification.** The structural shocks can be identified
through heteroskedasticity or non-normality following Lütkepohl, Shang,
Uzeda, & Woźniak (2025) and Lütkepohl & Woźniak (2020). The package
provides functions to verify both, homoskedasticity and normality of the
structural shocks, which facilitates making probabilistic statements
regarding the identification. Additionally, the package makes it
possible to verify linear restrictions on autoregressive parameters.

## Note

This package is currently in active development. Your comments,
suggestions and requests are warmly welcome!

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

Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural
Vector Autoregressions Identified by Markov-Switching
Heteroskedasticity. *Journal of Economic Dynamics and Control* **113**,
103862,
[doi:10.1016/j.jedc.2020.103862](https://doi.org/10.1016/j.jedc.2020.103862)
.

Song, Y., and Woźniak, T. (2021) Markov Switching Heteroskedasticity in
Time Series Analysis. In: *Oxford Research Encyclopedia of Economics and
Finance*. Oxford University Press,
[doi:10.1093/acrefore/9780190625979.013.174](https://doi.org/10.1093/acrefore/9780190625979.013.174)
.

Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural
vector autoregressions. *Journal of Economic Dynamics and Control*,
**28**, 349–366,
[doi:10.1016/S0165-1889(02)00168-9](https://doi.org/10.1016/S0165-1889%2802%2900168-9)
.

## See also

Useful links:

- <https://bsvars.org/bsvars/>

- Report bugs at <https://github.com/bsvars/bsvars/issues>

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
spec  = specify_bsvar_sv$new(         # specify the model
          us_fiscal_lsuw, 
          exogenous = us_fiscal_ex
        )
#> The identification is set to the default option of lower-triangular structural matrix.
burn  = estimate(spec, 5)             # run the burn-in
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
post  = estimate(burn, 10)            # estimate the model
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
irf   = compute_impulse_responses(    # compute impulse responses
          post, 
          horizon = 2
         )

# compute forecast error variance decomposition one year ahead
fevd  = compute_variance_decompositions(post, horizon = 4)

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(exogenous = us_fiscal_ex) |>
  estimate(S = 5) |> 
  estimate(S = 10) |> 
  compute_variance_decompositions(horizon = 4) -> fevds
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
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# conditional forecasting using a model with exogenous variables
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(exogenous = us_fiscal_ex) |>
  estimate(S = 5) |> 
  estimate(S = 10) -> post
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
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
  
 post |> forecast(
    horizon = 8,
    exogenous_forecast = us_fiscal_ex_forecasts,
    conditional_forecast = us_fiscal_cond_forecasts
  ) -> pred
  
  pred |> summary()
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of forecasts                  |
#>  **************************************************|
#> $variable1
#>        mean sd 5% quantile 95% quantile
#> 1 -8.914237  0   -8.914237    -8.914237
#> 2 -8.908921  0   -8.908921    -8.908921
#> 3 -8.903604  0   -8.903604    -8.903604
#> 4 -8.898288  0   -8.898288    -8.898288
#> 5 -8.892971  0   -8.892971    -8.892971
#> 6 -8.887655  0   -8.887655    -8.887655
#> 7 -8.882338  0   -8.882338    -8.882338
#> 8 -8.877022  0   -8.877022    -8.877022
#> 
#> $variable2
#>        mean         sd 5% quantile 95% quantile
#> 1 -9.801570 0.02461738   -9.826443    -9.759645
#> 2 -9.781897 0.05291951   -9.829511    -9.692489
#> 3 -9.771452 0.03441272   -9.810729    -9.722865
#> 4 -9.764805 0.02728938   -9.795746    -9.729700
#> 5 -9.737497 0.03838067   -9.788843    -9.682628
#> 6 -9.713819 0.04279013   -9.783243    -9.662745
#> 7 -9.704600 0.05570405   -9.792471    -9.641823
#> 8 -9.687173 0.05591612   -9.762390    -9.632503
#> 
#> $variable3
#>        mean          sd 5% quantile 95% quantile
#> 1 -7.038850 0.008640515   -7.051357    -7.026558
#> 2 -7.027129 0.021443456   -7.057193    -6.998167
#> 3 -7.022918 0.022102259   -7.053668    -6.998491
#> 4 -7.022433 0.028556017   -7.058885    -6.984686
#> 5 -7.010358 0.032624488   -7.062007    -6.973963
#> 6 -6.998988 0.035197000   -7.052331    -6.959375
#> 7 -6.997833 0.036982789   -7.054506    -6.955022
#> 8 -6.990241 0.030686309   -7.040281    -6.954311
#> 
  pred |> plot(probability = 0.68)

  
# estimation of a model with exogeneity restrictions on the  autoregressive matrix
#############################################################
A = matrix(TRUE, 3, 7)
A[1,3] = A[1,6] = FALSE
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 2, A = A) |>
  estimate(S = 5) |> 
  estimate(S = 10) -> post
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
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
post |> summary()
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1194719 0.004819527   0.1128786     0.126199
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -9.356277 0.5603879   -10.07410     -8.51888
#> B[2,2] 42.512822 2.5341297    38.74305     45.79078
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -27.744161 1.300482   -29.84577   -26.201960
#> B[3,2]  -7.333758 3.339787   -11.96375    -3.077487
#> B[3,3] 101.762096 5.205416    96.79466   110.253466
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.36821264 0.11119564  0.20033633   0.47820684
#> lag1_var1 -0.01079518 0.07517436 -0.10739904   0.10219896
#> lag1_var2  0.00000000 0.00000000  0.00000000   0.00000000
#> lag2_var2 -0.19009102 0.10842426 -0.29920507  -0.02339306
#> lag2_var3  0.08290319 0.07840665 -0.03293198   0.17455763
#> lag2_var3  0.00000000 0.00000000  0.00000000   0.00000000
#> const      1.29335160 0.22091919  1.05801206   1.65583041
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.08079585 0.06157712  -0.1639466 -0.005912932
#> lag1_var1  1.32562528 0.04810601   1.2554261  1.395657225
#> lag1_var2 -0.02792873 0.14746302  -0.2366955  0.169167395
#> lag2_var2 -0.10556671 0.06359887  -0.1922593 -0.028166620
#> lag2_var3 -0.35150369 0.04537706  -0.4088457 -0.282082178
#> lag2_var3  0.04192893 0.15250754  -0.1555828  0.268448060
#> const     -0.07764524 0.12123637  -0.2507233  0.062169690
#> 
#> $A$equation3
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1 -0.16080159 0.04945300 -0.224462088  -0.09419038
#> lag1_var1 -0.01974789 0.02487709 -0.050642494   0.02226621
#> lag1_var2  0.97136418 0.04945745  0.911779437   1.02906339
#> lag2_var2 -0.07420789 0.04713039 -0.137116046  -0.01310627
#> lag2_var3  0.03813332 0.02410573 -0.003764333   0.06451397
#> lag2_var3  0.02445496 0.04836261 -0.034274487   0.08471546
#> const      0.34018130 0.05793614  0.267311261   0.42804340
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         46.76948  41.86744    7.343454     111.1308
#> B[2,]_shrinkage        202.39598  82.58750  113.252063     336.8246
#> B[3,]_shrinkage       1067.62451 391.33459  536.639196    1625.6988
#> B[1,]_shrinkage_scale  423.43113 417.24099   80.039587    1105.0414
#> B[2,]_shrinkage_scale  565.30993 344.13797  249.348601    1092.2456
#> B[3,]_shrinkage_scale  655.88459 594.70820  146.789987    1692.5588
#> B_global_scale          48.73223  44.05628   11.743976     124.7098
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4077533 0.23847674   0.2611852    0.7767609
#> A[2,]_shrinkage       0.1896373 0.05754023   0.1221834    0.2742914
#> A[3,]_shrinkage       0.2081753 0.10506497   0.1155404    0.3765476
#> A[1,]_shrinkage_scale 3.9119189 0.65201363   3.1519602    4.6940409
#> A[2,]_shrinkage_scale 3.1114483 1.27400186   2.2555118    5.3782120
#> A[3,]_shrinkage_scale 2.9740477 1.14822406   1.7627029    4.9295659
#> A_global_scale        0.5032632 0.13970375   0.4030431    0.7621052
#> 
#> 
```
