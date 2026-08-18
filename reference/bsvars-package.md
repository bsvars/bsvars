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
#> 1 -8.860009  0   -8.860009    -8.860009
#> 2 -8.854638  0   -8.854638    -8.854638
#> 3 -8.849268  0   -8.849268    -8.849268
#> 4 -8.843897  0   -8.843897    -8.843897
#> 5 -8.838526  0   -8.838526    -8.838526
#> 6 -8.833155  0   -8.833155    -8.833155
#> 7 -8.827784  0   -8.827784    -8.827784
#> 8 -8.822413  0   -8.822413    -8.822413
#> 
#> $variable2
#>        mean         sd 5% quantile 95% quantile
#> 1 -9.808946 0.01149434   -9.827014    -9.799523
#> 2 -9.814720 0.02572567   -9.847994    -9.778644
#> 3 -9.784490 0.04403206   -9.830961    -9.718780
#> 4 -9.784122 0.04898702   -9.850565    -9.710810
#> 5 -9.771789 0.05413591   -9.831524    -9.688980
#> 6 -9.767757 0.08552309   -9.856296    -9.623689
#> 7 -9.757303 0.08538602   -9.860400    -9.628535
#> 8 -9.748408 0.07632579   -9.841617    -9.624426
#> 
#> $variable3
#>        mean         sd 5% quantile 95% quantile
#> 1 -7.036665 0.02212695   -7.064291    -7.006164
#> 2 -7.022156 0.03168179   -7.060605    -6.975022
#> 3 -7.008151 0.03972318   -7.044064    -6.946744
#> 4 -7.004043 0.04512843   -7.043386    -6.922379
#> 5 -6.992436 0.03633704   -7.046851    -6.941121
#> 6 -6.992670 0.04503232   -7.050485    -6.926492
#> 7 -6.993473 0.05344579   -7.055922    -6.903757
#> 8 -6.977678 0.05343765   -7.051852    -6.899576
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
#> B[1,1] 0.1939329 0.009646932   0.1818694    0.2082268
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -35.37309 1.324593   -37.57746    -33.88566
#> B[2,2]  22.01494 0.826966    21.08650     23.39243
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1]  -9.691070 1.934011   -12.82203    -7.556679
#> B[3,2]  -9.294537 1.578474   -10.95979    -6.695958
#> B[3,3] 111.355140 4.068384   105.33597   116.746219
#> 
#> 
#> $A
#> $A$equation1
#>                  mean        sd 5% quantile 95% quantile
#> lag1_var1  1.15325419 0.1299464   0.9886993    1.3082243
#> lag1_var1 -0.12858231 0.1617556  -0.3140787    0.1153905
#> lag1_var2  0.00000000 0.0000000   0.0000000    0.0000000
#> lag2_var2 -0.49107347 0.1279825  -0.6406316   -0.3269496
#> lag2_var3 -0.05203469 0.1411378  -0.2644317    0.1120335
#> lag2_var3  0.00000000 0.0000000   0.0000000    0.0000000
#> const      0.40519764 0.2578049   0.1662115    0.7723809
#> 
#> $A$equation2
#>                 mean        sd 5% quantile 95% quantile
#> lag1_var1  0.3301836 0.2300241  0.05478997    0.6588747
#> lag1_var1  1.1618567 0.2887499  0.89870093    1.6664636
#> lag1_var2 -0.6010168 0.1970295 -0.83144344   -0.2793773
#> lag2_var2 -0.7982022 0.2390196 -1.13698558   -0.5162058
#> lag2_var3 -0.4816495 0.2611741 -0.93964634   -0.2442424
#> lag2_var3  0.5161287 0.2018397  0.19922785    0.7744633
#> const      0.4181370 0.3625140  0.02855412    0.8847290
#> 
#> $A$equation3
#>                   mean         sd  5% quantile 95% quantile
#> lag1_var1  0.051348381 0.03710628  0.004531934   0.10653574
#> lag1_var1 -0.035899386 0.05437397 -0.090618595   0.04332309
#> lag1_var2  1.134653872 0.07891101  1.039495941   1.24788682
#> lag2_var2 -0.124392122 0.03946238 -0.176879940  -0.07693150
#> lag2_var3 -0.008262904 0.04890240 -0.082582880   0.04067917
#> lag2_var3 -0.136977686 0.08034024 -0.253714603  -0.03462753
#> const      0.048040520 0.06439839 -0.031184377   0.13581605
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage         97.55953  158.33485    8.542918     351.7559
#> B[2,]_shrinkage        261.60074  103.91650  101.098677     380.0408
#> B[3,]_shrinkage       1130.64608  371.24013  697.649342    1692.4905
#> B[1,]_shrinkage_scale  652.47811  577.90064  118.152536    1606.9877
#> B[2,]_shrinkage_scale  986.50780  565.35299  371.045645    1692.7210
#> B[3,]_shrinkage_scale 1428.18465 1126.57670  451.794872    3220.4208
#> B_global_scale          92.34489   60.12212   28.850851     177.6572
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.7569184 0.2890363   0.4266861     1.177235
#> A[2,]_shrinkage       0.9182613 0.2549895   0.6977958     1.346570
#> A[3,]_shrinkage       0.5292756 0.4147356   0.1801503     1.199330
#> A[1,]_shrinkage_scale 9.3813627 3.4081166   5.9640327    14.829617
#> A[2,]_shrinkage_scale 9.2997683 2.0664888   6.5275543    11.667274
#> A[3,]_shrinkage_scale 7.5919885 3.6703988   3.3920867    13.409561
#> A_global_scale        0.9869398 0.1813606   0.7153212     1.197391
#> 
#> 
```
