# Bayesian Estimation of Structural Vector Autoregressive Models

Provides fast and efficient procedures for Bayesian analysis of
Structural Vector Autoregressions. This package estimates a wide range
of models, including homo-, heteroskedastic, and non-normal
specifications. Structural models can be identified by adjustable
exclusion restrictions, time-varying volatility, or non-normality, and
include exclusion restrictions on autoregressive parameters. They all
include a flexible three-level equation-specific local-global
hierarchical prior distribution for the estimated level of shrinkage for
autoregressive and structural parameters. Additionally, the package
facilitates predictive and structural analyses such as impulse
responses, forecast error variance and historical decompositions,
forecasting, verification of heteroskedasticity, non-normality, and
hypotheses on autoregressive parameters, as well as analyses of
structural shocks, volatilities, and fitted values. Beautiful plots,
informative summary functions, and extensive documentation including the
vignette by Woźniak (2025) \<doi:10.48550/arXiv.2410.15090\> complement
all this. The implemented techniques align closely with those presented
in Lütkepohl, Shang, Uzeda, & Woźniak (2025)
\<doi:10.1016/j.jeconom.2025.106107\>, Lütkepohl & Woźniak (2020)
\<doi:10.1016/j.jedc.2020.103862\>, and Song & Woźniak (2021)
\<doi:10.1093/acrefore/9780190625979.013.174\> and they embed many
popular models proposed by other authors. The 'bsvars' package is
aligned regarding objects, workflows, and code structure with the R
packages 'bsvarSIGNs' by Wang & Woźniak (2025)
\<doi:10.32614/CRAN.package.bsvarSIGNs\>, 'bvars' by Liu, Ramirez
Hassan, Woźniak (2026) \<doi:10.32614/CRAN.package.bvars\>, and 'bpvars'
by Woźniak (2026) \<doi:10.32614/CRAN.package.bpvars\>, and they
constitute an integrated toolset.

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

Liu, Ramirez Hassan, Woźniak (2026) bvars: Bayesian Forecasting with
Large Vector Autoregressions. R package version 1.0,
[doi:10.32614/CRAN.package.bvars](https://doi.org/10.32614/CRAN.package.bvars)
.

Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2025) Partial
identification of structural vector autoregressions with non-centred
stochastic volatility. *Journal of Econometrics* **256**, 106107,
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

Wang X, Woźniak T (2025). bsvarSIGNs: Bayesian SVARs with Sign, Zero,
and Narrative Restrictions. R package version 2.0,
[doi:10.32614/CRAN.package.bsvarSIGNs](https://doi.org/10.32614/CRAN.package.bsvarSIGNs)
.

Woźniak T (2026) bpvars: Forecasting with Bayesian Panel Vector
Autoregressions. R package version 2.0,
[doi:10.32614/CRAN.package.bpvars](https://doi.org/10.32614/CRAN.package.bpvars)
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
post  = estimate(burn, 5)             # estimate the model
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
  estimate(S = 5) |> 
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
#>  Progress of the MCMC simulation for 5 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# conditional forecasting using a model with exogenous variables
############################################################
us_fiscal_lsuw |>
  specify_bsvar_sv$new(exogenous = us_fiscal_ex) |>
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
  
 post |> forecast(
    horizon = 8,
    exogenous_forecast = us_fiscal_ex_forecasts,
    conditional_forecast = us_fiscal_cond_forecasts
  ) -> pred
  
  pred |> summary()
#> $ttr
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
#> $gs
#>        mean         sd 5% quantile 95% quantile
#> 1 -9.795046 0.02018143   -9.813070    -9.768839
#> 2 -9.775744 0.02007064   -9.793271    -9.749397
#> 3 -9.753439 0.01843346   -9.771690    -9.734142
#> 4 -9.711736 0.05399605   -9.755961    -9.640522
#> 5 -9.725097 0.05918957   -9.782110    -9.648543
#> 6 -9.715166 0.06904580   -9.785427    -9.630191
#> 7 -9.709981 0.07141973   -9.775264    -9.617198
#> 8 -9.693611 0.06552284   -9.762870    -9.620182
#> 
#> $gdp
#>        mean          sd 5% quantile 95% quantile
#> 1 -7.020589 0.013086551   -7.033896    -7.006255
#> 2 -7.010218 0.009285931   -7.017449    -6.997636
#> 3 -6.997427 0.014863412   -7.011668    -6.979938
#> 4 -6.987638 0.042965323   -7.036599    -6.944936
#> 5 -6.984943 0.048662142   -7.038135    -6.933244
#> 6 -6.967975 0.043256595   -7.022512    -6.924784
#> 7 -6.969550 0.042334420   -7.022054    -6.924244
#> 8 -6.972120 0.034935348   -7.012077    -6.933536
#> 
  pred |> plot(probability = 0.68)

  
# estimation of a model with exogeneity restrictions on the  autoregressive matrix
#############################################################
A = matrix(TRUE, 3, 7)
A[1,3] = A[1,6] = FALSE
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 2, A = A) |>
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
post |> summary()
#> $B
#> $B$ttr
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.8346538 0.07687118   0.7398016    0.9156767
#> 
#> $B$gs
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -31.45359 2.089851   -33.66528    -28.85055
#> B[2,2]  26.66406 1.749549    24.47512     28.50770
#> 
#> $B$gdp
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -20.76785 2.539802   -23.14730    -17.62516
#> B[3,2] -22.22550 2.592977   -24.22704    -18.67935
#> B[3,3] 108.62362 6.903294   102.50042    117.00696
#> 
#> 
#> $A
#> $A$ttr
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.829590825 0.04874510  0.76470654  0.873327603
#> lag1_var1  0.006463362 0.02485060 -0.02210537  0.033342975
#> lag1_var2  0.000000000 0.00000000  0.00000000  0.000000000
#> lag2_var2  0.095597048 0.04614471  0.05636030  0.157759887
#> lag2_var3 -0.014666611 0.01983576 -0.03653665  0.009014286
#> lag2_var3  0.000000000 0.00000000  0.00000000  0.000000000
#> const      0.336823569 0.06045972  0.27629692  0.414101440
#> 
#> $A$gs
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.2801029 0.07446388  -0.3716875  -0.20007020
#> lag1_var1  1.3268190 0.06659880   1.2628576   1.41291518
#> lag1_var2 -0.6032236 0.03210738  -0.6423815  -0.56961184
#> lag2_var2  0.1811386 0.06641211   0.1179741   0.26651610
#> lag2_var3 -0.3953958 0.06966104  -0.4843496  -0.32806994
#> lag2_var3  0.6245727 0.05388911   0.5660034   0.69104652
#> const     -0.1312019 0.08800741  -0.2013470  -0.02332273
#> 
#> $A$gdp
#>                   mean         sd 5% quantile  95% quantile
#> lag1_var1 -0.087348221 0.02566571 -0.11504785 -0.0559587339
#> lag1_var1 -0.018200073 0.01519370 -0.03466294  0.0002882999
#> lag1_var2  0.931914474 0.01285662  0.91869875  0.9468942783
#> lag2_var2  0.049632163 0.02253587  0.02460215  0.0769137981
#> lag2_var3  0.006141407 0.01422308 -0.01138137  0.0207022722
#> lag2_var3  0.071946615 0.01080887  0.05964428  0.0809977393
#> const      0.041689367 0.01517868  0.02505657  0.0600948551
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         55.99619  22.06188    28.30311     78.56033
#> B[2,]_shrinkage        289.76221 131.34046   115.52387    408.24265
#> B[3,]_shrinkage       1083.61981 585.07643   573.44744   1757.87404
#> B[1,]_shrinkage_scale  592.61653 220.08703   305.25507    784.90482
#> B[2,]_shrinkage_scale  993.48664 724.54559   369.23744   1921.83306
#> B[3,]_shrinkage_scale  852.89247 454.34567   381.01954   1355.99249
#> B_global_scale          66.42888  39.13224    29.83294    116.37861
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3805089 0.16880986   0.2045192    0.5680707
#> A[2,]_shrinkage       0.4483301 0.16488916   0.2779497    0.6521025
#> A[3,]_shrinkage       0.3598296 0.05849243   0.3003801    0.4217098
#> A[1,]_shrinkage_scale 5.0663257 2.04398589   2.9585895    7.5745066
#> A[2,]_shrinkage_scale 5.7753529 1.94329888   4.2055002    8.3958837
#> A[3,]_shrinkage_scale 5.2341099 1.12602936   3.8418290    6.1934480
#> A_global_scale        0.6099824 0.17430380   0.4181814    0.8093451
#> 
#> 
```
