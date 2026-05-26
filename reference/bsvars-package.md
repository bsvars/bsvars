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
#> 1 -9.809162 0.03095952   -9.857283    -9.769532
#> 2 -9.802370 0.03369145   -9.847244    -9.755254
#> 3 -9.803256 0.04951898   -9.870528    -9.731612
#> 4 -9.794460 0.05071412   -9.868565    -9.721842
#> 5 -9.790924 0.05829578   -9.857970    -9.698022
#> 6 -9.796566 0.05926620   -9.865445    -9.702189
#> 7 -9.763687 0.08336234   -9.854451    -9.632183
#> 8 -9.748332 0.07555445   -9.821000    -9.628020
#> 
#> $variable3
#>        mean         sd 5% quantile 95% quantile
#> 1 -7.047851 0.01870605   -7.074227    -7.024313
#> 2 -7.038772 0.02364922   -7.072683    -7.010407
#> 3 -7.027653 0.03436719   -7.079494    -6.986415
#> 4 -7.028843 0.03535703   -7.076588    -6.980119
#> 5 -7.022740 0.04212023   -7.078254    -6.960330
#> 6 -7.015837 0.03937045   -7.070493    -6.960394
#> 7 -7.011471 0.04550180   -7.073121    -6.951373
#> 8 -7.004730 0.03818321   -7.050699    -6.951307
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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.4351886 0.04057065    0.370738    0.4840737
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -39.14288 1.1761587   -40.49700    -37.48781
#> B[2,2]  12.54606 0.3788675    12.01247     12.97395
#> 
#> $B$equation3
#>             mean        sd 5% quantile 95% quantile
#> B[3,1] -12.95666 1.7824795   -15.73888    -10.87287
#> B[3,2] -20.63251 0.7801944   -21.52050    -19.59414
#> B[3,3] 106.15346 3.6645783   100.73551    110.12580
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  1.067168018 0.05985621   0.9751541    1.1243855
#> lag1_var1 -0.196572326 0.07859234  -0.3276933   -0.1134897
#> lag1_var2  0.000000000 0.00000000   0.0000000    0.0000000
#> lag2_var2 -0.234029269 0.05857678  -0.2913474   -0.1471790
#> lag2_var3  0.150783481 0.07927583   0.0582637    0.2788866
#> lag2_var3  0.000000000 0.00000000   0.0000000    0.0000000
#> const      0.002548191 0.09912756  -0.1118450    0.1505908
#> 
#> $A$equation2
#>                  mean        sd  5% quantile 95% quantile
#> lag1_var1  0.15794881 0.1119293  0.005259186    0.3211300
#> lag1_var1  0.71832695 0.1457735  0.545870901    0.9434932
#> lag1_var2 -0.86023862 0.4468234 -1.311460614   -0.3027990
#> lag2_var2 -0.48709036 0.1170731 -0.653981098   -0.3384955
#> lag2_var3  0.13502923 0.1485318 -0.093841940    0.3060830
#> lag2_var3  0.63246521 0.4716013  0.045213602    1.1176281
#> const      0.05104786 0.1527500 -0.192413114    0.2423451
#> 
#> $A$equation3
#>                  mean         sd  5% quantile 95% quantile
#> lag1_var1  0.03097169 0.03482675 -0.019909513   0.07741400
#> lag1_var1 -0.16625397 0.04050938 -0.215136560  -0.11274771
#> lag1_var2  1.00844120 0.13129849  0.855772059   1.18111226
#> lag2_var2 -0.11213837 0.03649688 -0.158592331  -0.05588535
#> lag2_var3  0.13518141 0.03869068  0.085699339   0.17955198
#> lag2_var3 -0.05820648 0.13525327 -0.237460029   0.09688619
#> const      0.03162670 0.02960544 -0.007351244   0.07763420
#> 
#> 
#> $hyper
#> $hyper$B
#>                             mean         sd 5% quantile 95% quantile
#> B[1,]_shrinkage         53.35959   39.57581    7.206179     106.5232
#> B[2,]_shrinkage        292.47790  134.50510  140.137099     499.4812
#> B[3,]_shrinkage       1158.31098  623.06406  580.300266    2145.3329
#> B[1,]_shrinkage_scale  701.76856  625.74159   78.959240    1667.1046
#> B[2,]_shrinkage_scale 1480.91614 1125.97759  456.280440    3280.7284
#> B[3,]_shrinkage_scale 1876.59100 1796.13177  612.847341    5206.9325
#> B_global_scale         114.43121   94.77739   31.564637     283.1928
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4784869 0.14175388   0.3287622    0.7213826
#> A[2,]_shrinkage       0.8468416 0.41076667   0.2487185    1.3904739
#> A[3,]_shrinkage       0.2122907 0.02921836   0.1751911    0.2457228
#> A[1,]_shrinkage_scale 5.7787479 1.40825683   4.1144553    7.8010695
#> A[2,]_shrinkage_scale 6.4618720 2.37242060   3.6503471   10.0403732
#> A[3,]_shrinkage_scale 3.6106190 1.04707584   2.1356315    5.0329282
#> A_global_scale        0.6186152 0.20334185   0.4281262    0.9789363
#> 
#> 
```
