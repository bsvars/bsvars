# Provides posterior summary of heteroskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARHMSH'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARHMSH obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to heteroskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar_hmsh$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_hmsh`](https://bsvars.org/bsvars/reference/specify_bsvar_hmsh.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# specify the model
specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|

# estimate the model
posterior      = estimate(burn_in, 20)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
summary(posterior)
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.1789118 0.01026957     0.16584    0.1956653
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1] -12.80111 0.7596915   -13.93354    -11.72838
#> B[2,2]  36.58827 2.1786253    33.48524     39.88546
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -35.05038 2.747875   -39.32122   -30.578274
#> B[3,2] -11.36674 2.432427   -14.18584    -6.168833
#> B[3,3]  32.49969 2.407969    28.30683    35.652735
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.90967770 0.02470993  0.87345084   0.94882801
#> lag1_var2  0.05947905 0.01319996  0.03763011   0.08004295
#> lag1_var3 -0.64438324 0.03229218 -0.69105834  -0.59660998
#> const      0.40567035 0.13312324  0.18951413   0.60606981
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.03772228 0.01422437 -0.05730036 -0.015564699
#> lag1_var2  0.98628489 0.01173195  0.96804445  1.004482365
#> lag1_var3 -0.21295977 0.01944843 -0.24338374 -0.187160526
#> const     -0.16600216 0.10109203 -0.32716464  0.005264117
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.06004635 0.03155411 -0.10703957  -0.01583163
#> lag1_var2  0.08062220 0.01737031  0.05938165   0.10687792
#> lag1_var3  0.16630617 0.03983860  0.11269654   0.22450562
#> const      0.58227678 0.18201377  0.31233635   0.84345472
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        242.8374  226.1929    54.28151     684.0468
#> B[2,]_shrinkage        401.5682  211.4464   125.12740     875.1486
#> B[3,]_shrinkage        479.3213  255.3951   223.42127     720.8592
#> B[1,]_shrinkage_scale 2173.6817 1485.0640   658.59455    5280.8991
#> B[2,]_shrinkage_scale 2359.6650 1150.4442   430.68283    4020.0706
#> B[3,]_shrinkage_scale 2354.1346 1149.9699   691.52993    4122.3042
#> B_global_scale         219.6093  118.6131    51.13306     393.2670
#> 
#> $hyper$A
#>                             mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage        0.8336312 0.5598849   0.2305352     1.882102
#> A[2,]_shrinkage        0.9051685 0.4972156   0.4084361     1.968304
#> A[3,]_shrinkage        0.9279852 0.5443009   0.3962073     1.589719
#> A[1,]_shrinkage_scale  9.8673266 4.2554176   4.6648876    15.338829
#> A[2,]_shrinkage_scale  8.9499736 3.1738665   4.9755867    12.948863
#> A[3,]_shrinkage_scale 10.1148751 4.0364750   6.1147441    17.496745
#> A_global_scale         1.0648859 0.3649887   0.6717201     1.666197
#> 
#> 

# workflow with the pipe |>
############################################################
us_fiscal_lsuw |>
  specify_bsvar_msh$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-stationaryMSH model             |
#> **************************************************|
#>  Progress of the MCMC simulation for 20 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#>  **************************************************|
#>  bsvars: Bayesian Structural Vector Autoregressions|
#>  **************************************************|
#>    Posterior summary of the parameters             |
#>  **************************************************|
#> $B
#> $B$equation1
#>             mean          sd 5% quantile 95% quantile
#> B[1,1] 0.1742572 0.008052961    0.163317    0.1847464
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -28.79196 2.099190   -31.72750    -25.81905
#> B[2,2]  20.14358 1.466298    18.06149     22.20197
#> 
#> $B$equation3
#>             mean       sd 5% quantile 95% quantile
#> B[3,1] -23.75916 2.450360   -26.74439    -19.15958
#> B[3,2] -16.85869 2.025095   -19.80842    -14.48747
#> B[3,3]  80.05973 6.185880    71.47901     89.46156
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.8503572 0.03195041   0.8154927   0.89857221
#> lag1_var2 -0.0978526 0.01222486  -0.1147526  -0.08321204
#> lag1_var3 -0.4919924 0.02990705  -0.5371570  -0.44825117
#> const     -0.4885056 0.07237556  -0.5756083  -0.36110405
#> 
#> $A$equation2
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.1844938 0.04710660  -0.2560951   -0.1170638
#> lag1_var2  0.8385829 0.01828155   0.8181495    0.8745587
#> lag1_var3 -0.7380005 0.04832168  -0.8016110   -0.6635635
#> const     -0.9037754 0.14576084  -1.0944933   -0.6842441
#> 
#> $A$equation3
#>                  mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.07105554 0.020033092 -0.09678818  -0.04185277
#> lag1_var2 -0.05760734 0.006697735 -0.06578254  -0.04657526
#> lag1_var3  0.68196028 0.019932988  0.65591574   0.71125050
#> const     -0.28871393 0.046611167 -0.38002158  -0.22174593
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage         79.7411  55.66573    32.71952     163.4945
#> B[2,]_shrinkage        239.4680 190.62081    78.37800     744.8698
#> B[3,]_shrinkage        712.0784 258.90711   373.46651    1204.8042
#> B[1,]_shrinkage_scale  794.9707 554.73510   310.91795    2192.7775
#> B[2,]_shrinkage_scale 1207.6374 975.88922   387.70936    2416.6290
#> B[3,]_shrinkage_scale 1476.5169 987.20345   422.50974    3005.1800
#> B_global_scale         105.3997  79.46956    36.68018     206.8365
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.4397922 0.2084074   0.2735061    0.7395639
#> A[2,]_shrinkage       0.4840634 0.2405029   0.2344259    0.9731473
#> A[3,]_shrinkage       0.4169423 0.2254810   0.1770931    0.8859068
#> A[1,]_shrinkage_scale 5.3253607 2.1127753   3.3825122    8.9218217
#> A[2,]_shrinkage_scale 5.0924967 1.7833864   2.8281688    7.5836758
#> A[3,]_shrinkage_scale 4.6736211 2.0250521   2.3775768    7.8608095
#> A_global_scale        0.5299876 0.1407534   0.3845435    0.8109238
#> 
#> 
```
