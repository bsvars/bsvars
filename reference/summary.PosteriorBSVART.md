# Provides posterior summary of Structural VAR with t-distributed shocks estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, hyper-parameters, and Student-t
degrees-of-freedom parameter \\\nu\\.

## Usage

``` r
# S3 method for class 'PosteriorBSVART'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVART obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, hyper-parameters, and Student-t
degrees-of-freedom parameter \\\nu\\.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_t`](https://bsvars.org/bsvars/reference/specify_bsvar_t.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 6.986407 0.4052046    6.420688     7.618239
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  8.885183 0.7445038    7.611896     10.13394
#> B[2,2] 37.278867 2.1220324   34.552106     41.15462
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -38.240986 2.224386  -42.275893   -34.985661
#> B[3,2]   5.112737 2.017142    1.783077     7.938718
#> B[3,3]  52.215967 5.452650   46.378660    62.969512
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  1.1476675 0.04936589   1.0889652    1.2155825
#> lag1_var2 -0.4327333 0.02492828  -0.4625983   -0.3970032
#> lag1_var3  0.2024447 0.04525206   0.1369656    0.2545805
#> const     -1.3510795 0.10817354  -1.4976721   -1.1960780
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.06196858 0.01965873 -0.08501252  -0.03456420
#> lag1_var2  1.07492176 0.01416584  1.05430559   1.09820183
#> lag1_var3 -0.01284856 0.01974155 -0.04642904   0.01365825
#> const      0.05690747 0.08047455 -0.08095569   0.18635901
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1412015 0.04791534  0.09002244    0.2064528
#> lag1_var2 -0.3293734 0.04280591 -0.38673485   -0.2660267
#> lag1_var3  1.1231951 0.03027592  1.07800762    1.1609475
#> const     -0.9892129 0.15763814 -1.20079428   -0.7351580
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        317.8088  434.6918    67.01761    1270.6095
#> B[2,]_shrinkage        522.5644  268.8648   209.62584     875.1176
#> B[3,]_shrinkage        783.4207  640.1287   277.01265    1729.4982
#> B[1,]_shrinkage_scale 2708.5264 2758.7618   757.45696    6907.9008
#> B[2,]_shrinkage_scale 3026.6377 1795.1872  1271.35982    5752.7904
#> B[3,]_shrinkage_scale 3184.0733 2009.3272  1096.90125    7226.8148
#> B_global_scale         262.2633  167.7620   102.36156     543.6014
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6921954 0.3049717   0.4030267     1.052482
#> A[2,]_shrinkage       0.6405619 0.3271096   0.2787772     1.178866
#> A[3,]_shrinkage       0.7341404 0.4513075   0.2943205     1.512583
#> A[1,]_shrinkage_scale 6.9300517 1.8613776   4.6184871     9.728177
#> A[2,]_shrinkage_scale 7.1976699 2.9985865   4.1985243    12.974542
#> A[3,]_shrinkage_scale 6.6863261 2.0059668   4.2590780     9.833420
#> A_global_scale        0.7884900 0.1589324   0.5820656     1.009595
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>            3            0            3            3 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_t$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#>     with t-distributed structural skocks          |
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
#>            mean        sd 5% quantile 95% quantile
#> B[1,1] 6.986407 0.4052046    6.420688     7.618239
#> 
#> $B$equation2
#>             mean        sd 5% quantile 95% quantile
#> B[2,1]  8.885183 0.7445038    7.611896     10.13394
#> B[2,2] 37.278867 2.1220324   34.552106     41.15462
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -38.240986 2.224386  -42.275893   -34.985661
#> B[3,2]   5.112737 2.017142    1.783077     7.938718
#> B[3,3]  52.215967 5.452650   46.378660    62.969512
#> 
#> 
#> $A
#> $A$equation1
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  1.1476675 0.04936589   1.0889652    1.2155825
#> lag1_var2 -0.4327333 0.02492828  -0.4625983   -0.3970032
#> lag1_var3  0.2024447 0.04525206   0.1369656    0.2545805
#> const     -1.3510795 0.10817354  -1.4976721   -1.1960780
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.06196858 0.01965873 -0.08501252  -0.03456420
#> lag1_var2  1.07492176 0.01416584  1.05430559   1.09820183
#> lag1_var3 -0.01284856 0.01974155 -0.04642904   0.01365825
#> const      0.05690747 0.08047455 -0.08095569   0.18635901
#> 
#> $A$equation3
#>                 mean         sd 5% quantile 95% quantile
#> lag1_var1  0.1412015 0.04791534  0.09002244    0.2064528
#> lag1_var2 -0.3293734 0.04280591 -0.38673485   -0.2660267
#> lag1_var3  1.1231951 0.03027592  1.07800762    1.1609475
#> const     -0.9892129 0.15763814 -1.20079428   -0.7351580
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        317.8088  434.6918    67.01761    1270.6095
#> B[2,]_shrinkage        522.5644  268.8648   209.62584     875.1176
#> B[3,]_shrinkage        783.4207  640.1287   277.01265    1729.4982
#> B[1,]_shrinkage_scale 2708.5264 2758.7618   757.45696    6907.9008
#> B[2,]_shrinkage_scale 3026.6377 1795.1872  1271.35982    5752.7904
#> B[3,]_shrinkage_scale 3184.0733 2009.3272  1096.90125    7226.8148
#> B_global_scale         262.2633  167.7620   102.36156     543.6014
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6921954 0.3049717   0.4030267     1.052482
#> A[2,]_shrinkage       0.6405619 0.3271096   0.2787772     1.178866
#> A[3,]_shrinkage       0.7341404 0.4513075   0.2943205     1.512583
#> A[1,]_shrinkage_scale 6.9300517 1.8613776   4.6184871     9.728177
#> A[2,]_shrinkage_scale 7.1976699 2.9985865   4.1985243    12.974542
#> A[3,]_shrinkage_scale 6.6863261 2.0059668   4.2590780     9.833420
#> A_global_scale        0.7884900 0.1589324   0.5820656     1.009595
#> 
#> 
#> $df
#>         mean           sd  5% quantile 95% quantile 
#>            3            0            3            3 
#> 
```
