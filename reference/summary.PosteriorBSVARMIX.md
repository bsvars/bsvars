# Provides posterior summary of non-normal Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVARMIX'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVARMIX obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to non-normal Bayesian Structural VAR model
  specification set by function `specify_bsvar_mix$new()` containing
  draws from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar_mix`](https://bsvars.org/bsvars/reference/specify_bsvar_mix.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar_mix$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#> B[1,1] 0.9005433 0.04574206   0.8203742    0.9624406
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.24211 1.143443   -19.94457    -16.44809
#> B[2,2]  33.80342 2.114931    30.53473     36.82297
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -31.491697 2.662864  -35.009798   -27.944403
#> B[3,2] -16.279204 2.129902  -19.323706   -13.187279
#> B[3,3]   6.180156 0.399678    5.546039     6.719512
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.95669529 0.05287017  0.87357150  1.014508486
#> lag1_var2 -0.02020603 0.01758662 -0.04275015  0.007203617
#> lag1_var3 -0.08754558 0.07268735 -0.16416752  0.028856275
#> const     -0.16177188 0.12971447 -0.34538052  0.002610843
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02432531 0.02315977 -0.04911686  0.009815769
#> lag1_var2  0.95328875 0.01135388  0.93684190  0.969055663
#> lag1_var3 -0.04196492 0.03219588 -0.08835816 -0.009183178
#> const     -0.41505636 0.10263745 -0.59575575 -0.300690131
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.02339197 0.32430766  -0.5049411   0.45510725
#> lag1_var2 -0.06357084 0.06049177  -0.1426047   0.02768859
#> lag1_var3  0.04382255 0.43710530  -0.5068822   0.77822603
#> const     -0.42557601 0.60560350  -1.3871623   0.23395753
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        138.4687  72.25939    58.72117     288.3024
#> B[2,]_shrinkage        263.5317 130.50338   134.83879     435.1622
#> B[3,]_shrinkage        249.0862 131.97925   115.86525     553.9159
#> B[1,]_shrinkage_scale 1391.2338 550.75569   499.93634    2169.3098
#> B[2,]_shrinkage_scale 1621.1281 665.42683   648.90515    2565.6762
#> B[3,]_shrinkage_scale 1698.4291 835.92325   726.36554    3371.3704
#> B_global_scale         156.8093  60.58783    64.44263     247.4316
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6857877 0.4887629   0.2772815     1.590642
#> A[2,]_shrinkage       0.7544812 0.4553286   0.2623970     1.493287
#> A[3,]_shrinkage       0.8514122 0.3946918   0.4014989     1.430982
#> A[1,]_shrinkage_scale 7.8097681 2.5587499   4.6698253    12.682484
#> A[2,]_shrinkage_scale 8.7308486 2.8321626   4.9677743    13.188784
#> A[3,]_shrinkage_scale 9.7150392 2.5907258   5.6873462    13.301033
#> A_global_scale        0.8991397 0.1600903   0.6502228     1.134657
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar_mix$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
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
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR-finiteMIX model             |
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
#>             mean         sd 5% quantile 95% quantile
#> B[1,1] 0.9005433 0.04574206   0.8203742    0.9624406
#> 
#> $B$equation2
#>             mean       sd 5% quantile 95% quantile
#> B[2,1] -18.24211 1.143443   -19.94457    -16.44809
#> B[2,2]  33.80342 2.114931    30.53473     36.82297
#> 
#> $B$equation3
#>              mean       sd 5% quantile 95% quantile
#> B[3,1] -31.491697 2.662864  -35.009798   -27.944403
#> B[3,2] -16.279204 2.129902  -19.323706   -13.187279
#> B[3,3]   6.180156 0.399678    5.546039     6.719512
#> 
#> 
#> $A
#> $A$equation1
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.95669529 0.05287017  0.87357150  1.014508486
#> lag1_var2 -0.02020603 0.01758662 -0.04275015  0.007203617
#> lag1_var3 -0.08754558 0.07268735 -0.16416752  0.028856275
#> const     -0.16177188 0.12971447 -0.34538052  0.002610843
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02432531 0.02315977 -0.04911686  0.009815769
#> lag1_var2  0.95328875 0.01135388  0.93684190  0.969055663
#> lag1_var3 -0.04196492 0.03219588 -0.08835816 -0.009183178
#> const     -0.41505636 0.10263745 -0.59575575 -0.300690131
#> 
#> $A$equation3
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1  0.02339197 0.32430766  -0.5049411   0.45510725
#> lag1_var2 -0.06357084 0.06049177  -0.1426047   0.02768859
#> lag1_var3  0.04382255 0.43710530  -0.5068822   0.77822603
#> const     -0.42557601 0.60560350  -1.3871623   0.23395753
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        138.4687  72.25939    58.72117     288.3024
#> B[2,]_shrinkage        263.5317 130.50338   134.83879     435.1622
#> B[3,]_shrinkage        249.0862 131.97925   115.86525     553.9159
#> B[1,]_shrinkage_scale 1391.2338 550.75569   499.93634    2169.3098
#> B[2,]_shrinkage_scale 1621.1281 665.42683   648.90515    2565.6762
#> B[3,]_shrinkage_scale 1698.4291 835.92325   726.36554    3371.3704
#> B_global_scale         156.8093  60.58783    64.44263     247.4316
#> 
#> $hyper$A
#>                            mean        sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.6857877 0.4887629   0.2772815     1.590642
#> A[2,]_shrinkage       0.7544812 0.4553286   0.2623970     1.493287
#> A[3,]_shrinkage       0.8514122 0.3946918   0.4014989     1.430982
#> A[1,]_shrinkage_scale 7.8097681 2.5587499   4.6698253    12.682484
#> A[2,]_shrinkage_scale 8.7308486 2.8321626   4.9677743    13.188784
#> A[3,]_shrinkage_scale 9.7150392 2.5907258   5.6873462    13.301033
#> A_global_scale        0.8991397 0.1600903   0.6502228     1.134657
#> 
#> 
```
