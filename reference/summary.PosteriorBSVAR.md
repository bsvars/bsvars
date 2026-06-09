# Provides posterior summary of homoskedastic Structural VAR estimation

Provides posterior mean, standard deviations, as well as 5 and 95
percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper parameters.

## Usage

``` r
# S3 method for class 'PosteriorBSVAR'
summary(object, ...)
```

## Arguments

- object:

  an object of class PosteriorBSVAR obtained using the
  [`estimate()`](https://bsvars.org/bsvars/reference/estimate.md)
  function applied to homoskedastic Bayesian Structural VAR model
  specification set by function `specify_bsvar$new()` containing draws
  from the posterior distribution of the parameters.

- ...:

  additional arguments affecting the summary produced.

## Value

A list reporting the posterior mean, standard deviations, as well as 5
and 95 percentiles of the parameters: the structural matrix \\B\\,
autoregressive parameters \\A\\, and hyper-parameters.

## See also

[`estimate`](https://bsvars.org/bsvars/reference/estimate.md),
[`specify_bsvar`](https://bsvars.org/bsvars/reference/specify_bsvar.md)

## Author

Tomasz Woźniak <wozniak.tom@pm.me>

## Examples

``` r
# upload data
data(us_fiscal_lsuw)

# specify the model and set seed
set.seed(123)
specification  = specify_bsvar$new(us_fiscal_lsuw)
#> The identification is set to the default option of lower-triangular structural matrix.

# run the burn-in
burn_in        = estimate(specification, 10)
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
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
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 35.09588 1.686944    32.60351     37.48121
#> 
#> $B$equation2
#>              mean       sd 5% quantile 95% quantile
#> B[2,1] -0.1028203 2.565796   -2.789346     5.252764
#> B[2,2] 39.4936838 2.545994   36.414127    43.647928
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -14.2980661 1.532602  -16.768833    -12.05737
#> B[3,2]  -0.2501413 2.629023   -4.610455      3.39692
#> B[3,3]  97.4220490 4.249847   91.098013    103.00009
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.913398680 0.01679839  0.88815999   0.93957593
#> lag1_var2 -0.001027232 0.01422207 -0.01998918   0.01841525
#> lag1_var3  0.103631364 0.02346832  0.06937820   0.13147049
#> const     -0.043058966 0.10580799 -0.22013019   0.14085229
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02252694 0.02217486 -0.04814025   0.01829944
#> lag1_var2  0.95238376 0.01174687  0.93381567   0.97616006
#> lag1_var3  0.03295538 0.02871608 -0.01258597   0.07053000
#> const     -0.43677036 0.10332090 -0.60060236  -0.26884739
#> 
#> $A$equation3
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.017960694 0.007821574 -0.02641538 -0.006497867
#> lag1_var2 -0.004662448 0.004312109 -0.01030642  0.002116785
#> lag1_var3  1.020024669 0.010466417  1.00624043  1.032285960
#> const     -0.061224135 0.030759783 -0.09527021 -0.018315391
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        508.5376  352.7570   169.84064     1171.878
#> B[2,]_shrinkage        582.6517  651.6802   174.95082     2240.144
#> B[3,]_shrinkage       1115.3237  532.3208   521.39944     1924.157
#> B[1,]_shrinkage_scale 3514.8043 3966.1603   566.36648    10829.201
#> B[2,]_shrinkage_scale 4519.4759 5223.2397   563.08518    12036.201
#> B[3,]_shrinkage_scale 4332.5062 5099.1766   638.50374    13759.712
#> B_global_scale         364.5536  402.9807    39.86076     1148.559
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3385170 0.17698822  0.08581606    0.6219381
#> A[2,]_shrinkage       0.2848364 0.16713385  0.11335237    0.5611669
#> A[3,]_shrinkage       0.3237228 0.26792304  0.07360823    0.5715186
#> A[1,]_shrinkage_scale 3.9576003 1.46323260  1.56941879    5.8459596
#> A[2,]_shrinkage_scale 3.3051021 1.47918569  1.90384497    6.4437421
#> A[3,]_shrinkage_scale 3.5180074 1.92050729  1.26113427    6.8988395
#> A_global_scale        0.4550062 0.07662271  0.33770816    0.5882126
#> 
#> 

# workflow with the pipe |>
############################################################
set.seed(123)
us_fiscal_lsuw |>
  specify_bsvar$new() |>
  estimate(S = 10) |> 
  estimate(S = 20) |> 
  summary()
#> The identification is set to the default option of lower-triangular structural matrix.
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
#> **************************************************|
#>  Progress of the MCMC simulation for 10 draws
#>     Every draw is saved via MCMC thinning
#>  Press Esc to interrupt the computations
#> **************************************************|
#> **************************************************|
#> bsvars: Bayesian Structural Vector Autoregressions|
#> **************************************************|
#>  Gibbs sampler for the SVAR model                 |
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
#>            mean       sd 5% quantile 95% quantile
#> B[1,1] 35.09588 1.686944    32.60351     37.48121
#> 
#> $B$equation2
#>              mean       sd 5% quantile 95% quantile
#> B[2,1] -0.1028203 2.565796   -2.789346     5.252764
#> B[2,2] 39.4936838 2.545994   36.414127    43.647928
#> 
#> $B$equation3
#>               mean       sd 5% quantile 95% quantile
#> B[3,1] -14.2980661 1.532602  -16.768833    -12.05737
#> B[3,2]  -0.2501413 2.629023   -4.610455      3.39692
#> B[3,3]  97.4220490 4.249847   91.098013    103.00009
#> 
#> 
#> $A
#> $A$equation1
#>                   mean         sd 5% quantile 95% quantile
#> lag1_var1  0.913398680 0.01679839  0.88815999   0.93957593
#> lag1_var2 -0.001027232 0.01422207 -0.01998918   0.01841525
#> lag1_var3  0.103631364 0.02346832  0.06937820   0.13147049
#> const     -0.043058966 0.10580799 -0.22013019   0.14085229
#> 
#> $A$equation2
#>                  mean         sd 5% quantile 95% quantile
#> lag1_var1 -0.02252694 0.02217486 -0.04814025   0.01829944
#> lag1_var2  0.95238376 0.01174687  0.93381567   0.97616006
#> lag1_var3  0.03295538 0.02871608 -0.01258597   0.07053000
#> const     -0.43677036 0.10332090 -0.60060236  -0.26884739
#> 
#> $A$equation3
#>                   mean          sd 5% quantile 95% quantile
#> lag1_var1 -0.017960694 0.007821574 -0.02641538 -0.006497867
#> lag1_var2 -0.004662448 0.004312109 -0.01030642  0.002116785
#> lag1_var3  1.020024669 0.010466417  1.00624043  1.032285960
#> const     -0.061224135 0.030759783 -0.09527021 -0.018315391
#> 
#> 
#> $hyper
#> $hyper$B
#>                            mean        sd 5% quantile 95% quantile
#> B[1,]_shrinkage        508.5376  352.7570   169.84064     1171.878
#> B[2,]_shrinkage        582.6517  651.6802   174.95082     2240.144
#> B[3,]_shrinkage       1115.3237  532.3208   521.39944     1924.157
#> B[1,]_shrinkage_scale 3514.8043 3966.1603   566.36648    10829.201
#> B[2,]_shrinkage_scale 4519.4759 5223.2397   563.08518    12036.201
#> B[3,]_shrinkage_scale 4332.5062 5099.1766   638.50374    13759.712
#> B_global_scale         364.5536  402.9807    39.86076     1148.559
#> 
#> $hyper$A
#>                            mean         sd 5% quantile 95% quantile
#> A[1,]_shrinkage       0.3385170 0.17698822  0.08581606    0.6219381
#> A[2,]_shrinkage       0.2848364 0.16713385  0.11335237    0.5611669
#> A[3,]_shrinkage       0.3237228 0.26792304  0.07360823    0.5715186
#> A[1,]_shrinkage_scale 3.9576003 1.46323260  1.56941879    5.8459596
#> A[2,]_shrinkage_scale 3.3051021 1.47918569  1.90384497    6.4437421
#> A[3,]_shrinkage_scale 3.5180074 1.92050729  1.26113427    6.8988395
#> A_global_scale        0.4550062 0.07662271  0.33770816    0.5882126
#> 
#> 
```
