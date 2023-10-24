
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bsvars

Bayesian Estimation of Structural Vector Autoregressive Models

This package provides efficient algorithms for Bayesian estimation of
Structural Vector Autoregressive (SVAR) models via Markov chain Monte
Carlo methods. A wide range of SVAR models is considered, including
homo- and heteroskedastic specifications and those with non-normal
structural shocks.

# Installation

Just open your **R** and type:

    install.packages("bsvars")

The package is under intensive development. To the the newest changes
install it by typing:

    devtools::install_git("https://github.com/bsvars/bsvars.git")

# Start your Bayesian analysis of data

The beginnings are as easy as ABC:

``` r
# upload the package
library(bsvars)

# upload data
data(us_fiscal_lsuw)
 
# specify the model and set seed
specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
set.seed(123)
 
# run the burn-in
burn_in        = estimate(specification, 1000)

# estimate the model
posterior      = estimate(burn_in, 50000, thin = 10)

# Now, you're ready to analyse your model!
```

Starting from **bsvars** version 2.0.0 a simplified workflow using the
`|>` is possible:

``` r
library(bsvars)
set.seed(123)
data(us_fiscal_lsuw)
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 4) |>
  estimate(S = 1000) |> 
  estimate(S = 50000) |> 
  compute_impulse_responses(horizon = 8) -> irfs
```

# Progress bar

MCMCs for multivariate dynamic structural models might take a while to
run. **bsvars** relies on efficient algorithms and fast codes developed
using **cpp** code via **Rcpp** and **RcppArmadillo** packages to cut
the time of computations by orders of magnitude. Still, while waiting
these a few minutes, you can track the progress by looking at the
beautiful progress bar:

    > estimate(specification, 1000)
    **************************************************|
    bsvars: Bayesian Structural Vector Autoregressions|
    **************************************************|
     Gibbs sampler for the SVAR-SV model              |
       Non-centred SV model is estimated             |
    **************************************************|
     Progress of the MCMC simulation for 1000 draws
        Every 10th draw is saved via MCMC thinning
     Press Esc to interrupt the computations
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    ******************

## License

This package is distributed under license GPL (\>= 3)

Copyright © 2022 Tomasz Woźniak (email: <wozniak.tom@pm.me>)

<!-- badges: start -->

[![R-CMD-check](https://github.com/bsvars/bsvars/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bsvars/bsvars/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
