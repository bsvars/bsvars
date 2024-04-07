
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bsvars <img src="man/figures/logo.svg" align="right" padding-left="3px" alt="bsvars website" />

An **R** package for Bayesian Estimation of Structural Vector
Autoregressive Models

<!-- badges: start -->

[![CRAN
version](http://www.r-pkg.org/badges/version/bsvars)](https://cran.r-project.org/package=bsvars)
[![R-CMD-check](https://github.com/bsvars/bsvars/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bsvars/bsvars/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Provides fast and efficient procedures for Bayesian analysis of
Structural Vector Autoregressions. This package estimates a wide range
of models, including homo-, heteroskedastic and non-normal
specifications. Structural models can be identified by adjustable
exclusion restrictions, time-varying volatility, or non-normality. They
all include a flexible three-level equation-specific local-global
hierarchical prior distribution for the estimated level of shrinkage for
autoregressive and structural parameters. Additionally, the package
facilitates predictive and structural analyses such as impulse
responses, forecast error variance and historical decompositions,
forecasting, verification of heteroskedasticity and hypotheses on
autoregressive parameters, and analyses of structural shocks,
volatilities, and fitted values. Beautiful plots, informative summary
functions, and extensive documentation complement all this. The
implemented techniques align closely with those presented in [Lütkepohl
& Woźniak (2020)](http://doi.org/10.1016/j.jedc.2020.103862), Lütkepohl,
Shang, Uzeda, & Woźniak (2024), [Song & Woźniak
(2021)](https://doi.org/10.1093/acrefore/9780190625979.013.174), and
[Woźniak & Droumaguet
(2015)](http://doi.org/10.13140/RG.2.2.19492.55687).

## Features

#### Structural Vector Autoregressions

- All the models in the **bsvars** package consist of the Vector
  Autoregressive equation, with autoregressive parameters `A` and error
  terms `E`, and the structural equation with structural shocks `U`

<!-- -->

        Y = AX + E           (VAR equation)
       BE = U                (structural equation)

- The models are identified via exclusion restrictions,
  heteroskedasticity, or non-normality
- Autoregressive parameters `A` and the structural matrix `B` are
  feature a three-level local-global hierarchical prior that estimates
  the equation-specific level of shrinkage
- In **five models** the structural shocks are conditionally normal with
  zero mean and diagonal covariance matrix with variances that are:
  - equal to one
  - time-varying following non-centred **Stochastic Volatility**
  - time-varying following centred **Stochastic Volatility**
  - time-varying with stationary **Markov Switching**
  - time-varying with **sparse Markov Switching** where the number of
    volatility regimes is estimated
- In **two more models** non-normal structural shocks following
  - a finite **mixture of normal** components and component-specific
    variances
  - a **sparse mixture of normal** components and component-specific
    variances where the number of states is estimated

#### Simple workflows

- Specify the models using `specify_bsvar_*()` functions, for instance,
  `specify_bsvar()`
- Estimate the models using the `estimate()` method
- Predict the future using the `predict()` method
- Provide structural analyses using **impulse responses**, forecast
  error variance decompositions, historical decompositions, and
  structural shocks using functions `compute_impulse_responses()`,
  `compute_variance_decompositions()`,
  `compute_historical_decompositions()`, and
  `compute_structural_shocks()` respectively
- Analyse the fitted values, time-varying volatility, and volatility
  regimes using functions `compute_fitted_values()`,
  `compute_conditional_sd()`, and `compute_regime_probabilities()`
  respectively

#### Fast and efficient computations

- Extraordinary computational speed is obtained by combining
  - the implementation of frontier econometric techniques, and
  - compiled code written in **cpp**
- It combines the best of two worlds: the ease of data analysis with
  **R** and fast **cpp** algorithms
- The algorithms used here are very fast. But still, Bayesian estimation
  might take a little time. Look at our beautiful **progress bar** in
  the meantime:

<!-- -->

    **************************************************|
    bsvars: Bayesian Structural Vector Autoregressions|
    **************************************************|
     Gibbs sampler for the SVAR-SV model              |
       Non-centred SV model is estimated              |
    **************************************************|
     Progress of the MCMC simulation for 1000 draws
        Every 10th draw is saved via MCMC thinning
     Press Esc to interrupt the computations
    **************************************************|
    0%   10   20   30   40   50   60   70   80   90   100%
    [----|----|----|----|----|----|----|----|----|----|
    *************************************

#### The hexagonal logo

This beautiful logo can be reproduced in R using [this
file](https://github.com/bsvars/bsvars/blob/master/inst/varia/bsvars_logo.R).

<p>
</p>
<a href="https://bsvars.github.io/bsvars/"><img src="man/figures/logo.png" height="400" alt="bsvars website" /></a>
<p>
</p>

## Start your Bayesian analysis of data

The beginnings are as easy as ABC:

``` r
library(bsvars)                       # upload the package
data(us_fiscal_lsuw)                  # upload data
spec      = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)     # specify the model
burn_in   = estimate(spec, 1000)      # run the burn-in
out       = estimate(burn_in, 50000)  # estimate the model
```

Starting from **bsvars** version 2.0.0 a simplified workflow using the
`|>` pipe is possible:

``` r
library(bsvars)                       # upload the package
data(us_fiscal_lsuw)                  # upload data
us_fiscal_lsuw |>
  specify_bsvar_sv$new(p = 4) |>      # specify the model
  estimate(S = 1000) |>               # run the burn-in
  estimate(S = 50000) -> out          # estimate the model
```

Now, you’re ready to analyse your model!

## Installation

#### The first time you install the package

You must have a **cpp** compiler. Follow the instructions from [Section
1.3. by Eddelbuettel & François
(2023)](https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-FAQ.pdf).
In short:

**Windows:** install
[RTools](https://cran.r-project.org/bin/windows/Rtools/).

**macOS:** install [Xcode Command Line
Tools](https://www.freecodecamp.org/news/install-xcode-command-line-tools/).

**Linux:** install the standard developement packages.

#### Once that’s done:

Just open your **R** and type:

    install.packages("bsvars")

The developer’s version of the package with the newest features can be
installed by typing:

    devtools::install_git("https://github.com/bsvars/bsvars.git")

## Development

The package is under intensive development. Your help is welcome!
Please, have a look at the
[roadmap](https://github.com/bsvars/bsvars/milestone/3),
[discuss](https://github.com/bsvars/bsvars/discussions) package features
and applications, or [report a
bug](https://github.com/bsvars/bsvars/issues). Thank you!

## About the author

Tomasz is a Bayesian econometrician and a Senior Lecturer at the
University of Melbourne. He develops methodology for empirical
macroeconomic analyses and programs in **R** and **cpp** using **Rcpp**.

<a href="mailto:twozniak@unimelb.edu.au">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/envelope.svg" width="40" height="40"/>
</a> <a href="https://github.com/donotdespair">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/github.svg" width="40" height="40"/>
</a> <a href="https://gitlab.com/tomaszwozniak">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/gitlab.svg" width="40" height="40"/>
</a> <a href="https://orcid.org/0000-0003-2212-2378">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/orcid.svg" width="40" height="40"/>
</a> <a href="http://scholar.google.com/citations?user=2uWpFrYAAAAJ&hl">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/google.svg" width="40" height="40"/>
</a> <a href="https://arxiv.org/a/wozniak_t_1">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/circle-xmark.svg" width="40" height="40"/>
</a>
<a href="https://www.linkedin.com/in/tomasz-wo%C5%BAniak-7b85361b1">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/linkedin.svg" width="40" height="40"/>
</a> <a href="https://fosstodon.org/@tomaszwozniak">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/brands/mastodon.svg" width="50" height="50"/>
</a> <a href="https://bsky.app/profile/tomaszwozniak.bsky.social">
<img src="https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/cloud.svg" width="50" height="50"/>
</a>
