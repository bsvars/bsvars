# bsvars 3.0

The package is under intensive development, and more functionality will be provided soon! To see the package [ROADMAP](https://github.com/bsvars/bsvars/milestones) towards the next version.

Have a question, or suggestion, or wanna get in touch? Join the package [DISCUSSION](https://github.com/bsvars/bsvars/discussions) forum.

1. The package has a logo! And it's beautiful! [#37](https://github.com/bsvars/bsvars/issues/37)
2. The package includes `summary` methods [#1](https://github.com/bsvars/bsvars/issues/1)
3. The package includes `plot` methods [#36](https://github.com/bsvars/bsvars/issues/36)
4. Method `forecast` allow for conditional forecasting given provided future trajectories of selected variables [#76](https://github.com/bsvars/bsvars/issues/76)
5. Sparse mixture and Markov-switching models can now have more than 20 regimes [#57](https://github.com/bsvars/bsvars/issues/57)
6. A new, more detailed, package description [#62](https://github.com/bsvars/bsvars/issues/62)
7. The website features the new logo. And includes some new information [#38](https://github.com/bsvars/bsvars/issues/38)
8. Updates on documentation to accommodate the fact that some generics and functions from package **bsvars** will be used in a broader family of packages, first of which is [**bsvarSIGNs**](https://github.com/bsvars/bsvarSIGNs). Includes updates on references. [#63](https://github.com/bsvars/bsvars/issues/63)
9. Fixed `compute_fitted_values()`. Now it's correctly sampling from the predictive data density. [#67](https://github.com/bsvars/bsvars/issues/67)
10. Fixed some bugs that did not create problems [#55](https://github.com/bsvars/bsvars/issues/55)
11. Got rid of filling by reference in the samplers for the sake of granting the exported **cpp** functions usability [#56](https://github.com/bsvars/bsvars/issues/56)
12. Coded `compute_*()` functions as generics and methods [#70](https://github.com/bsvars/bsvars/issues/70)
13. Updated code for forecast error variance decompositions for heteroskedastic models (qas prompted by [@adamwang15](https://github.com/adamwang15)) [#69](https://github.com/bsvars/bsvars/issues/69)

# bsvars 2.1.0

Published on 11 December 2023

1. Included Bayesian procedure for verifying structural shocks' heteroskedastiicty equation-by-equation using Savage-Dickey density ratios [#26](https://github.com/bsvars/bsvars/issues/26)
2. Included Bayesian procedure for verifying joint hypotheses on autoregressive parameters using Savage-Dickey density ratios [#26](https://github.com/bsvars/bsvars/issues/26)
3. Included the possibility of specifying exogenous variables or deterministic terms and included the deterministic terms used by Lütkepohl, Shang, Uzeda, Woźniak (2023) [#45](https://github.com/bsvars/bsvars/issues/45)
4. Updated the data as in Lütkepohl, Shang, Uzeda, Woźniak (2023) [#45](https://github.com/bsvars/bsvars/issues/45)
5. Fixing the compilation problems reported [HERE](https://cran.r-project.org/web/checks/check_results_bsvars.html) [#48](https://github.com/bsvars/bsvars/issues/48)
6. The package has its pkgdown website at [bsvars.github.io/bsvars/](https://bsvars.github.io/bsvars/) [#38](https://github.com/bsvars/bsvars/issues/38)

# bsvars 2.0.0

Published on 23 October 2023

1.  Included Imports from package **stochvol**
2.  Posterior computations for:

-   impulse responses and forecast error variance decomposition [#3](https://github.com/bsvars/bsvars/issues/3),
-   structural shocks and historical decompositions [#14](https://github.com/bsvars/bsvars/issues/14)
-   fitted values [#17](https://github.com/bsvars/bsvars/issues/17)
-   conditional standard deviations [#16](https://github.com/bsvars/bsvars/issues/16)
-   regime probabilities for MS and MIX models [#18](https://github.com/bsvars/bsvars/issues/18)

3.  Implemented faster samplers based on random number generators from **armadillo** via **RcppArmadillo** [#7](https://github.com/bsvars/bsvars/issues/7)
4.  The `estimate_bsvar*` functions now also normalise the output w.r.t. to a structural matrix with positive elements on the main diagonal [#9](https://github.com/bsvars/bsvars/issues/9)
5.  Changed the order of arguments in the `estimate_bsvar*` functions with `posterior` first to facilitate workflows using the pipe `|>` [#10](https://github.com/bsvars/bsvars/issues/10)
6.  Include citation info for the package [#12](https://github.com/bsvars/bsvars/issues/12)
7.  Corrected sampler for AR parameter of the SV equations [#19](https://github.com/bsvars/bsvars/issues/19)
8.  Added samplers from joint predictive densities [#15](https://github.com/bsvars/bsvars/issues/15)
9.  A new centred Stochastic Volatility heteroskedastic process is implemented [#22](https://github.com/bsvars/bsvars/issues/22)
10. Introduced a three-level local-global equation-specific prior shrinkage hierarchy for the parameters of matrices \eqn{B} and \eqn{A} [#34](https://github.com/bsvars/bsvars/issues/34)
11. Improved checks for correct specification of arguments `S` and `thin` of the `estimate` method as enquired by [@mfaragd](https://github.com/mfaragd) [#33](https://github.com/bsvars/bsvars/issues/33)
12. Improved the ordinal numerals presentation for thinning in the progress bar [#27](https://github.com/bsvars/bsvars/issues/27)

# bsvars 1.0.0

Published on 1 September 2022

1.  repo transferred from GitLab to GitHub
2.  repository is made public
3.  version to be premiered on CRAN

# bsvars 0.0.2.9000

1.  Added a new progress bar for the `estimate_bsvar*` functions
2.  Developed **R6** classes for model specification and posterior outcomes; model specification includes sub-classes for priors, identifying restrictions, data matrices, and starting values
3.  Added a complete package documentation
4.  Written help files
5.  Developed tests for MCMC reproducibility
6.  Included sample data

# bsvars 0.0.1.9000

1.  **cpp** scripts are imported, compile, and give no Errors, Warnings, or Notes
2.  **R** wrappers for the functions are fully operating
3.  full documentation describing package and functions' functionality [sic!]
