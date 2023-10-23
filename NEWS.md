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
