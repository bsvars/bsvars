## Submission notes bsvars v2.1.0

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

Checking examples for two functions takes more time:
```
─  checking examples ... [38s/39s] OK (38.9s)
   Examples with CPU (user + system) or elapsed time > 5s
                                       user system elapsed
   verify_volatility                  9.237  0.081   9.319
   verify_volatility.PosteriorBSVARSV 9.219  0.063   9.282
```
This is the fastest check for this functions that is possible. The statistical 
methods for the evaluation of Numerical Standard Errors using subsampling require 
a minimum of 60 draws, which is implemented. These computations cannot go any 
faster.

## GitHub R-CMD-check using `usethis::use_github_action_check_standard()`

Passing on all platforms!

## Check at using `rhub::check_for_cran()`

