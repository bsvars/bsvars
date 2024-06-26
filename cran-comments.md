## Submission notes bsvars v3.0.1

## This version corrects for `Last released version's CRAN status` under: Additional issues clang-ASAN gcc-ASAN valgrind 

All the problems have been resolved in [#82](https://github.com/bsvars/bsvars/issues/82) following the discussion at [RcppCore/RcppArmadillo#443](https://github.com/RcppCore/RcppArmadillo/issues/443)

## Re the Last released version's CRAN status: OK: 9, NOTE: 4 all show the following:
```
checking installed package size ... NOTE
  installed size is 12.2Mb
  sub-directories of 1Mb or more:
    libs  11.1Mb
```
This seems to be normal in packages with **Rcpp** dependency and does not show in any of my tests, e.g., running `devtools::check()`.

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## GitHub R-CMD-check using `usethis::use_github_action_check_standard()`

Passing on all platforms!

## Check at using `devtools::check(manual = TRUE, remote = TRUE, incoming = TRUE)`

This shows:
>   Found the following (possibly) invalid file URI:
>     URI: www.linkedin.com/in/tomaszwwozniak
>       From: README.md
>       Status: 999
This is not a problem with the link, but how LinkedIn responds to automatic checks as documented e.g. [HERE](https://stackoverflow.com/questions/27231113/999-error-code-on-head-request-to-linkedin) and [HERE](https://http.dev/999)

## Done some more tests from `usethis::use_release_issue()`

All good here!