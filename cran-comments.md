## Submission notes bsvars v3.0

## Comments responding to cran-submissions@r-project.org

Thank you for your feedback! I have now accommodated all of it!
- the spelling is as desired
- corrected the links to a desired format
- changed the `\url{}` to `\doi{}` where it showed problems
- improved the running time of examples
- Re the Last released version's CRAN status: OK: 9, NOTE: 4 all show the following:
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

## Check at using `rhub::check_for_cran()`

All good here!

## Done some more tests from `usethis::use_release_issue()`

All good here!