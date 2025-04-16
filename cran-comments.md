## Submission notes bsvars v4.0

## Re the Last released version's CRAN status - all notes have been addressed. In particular, I implemented the explicit dependency on R's version 4.1.0. Also they show:
```
Version: 3.1
Check: installed package size
Result: NOTE
    installed size is 14.7Mb
    sub-directories of 1Mb or more:
      libs  13.1Mb
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