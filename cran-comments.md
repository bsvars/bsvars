## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## GitHub R-CMD-check using `usethis::use_github_action_check_standard()`

Passing on all platforms!

## Check at <https://win-builder.r-project.org/upload.aspx>

There were no ERRORs, WARNINGs.

There was one NOTE:

        * checking CRAN incoming feasibility ... [9s] NOTE
        Maintainer: 'Tomasz Woźniak <wozniak.tom@pm.me>'

        New submission

        Possibly misspelled words in DESCRIPTION:
          Autoregressive (3:49, 8:80)
          SVAR (8:96, 8:163)
          heteroskedastic (8:210)

All these words are spelled correctly and they are terms specific to the models that are implemented in the package. They are used in a top-field journal published paper co-authored by the author of the package. The paper can be found at: <https://doi.org/10.1016/j.jedc.2020.103862>.
