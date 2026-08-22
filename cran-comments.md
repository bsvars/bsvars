## Submission notes bsvars v4.0

- All GitHub Actions checks pass!
- Done all tests from `usethis::use_release_issue()` It passes!
- Running `revdepcheck::revdep_check(num_workers = 4)` gives us a pass!
- We did the spelling check running `devtools::spell_check()`. It reports some words. We investigated and all is as intended.
- We run `revdepcheck::revdep_check(num_workers = 4)` and it's all good here.
- winbuilder: Package bsvars_4.0.tar.gz returns a NOTE:
```
Possibly misspelled words in DESCRIPTION:
  Hassan (4:1650)
  Liu (4:1637)
```
All is as intended!

## CRAN Submission bsvars 4.0 autocheck returned

```
Best regards,
CRAN teams' auto-check service
Flavor: r-devel-windows-x86_64
Check: CRAN incoming feasibility, Result: NOTE
  Maintainer: 'Tomasz Woźniak <wozniak.tom@pm.me>'

  Possibly misspelled words in DESCRIPTION:
    Hassan (4:1650)
    Liu (4:1637)

Flavor: r-devel-linux-x86_64-debian-gcc
Check: CRAN incoming feasibility, Result: NOTE
  Maintainer: 'Tomasz Woźniak <wozniak.tom@pm.me>'

  Possibly misspelled words in DESCRIPTION:
    Hassan (4:1643)
    Liu (4:1630)

Flavor: r-devel-linux-x86_64-debian-gcc
Check: sizes of PDF files under 'inst/doc', Result: WARNING
    'gs+qpdf' made some significant size reductions:
       compacted 'bsvars_vignette.pdf' from 723Kb to 471Kb
    consider running tools::compactPDF(gs_quality = "ebook") on these files,
    or build the source package with --compact-vignettes=both
```
- compressed the pdfs all accross the package.
- spelling in "Liu" and "Hassan" is as intended.

Thanks!