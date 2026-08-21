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