## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## GitHub R-CMD-check using `usethis::use_github_action_check_standard()`

Passing on all platforms!

## Check at using `rhub::check_for_cran()`

There were no ERRORs, WARNINGs.

There were some NOTEs:
```
Build ID:	bsvars_2.0.0.tar.gz-70dceca2c85641fd8ad5e1773246477b
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	8 minutes 10.9 seconds ago
Build time:	8 minutes 9.6 seconds
NOTES:
* checking HTML version of manual ... NOTE
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```

```
Build ID:	bsvars_2.0.0.tar.gz-1ed44260c0e941a8bc51d904948c520d
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	16 minutes 27.6 seconds ago
Build time:	16 minutes 24.8 seconds
NOTES:
* checking installed package size ... NOTE
  installed size is 10.7Mb
  sub-directories of 1Mb or more:
    libs   9.7Mb
* checking examples ... [26s/40s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                           user system elapsed
bsvars-package            4.450  0.129   7.309
estimate.PosteriorBSVARSV 4.484  0.007   6.909
forecast.PosteriorBSVARSV 3.600  0.000   5.294
estimate.BSVARSV          3.558  0.004   5.430
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```

```
Build ID:	bsvars_2.0.0.tar.gz-a561299b5cd24928a06c698d9e07ab96
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	20 minutes 9.7 seconds ago
Build time:	20 minutes 7.4 seconds
NOTES:
* checking installed package size ... NOTE
  installed size is 29.5Mb
  sub-directories of 1Mb or more:
    libs  28.5Mb
* checking examples ... [26s/33s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                           user system elapsed
bsvars-package            4.490  0.163   5.737
estimate.PosteriorBSVARSV 4.438  0.004   5.471
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
```
And one that I have no idea what it means and what kind of ERROR stops th process in the output files:
```
bsvars 2.0.0: PREPERROR
Build ID:	bsvars_2.0.0.tar.gz-398b44e837fb458dadfaefc28429506d
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	8 minutes 30.8 seconds ago
Build time:	8 minutes 28.6 seconds
```
