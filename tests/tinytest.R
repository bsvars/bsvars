if ( requireNamespace("tinytest", quietly = TRUE) ) {
  # To skip running the tests on CRAN make them run only on the developer version 
  home <- length(unclass(packageVersion("bsvars"))[[1]]) == 4
  tinytest::test_package("bsvars", at_home = home)
}