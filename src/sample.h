
#ifndef _SAMPLE_H_
#define _SAMPLE_H_

#include <RcppArmadillo.h>


int csample_num1 (
    Rcpp::NumericVector x,
    Rcpp::NumericVector prob = Rcpp::NumericVector::create()
);


#endif  // _SAMPLE_H_