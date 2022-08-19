
#ifndef _SAMPLE_H_
#define _SAMPLE_H_

#include <RcppArmadillo.h>


int csample_num1 (
    Rcpp::NumericVector x,
    Rcpp::NumericVector prob = Rcpp::NumericVector::create()
);


Rcpp::List logSDDR_homoskedasticity (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X,          // KxT explanatory variables
    const bool              sample_s_ = true
);



#endif  // _SAMPLE_H_