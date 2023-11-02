
#ifndef _VERIFY_H_
#define _VERIFY_H_

#include <RcppArmadillo.h>


Rcpp::List verify_volatility_cpp (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X,          // KxT explanatory variables
    const bool              sample_s_ = true
);


#endif  // _VERIFY_H_