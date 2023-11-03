
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


double dig2dirichlet (
    const arma::vec&  x,                // M-vector of positive rv summing up to 1
    const arma::vec&  a,                // M-vector of positive parameters
    const arma::vec&  b,                // M-vector of positive parameters
    const bool  logarithm = true  
);


double ddirichlet (
    const arma::vec&  x,                // M-vector of positive rv summing up to 1
    const arma::vec&  a,                // M-vector of positive parameters
    const bool  logarithm = true  
);

#endif  // _VERIFY_H_