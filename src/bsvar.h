
#ifndef _BSVAR_H_
#define _BSVAR_H_

#include <RcppArmadillo.h>

Rcpp::List bsvar(
    const int&  S,                        // number of draws from the posterior
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const arma::field<arma::mat>& VB,     // N-list
    const Rcpp::List& prior,              // a list of priors
    const Rcpp::List& starting_values     // a list of starting values
);

#endif  // _BSVAR_H_