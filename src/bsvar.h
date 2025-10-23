
#ifndef _BSVAR_H_
#define _BSVAR_H_

#include <RcppArmadillo.h>

Rcpp::List bsvar_cpp(
    const int&  S,                        // number of draws from the posterior
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const arma::field<arma::mat>& VB,     // N-list
    const arma::field<arma::mat>& VA,     // N-list
    const Rcpp::List& prior,              // a list of priors
    const Rcpp::List& starting_values,    // a list of starting values
    const bool        normal = true,
    const int         thin = 100,         // introduce thinning
    const bool        show_progress = true
);

#endif  // _BSVAR_H_