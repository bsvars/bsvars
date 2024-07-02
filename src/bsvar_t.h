
#ifndef _BSVAR_T_H_
#define _BSVAR_T_H_

#include <RcppArmadillo.h>

Rcpp::List bsvar_t_cpp(
    const int&  S,                        // number of draws from the posterior
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const arma::field<arma::mat>& VB,     // N-list
    const Rcpp::List& prior,              // a list of priors
    const Rcpp::List& starting_values,    // a list of starting values
    const arma::vec&  adptive_alpha_gamma,// a 2x1 vector of adaptive MH tuning parameters: target acceptance and discounting factor
    const int         thin = 100,         // introduce thinning
    const bool        show_progress = true
);

#endif  // _BSVAR_T_H_