
#ifndef _BSVAR_H_
#define _BSVAR_H_

#include <RcppArmadillo.h>


void sample_B_homosk1 (
    arma::mat&        aux_B,              // NxN
    const arma::mat&  aux_A,              // NxK
    const arma::vec&  aux_hyper,          // NxM
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const Rcpp::List& prior,              // a list of priors - original dimensions
    const arma::field<arma::mat>& VB      // restrictions on B
);


void sample_A_homosk1 (
    arma::mat&        aux_A,              // NxK
    const arma::mat&  aux_B,              // NxN
    const arma::vec&  aux_hyper,          // NxM
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const Rcpp::List& prior               // a list of priors - original dimensions
);


void sample_hyperparameters (
    arma::vec&              aux_hyper,
    const arma::mat&        aux_B,
    const arma::mat&        aux_A,
    const arma::field<arma::mat>& VB,
    const Rcpp::List&       prior
);


Rcpp::List bsvar(
    const int&  S,                        // number of draws from the posterior
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const arma::field<arma::mat>& VB,     // N-list
    const Rcpp::List& prior,              // a list of priors
    const Rcpp::List& starting_values     // a list of starting values
);

#endif  // _BSVAR_H_