
#ifndef _SAMPLE_ABHYPER_H_
#define _SAMPLE_ABHYPER_H_

#include <RcppArmadillo.h>


arma::mat sample_A_homosk1 (
    arma::mat&        aux_A,          // NxK
    const arma::mat&  aux_B,          // NxN
    const arma::mat&  aux_hyper,      // (2*N+1) x 2 :: col 0 for B, col 1 for A
    const arma::mat&  Y,              // NxT dependent variables
    const arma::mat&  X,              // KxT dependent variables
    const Rcpp::List& prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VA  // restrictions on A
);


arma::mat sample_A_heterosk1 (
    arma::mat&        aux_A,          // NxK
    const arma::mat&  aux_B,          // NxN
    const arma::mat&  aux_hyper,      // (2*N+1) x 2 :: col 0 for B, col 1 for A
    const arma::mat&  aux_sigma,      // NxT conditional STANDARD DEVIATIONS
    const arma::mat&  Y,              // NxT dependent variables
    const arma::mat&  X,              // KxT dependent variables
    const Rcpp::List& prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VA  // restrictions on A
);


arma::mat sample_B_homosk1 (
    arma::mat&        aux_B,              // NxN
    const arma::mat&  aux_A,              // NxK
    const arma::mat&  aux_hyper,          // (2*N+1)x2
    const arma::mat&  Y,                  // NxT dependent variables
    const arma::mat&  X,                  // KxT dependent variables
    const Rcpp::List& prior,              // a list of priors - original dimensions
    const arma::field<arma::mat>& VB      // restrictions on B
);


arma::mat sample_B_heterosk1 (
    arma::mat&        aux_B,          // NxN
    const arma::mat&  aux_A,          // NxK
    const arma::mat&  aux_hyper,      // (2*N+1)x2
    const arma::mat&  aux_sigma,      // NxT conditional STANDARD DEVIATIONS
    const arma::mat&  Y,              // NxT dependent variables
    const arma::mat&  X,              // KxT dependent variables
    const Rcpp::List& prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VB  // restrictions on B0
);


arma::mat sample_hyperparameters (
    arma::mat&              aux_hyper,       // (2*N+1) x 2 :: col 0 for B, col 1 for A
    const arma::mat&        aux_B,            // NxN
    const arma::mat&        aux_A,
    const arma::field<arma::mat>& VB,
    const arma::field<arma::mat>& VA,
    const Rcpp::List&       prior
);


#endif  // _SAMPLE_ABHYPER_H_