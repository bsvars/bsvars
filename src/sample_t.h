
#ifndef _SAMPLE_T_H_
#define _SAMPLE_T_H_

#include <RcppArmadillo.h>


arma::mat sample_lambda (
    const arma::vec&    aux_df,     // Nx1
    const arma::mat&    aux_B,      // NxN
    const arma::mat&    aux_A,      // NxK
    const arma::mat&    Y,          // NxT
    const arma::mat&    X           // KxT
);


double log_kernel_df (
    const double&       aux_df,
    const arma::rowvec&    aux_lambda  // Tx1
);


Rcpp::List sample_df (
    arma::vec&        aux_df,             // Nx1
    arma::vec&        adaptive_scale,     // Nx1
    const arma::mat&  aux_lambda,         // NxT
    const int&        s,                  // MCMC iteration
    const arma::vec&  adptive_alpha_gamma // 2x1 vector with target acceptance rate and step size
);


#endif  // _SAMPLE_T_H_