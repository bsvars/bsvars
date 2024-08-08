
#ifndef _SAMPLE_T_H_
#define _SAMPLE_T_H_

#include <RcppArmadillo.h>


arma::vec sample_lambda (
    const double&       aux_df,
    const arma::mat&    aux_B,      // NxN
    const arma::mat&    aux_A,      // NxK
    const arma::mat&    Y,          // NxT
    const arma::mat&    X           // KxT
);


double log_kernel_df (
    const double&       aux_df,
    const arma::vec&    aux_lambda  // Tx1
);


arma::vec sample_df (
    double&           aux_df,
    double&           adaptive_scale,
    const arma::vec&  aux_lambda,         // Tx1
    const int&        s,                  // MCMC iteration
    const arma::vec&  adptive_alpha_gamma // 2x1 vector with target acceptance rate and step size
);


#endif  // _SAMPLE_T_H_