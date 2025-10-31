
#ifndef _FORECAST_H_
#define _FORECAST_H_

#include <RcppArmadillo.h>


arma::vec mvnrnd_cond (
    arma::vec x,        // Nx1 with NAs or without
    arma::vec mu,       // Nx1 mean vector
    arma::mat Sigma     // NxN covariance matrix
);


arma::cube forecast_sigma2_msh (
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::mat&    S_T,                // (M,S)
    const int&    horizon
);


arma::cube forecast_sigma2_hmsh (
    arma::cube&               posterior_sigma2,   // (N, M, S)
    arma::field<arma::cube>&  posterior_PR_TR,    // (S)(M, M, N)
    arma::cube&               S_T,                // (M,N,S)
    const int&                horizon
);


arma::cube forecast_sigma2_sv (
    arma::mat&    posterior_h_T,      // NxS
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    const int&    horizon,
    const bool&   centred_sv = FALSE
);


arma::cube forecast_lambda_t (
    arma::mat&    posterior_df,      // NxS
    const int&    horizon
);


Rcpp::List forecast_bsvars (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   forecast_sigma2,    // (N, horizon, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecast,     // (horizon, N)
    const int&    horizon
);


#endif  // _FORECAST_H_