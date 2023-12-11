
#ifndef _FORECAST_H_
#define _FORECAST_H_

#include <RcppArmadillo.h>

Rcpp::List forecast_bsvar (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int     horizon
);


Rcpp::List forecast_bsvar_msh (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::vec&    X_T,                // (K)
    arma::vec&    S_T,                // (M)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int     horizon
);


Rcpp::List forecast_bsvar_sv (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    posterior_h_T,      // Nx1
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int     horizon,
    const bool    centred_sv = FALSE
);


#endif  // _BSVARTOOLS_H_