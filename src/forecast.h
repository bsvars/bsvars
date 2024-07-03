
#ifndef _FORECAST_H_
#define _FORECAST_H_

#include <RcppArmadillo.h>

Rcpp::List forecast_bsvar (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon
);


arma::cube forecast_sigma2_msh (
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::mat&    S_T,                // (M,S)
    const int&    horizon
);


Rcpp::List forecast_bsvar_msh (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    S_T,                // (M,S)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon
);


arma::cube forecast_sigma2_sv (
    arma::mat&    posterior_h_T,      // NxS
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    const int&    horizon,
    const bool&   centred_sv = FALSE
);


Rcpp::List forecast_bsvar_sv (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    posterior_h_T,      // Nx1
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon,
    const bool&   centred_sv = FALSE
);


arma::mat forecast_lambda_t (
    arma::mat&    posterior_df,      // NxS
    const int&    horizon
);


Rcpp::List forecast_bsvar_t (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::mat&    posterior_df,       // (S, 1)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon
);


arma::vec mvnrnd_cond (
    arma::vec x,        // Nx1 with NAs or without
    arma::vec mu,       // Nx1 mean vector
    arma::mat Sigma     // NxN covariance matrix
);


Rcpp::List forecast_conditional_bsvar (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecasts,     // (horizon, N)
    const int&    horizon
);


Rcpp::List forecast_conditional_bsvar_msh (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    S_T,                // (M,S)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecasts,     // (horizon, N)
    const int&    horizon
);


Rcpp::List forecast_conditional_bsvar_sv (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::mat&    posterior_h_T,      // NxS
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecasts,     // (horizon, N)
    const int&    horizon,
    const bool&   centred_sv = FALSE
);


Rcpp::List forecast_conditional_bsvar_t (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::mat&    posterior_df,       // (S, 1)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecasts,     // (horizon, N)
    const int&    horizon
);


#endif  // _BSVARTOOLS_H_