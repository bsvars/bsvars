
#include <RcppArmadillo.h>
#include "sample.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List forecast_bsvar (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  const int       d = exogenous_forecast.n_cols;
  
  cube            forecasts(N, horizon, S);
  // vec             one(1, fill::value(1));
  vec             Xt = X_T;
  
  for (int s=0; s<S; s++) {
    
    mat Sigma     = inv_sympd(trans(posterior_B.slice(s)) * posterior_B.slice(s));
    
    for (int h=0; h<horizon; h++) {
      forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K-1-d), exogenous_forecast.row(h).t());
      
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts
  );
} // END forecast_bsvar





// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::cube forecast_sigma2_msh (
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::mat&    S_T,                // (M,S)
    const int&    horizon
) {
  
  const int       N = posterior_sigma2.n_rows;
  const int       M = posterior_PR_TR.n_rows;
  const int       S = posterior_PR_TR.n_slices;
  
  cube            forecasts_sigma2(N, horizon, S);
  
  for (int s=0; s<S; s++) {
    
    int St(M);
    vec PR_ST     = S_T.col(s);
    NumericVector zeroM   = wrap(seq_len(M) - 1);
    
    for (int h=0; h<horizon; h++) {
      
      PR_ST       = trans(posterior_PR_TR.slice(s)) * PR_ST;
      St          = csample_num1(zeroM, wrap(PR_ST));
      forecasts_sigma2.slice(s).col(h) = posterior_sigma2.slice(s).col(St);
      
    } // END h loop
  } // END s loop
  
  return forecasts_sigma2;
} // END forecast_sigma2_msh






// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List forecast_bsvar_msh (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    S_T,                // (M,S)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  const int       d = exogenous_forecast.n_cols;
  
  cube            forecasts(N, horizon, S);
  cube            forecasts_sigma2 = forecast_sigma2_msh ( posterior_sigma2, posterior_PR_TR, S_T, horizon );
  vec             Xt = X_T;
  
  for (int s=0; s<S; s++) {
    
    mat   B_inv     = inv(posterior_B.slice(s));
    mat   Sigma(N, N);
    
    for (int h=0; h<horizon; h++) {
      
      Sigma       = B_inv * diagmat(forecasts_sigma2.slice(s).col(h)) * B_inv.t();
      forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K-1-d), exogenous_forecast.row(h).t());
      
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts,
    _["forecasts_sigma"]  = sqrt(forecasts_sigma2)
  );
} // END forecast_bsvar_msh




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::cube forecast_sigma2_sv (
    arma::mat&    posterior_h_T,      // NxS
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    const int&    horizon,
    const bool&   centred_sv = FALSE
) {
  
  const int       N = posterior_rho.n_rows;
  const int       S = posterior_rho.n_cols;
  
  cube            forecasts_sigma2(N, horizon, S);
  vec             one(1, fill::value(1));
  
  for (int s=0; s<S; s++) {
    
    vec     ht    = posterior_h_T.col(s);
    double  xx    = 0;
    
    for (int h=0; h<horizon; h++) {
      for (int n=0; n<N; n++) {
        xx        = randn();
        if ( centred_sv ) {
          ht(n)     = posterior_rho(n, s) * ht(n) + posterior_omega(n, s) * xx;
        } else {
          ht(n)     = posterior_omega(n, s) * (posterior_rho(n, s) * ht(n) + xx);
        }
        forecasts_sigma2(n, h, s) = exp(ht(n));
      } // END n loop
    } // END h loop
  } // END s loop
  
  return forecasts_sigma2;
} // END forecast_sigma2_sv




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List forecast_bsvar_sv (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::mat&    posterior_h_T,      // NxS
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    const int&    horizon,
    const bool&   centred_sv = FALSE
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  const int       d = exogenous_forecast.n_cols;
  // vec             one(1, fill::value(1));
  vec             Xt = X_T;
  
  cube            forecasts(N, horizon, S);
  cube            forecasts_sigma2 = forecast_sigma2_sv( posterior_h_T, posterior_rho, posterior_omega, horizon, centred_sv );
  
  for (int s=0; s<S; s++) {
    
    mat B_inv     = inv(posterior_B.slice(s));
    mat Sigma(N, N);
    
    for (int h=0; h<horizon; h++) {
      
      Sigma       = B_inv * diagmat(forecasts_sigma2.slice(s).col(h)) * B_inv.t();
      forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K-1-d), exogenous_forecast.row(h).t());
      
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts,
    _["forecasts_sigma"]  = sqrt(forecasts_sigma2)
  );
} // END forecast_bsvar_sv




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::vec mvnrnd_cond (
    arma::vec x,        // Nx1 with NAs or without
    arma::vec mu,       // Nx1 mean vector
    arma::mat Sigma     // NxN covariance matrix
) {
  int   N         = x.n_elem;
  uvec  ind       = find_finite(x);
  uvec  ind_nan   = find_nan(x);
  mat   aj        = eye(N, N);
  
  vec   x2        = x(ind); 
  
  vec   mu1       = mu(ind_nan);
  vec   mu2       = mu(ind);
  mat   Sigma11   = Sigma(ind_nan, ind_nan);
  mat   Sigma12   = Sigma(ind_nan, ind);
  mat   Sigma22   = Sigma(ind, ind);
  mat   Sigma22_inv = inv_sympd(Sigma22);
  
  vec   mu_cond     = mu1 + Sigma12 * Sigma22_inv * (x2 - mu2);
  mat   Sigma_cond  = Sigma11 - Sigma12 * Sigma22_inv * Sigma12.t();
  
  vec   draw = mvnrnd( mu_cond, Sigma_cond);
  
  vec   out = aj.cols(ind_nan) * draw + aj.cols(ind) * x2;
  return out;
} // END mvnrnd_cond



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List forecast_conditional_bsvar (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecasts,     // (horizon, N)
    const int&    horizon
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  const int       d = exogenous_forecast.n_cols;
  vec             Xt = X_T;
  
  cube            forecasts(N, horizon, S);
  
  for (int s=0; s<S; s++) {
  
    mat Sigma     = inv_sympd(trans(posterior_B.slice(s)) * posterior_B.slice(s));
    
    for (int h=0; h<horizon; h++) {
      forecasts.slice(s).col(h) = mvnrnd_cond ( cond_forecasts.row(h).t(), posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K-1-d), exogenous_forecast.row(h).t());
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts
  );
} // END forecast_conditional_bsvar



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
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
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  const int       d = exogenous_forecast.n_cols;
  vec             Xt = X_T;
  
  cube            forecasts(N, horizon, S);
  cube            forecasts_sigma2 = forecast_sigma2_msh ( posterior_sigma2, posterior_PR_TR, S_T, horizon );
  
  for (int s=0; s<S; s++) {
    
    mat   B_inv     = inv(posterior_B.slice(s));
    mat   Sigma(N, N);
    
    for (int h=0; h<horizon; h++) {
      
      Sigma       = B_inv * diagmat(forecasts_sigma2.slice(s).col(h)) * B_inv.t();
      forecasts.slice(s).col(h) = mvnrnd_cond ( cond_forecasts.row(h).t(), posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K-1-d), exogenous_forecast.row(h).t());
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts,
    _["forecasts_sigma"]  = sqrt(forecasts_sigma2)
  );
} // END forecast_conditional_bsvar_msh



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
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
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  const int       d = exogenous_forecast.n_cols;
  vec             Xt = X_T;
  
  cube            forecasts(N, horizon, S);
  cube            forecasts_sigma2 = forecast_sigma2_sv( posterior_h_T, posterior_rho, posterior_omega, horizon, centred_sv );
  
  for (int s=0; s<S; s++) {
    
    mat B_inv     = inv(posterior_B.slice(s));
    mat Sigma(N, N);
    
    for (int h=0; h<horizon; h++) {
      
      Sigma       = B_inv * diagmat(forecasts_sigma2.slice(s).col(h)) * B_inv.t();
      forecasts.slice(s).col(h) = mvnrnd_cond ( cond_forecasts.row(h).t(), posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.subvec(N, K-1-d), exogenous_forecast.row(h).t());
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts,
    _["forecasts_sigma"]  = sqrt(forecasts_sigma2)
  );
} // END forecast_conditional_bsvar_sv
