
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
    const int     horizon
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  
  cube            forecasts(N, horizon, S);
  vec             one(1, fill::value(1));
  
  for (int s=0; s<S; s++) {
    
    vec x_t       = X_T.rows(0, K - 2);
    vec Xt        = join_cols(x_t, one);
    
    mat Sigma     = inv_sympd(trans(posterior_B.slice(s)) * posterior_B.slice(s));
    
    for (int h=0; h<horizon; h++) {
      forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.rows(N, K-2), one);
      
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts
  );
} // END forecast_bsvar




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List forecast_bsvar_msh (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   posterior_sigma2,   // (N, M, S)
    arma::cube&   posterior_PR_TR,    // (M, M, S)
    arma::vec&    X_T,                // (K)
    arma::vec&    S_T,                // (M)
    const int     horizon
) {
  
  const int       N = posterior_B.n_rows;
  const int       M = posterior_PR_TR.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  
  cube            forecasts(N, horizon, S);
  cube            forecasts_sigma2(N, horizon, S);
  vec             one(1, fill::value(1));
  
  for (int s=0; s<S; s++) {
    
    vec x_t       = X_T.rows(0, K - 2);
    vec Xt        = join_cols(x_t, one);
    
    mat B_inv     = inv(posterior_B.slice(s));
    vec PR_ST     = S_T;
    NumericVector zeroM   = wrap(seq_len(M) - 1);
    
    int St(M);
    mat Sigma(N, N);
    
    for (int h=0; h<horizon; h++) {
      
      PR_ST       = trans(posterior_PR_TR.slice(s)) * PR_ST;
      St          = csample_num1(zeroM, wrap(PR_ST));
      forecasts_sigma2.slice(s).col(h) = posterior_sigma2.slice(s).col(St);
      
      Sigma       = B_inv * diagmat(forecasts_sigma2.slice(s).col(h)) * B_inv.t();
      forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.rows(N, K-2), one);
      
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts,
    _["forecasts_sigma"]  = sqrt(forecasts_sigma2)
  );
} // END forecast_bsvar_msh




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List forecast_bsvar_sv (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::vec&    posterior_h_T,      // Nx1
    arma::mat&    posterior_rho,      // NxS
    arma::mat&    posterior_omega,    // NxS
    arma::vec&    X_T,                // (K)
    const int     horizon,
    const bool    centred_sv = FALSE
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  const int       K = posterior_A.n_cols;
  
  cube            forecasts(N, horizon, S);
  cube            forecasts_sigma2(N, horizon, S);
  vec             one(1, fill::value(1));
  
  for (int s=0; s<S; s++) {
    
    vec x_t       = X_T.rows(0, K - 2);
    vec Xt        = join_cols(x_t, one);
    vec ht        = posterior_h_T;
    
    mat B_inv     = inv(posterior_B.slice(s));
    mat Sigma(N, N);
    double xx = 0;
    
    for (int h=0; h<horizon; h++) {
      
      for (int n=0; n<N; n++) {
        xx        = randn();
        if ( centred_sv ) {
          ht(n)     = posterior_rho(n, s) * ht(n) + posterior_omega(n, s) * xx;
        } else {
          ht(n)     = posterior_omega(n, s) * (posterior_rho(n, s) * ht(n) + xx);
        }
        forecasts_sigma2(n, h, s) = exp(ht(n));
      }
      
      Sigma       = B_inv * diagmat(forecasts_sigma2.slice(s).col(h)) * B_inv.t();
      forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      Xt          = join_cols(forecasts.slice(s).col(h), Xt.rows(N, K-2), one);
      
    } // END h loop
  } // END s loop
  
  return List::create(
    _["forecasts"]        = forecasts,
    _["forecasts_sigma"]  = sqrt(forecasts_sigma2)
  );
} // END forecast_bsvar_sv
