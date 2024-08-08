
#include <RcppArmadillo.h>
#include "sample.h"

using namespace Rcpp;
using namespace arma;


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
arma::mat forecast_lambda_t (
    arma::mat&    posterior_df,      // Sx1
    const int&    horizon
) {
  
  const int       S = posterior_df.n_rows;
  mat             forecasts_lambda(horizon, S, fill::ones);
  
  for (int s=0; s<S; s++) {
    for (int h=0; h<horizon; h++) {
      double df_s               = as_scalar(posterior_df.row(s));
      forecasts_lambda.col(s)   *= df_s + 2;
      forecasts_lambda.col(s)  /= chi2rnd( df_s, horizon );
    } // END h loop
  } // END s loop
  
  return forecasts_lambda;
} // END forecast_lambda_t





// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::cube forecast_bsvars (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    arma::cube&   forecast_sigma2,    // (N, horizon, S)
    arma::vec&    X_T,                // (K)
    arma::mat&    exogenous_forecast, // (horizon, d)
    arma::mat&    cond_forecast,     // (horizon, N)
    const int&    horizon
) {
  
  const int   N = posterior_B.n_rows;
  const int   S = posterior_B.n_slices;
  const int   K = posterior_A.n_cols;
  const int   d = exogenous_forecast.n_cols;
  
  bool        do_exog = exogenous_forecast.is_finite();
  vec         x_t;
  if ( do_exog ) {
    x_t       = X_T.rows(0, K - 1 - d);
  } else {
    x_t       = X_T.rows(0, K - 1);
  } // END if do_exog
  
  vec         Xt(K);
  cube        forecasts(N, horizon, S);
  
  for (int s=0; s<S; s++) {
    
    if ( do_exog ) {
      Xt          = join_cols(x_t, trans(exogenous_forecast.row(0)));
    } else {
      Xt          = x_t;
    } // END if do_exog
    
    for (int h=0; h<horizon; h++) {
      
      mat   B_inv             = inv(posterior_B.slice(s));
      mat   s2_diag           = diagmat(forecast_sigma2.slice(s).col(h));
      mat   Sigma             = B_inv * s2_diag * B_inv.t();
        
      vec   cond_forecast_h   = trans(cond_forecast.row(h));
      uvec  nonf_el           = find_nonfinite( cond_forecast_h );
      int   nonf_no           = nonf_el.n_elem;
      
      if ( nonf_no == N ) {
        forecasts.slice(s).col(h) = mvnrnd( posterior_A.slice(s) * Xt, Sigma );
      } else {
        forecasts.slice(s).col(h) = mvnrnd_cond( cond_forecast_h, posterior_A.slice(s) * Xt, Sigma );   // does not work if cond_fc_h is all nan
      } // END if nonf_no
      
      if ( h != horizon - 1 ) {
        if ( do_exog ) {
          Xt          = join_cols( forecasts.slice(s).col(h), Xt.subvec(N, K - 1 - d), trans(cond_forecast.row(h + 1)) );
        } else {
          Xt          = join_cols( forecasts.slice(s).col(h), Xt.subvec(N, K - 1) );
        }
      } // END if h
      
    } // END h loop
  } // END s loop
  
  return forecasts;
} // END forecast_bsvar

