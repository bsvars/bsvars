
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "msh.h"
#include "forecast.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::cube bsvars_ir1 (
    arma::mat&    aux_B,              // (N, N)
    arma::mat&    aux_A,              // (N, K)
    const int     horizon,
    const int     p,
    const bool    standardise = false
) {
  
  const int       N = aux_B.n_rows;
  cube            aux_irfs(N, N, horizon + 1);  // + 0 horizons
  mat             A_bold_tmp(N * (p - 1), N * p, fill::eye);
  
    mat   irf_0         = inv(aux_B);
    if ( standardise ) {
      irf_0             = irf_0 * diagmat(pow(diagvec(irf_0), -1));
    }
    mat   A_bold        = join_cols(aux_A.cols(0, N * p - 1), A_bold_tmp);
    mat   A_bold_power  = A_bold;
    
    aux_irfs.slice(0)   = irf_0;
    
    for (int h=1; h<horizon + 1; h++) {
      aux_irfs.slice(h) = A_bold_power.submat(0, 0, N-1, N-1) * irf_0;
      A_bold_power      = A_bold_power * A_bold;
    } // END h loop
    
  return aux_irfs;
} // END bsvars_ir1


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::field<arma::cube> bsvars_ir (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    const int     horizon,
    const int     p,
    const bool    standardise = false
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  
  cube            aux_irfs(N, N, horizon + 1);
  field<cube>     irfs(S);
  
  for (int s=0; s<S; s++) {
    aux_irfs            = bsvars_ir1( posterior_B.slice(s), posterior_A.slice(s), horizon, p , standardise);
    irfs(s)             = aux_irfs;
  } // END s loop
  
  return irfs;
} // END bsvars_ir



// [[Rcpp::interfaces(cpp,r)]]
// [[Rcpp::export]]
arma::field<arma::cube> bsvars_fevd_homosk (
    arma::field<arma::cube>&    posterior_irf   // output of bsvars_irf
) {
  
  const int       N = posterior_irf(0).n_rows;
  const int       S = posterior_irf.n_rows;
  const int       horizon = posterior_irf(0).n_slices;
  
  field<cube>     fevds(S);
  cube            aux_fevds(N, N, horizon);
  cube            tmp_fevds(N, N, horizon);
  
  for (int s=0; s<S; s++) {
    for (int h=0; h<horizon; h++) {
      
      tmp_fevds.slice(h) = square(posterior_irf(s).slice(h));
      
      for (int n=0; n<N; n++) {
        for (int nn=0; nn<N; nn++) {
          aux_fevds.subcube(n, nn, h, n, nn, h) = accu(tmp_fevds.subcube(n, nn, 0, n, nn, h));
        }
      }
      aux_fevds.slice(h)  = diagmat(1/sum(aux_fevds.slice(h), 1)) * aux_fevds.slice(h);
    }
    aux_fevds            *= 100;
    fevds(s)              = aux_fevds;
  } // END s loop
  
  return fevds;
} // END bsvars_fevd_homosk





// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::field<arma::cube> bsvars_fevd_heterosk (
    arma::field<arma::cube>&    posterior_irf,    // output of bsvars_irf
    arma::cube&                 forecast_sigma2,  // (N, H, S) output from forecast_sigma2 or forecast_sigma2_msh
    arma::mat&                  sigma2_T          // (N, S) the last in the sample
) {
  
  const int       N = posterior_irf(0).n_rows;
  const int       S = posterior_irf.n_rows;
  const int       horizon = posterior_irf(0).n_slices;
  
  field<cube>     fevds(S);
  cube            aux_fevds(N, N, horizon);
  cube            tmp_fevds(N, N, horizon);
  
  for (int s=0; s<S; s++) {
    for (int h=0; h<horizon; h++) {
      if ( h == 0) {
        mat diag_sigma2     = diagmat(sigma2_T.col(s));
        tmp_fevds.slice(h)  = square(posterior_irf(s).slice(h)) * diag_sigma2;
      } else {
        mat diag_sigma2     = diagmat(forecast_sigma2.slice(s).col(h - 1));
        tmp_fevds.slice(h)  = square(posterior_irf(s).slice(h)) * diag_sigma2;
      }
      
      for (int n=0; n<N; n++) {
        for (int nn=0; nn<N; nn++) {
          aux_fevds.subcube(n, nn, h, n, nn, h) = accu(tmp_fevds.subcube(n, nn, 0, n, nn, h));
        }
      }
      aux_fevds.slice(h)  = diagmat(1/sum(aux_fevds.slice(h), 1)) * aux_fevds.slice(h);
    }
    aux_fevds            *= 100;
    fevds(s)              = aux_fevds;
  } // END s loop
  
  return fevds;
} // END bsvars_fevd_heterosk




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::cube bsvars_structural_shocks (
    const arma::cube&     posterior_B,    // (N, N, S)
    const arma::cube&     posterior_A,    // (N, K, S)
    const arma::mat&      Y,              // NxT dependent variables
    const arma::mat&      X               // KxT dependent variables
) {
  
  const int       N = Y.n_rows;
  const int       T = Y.n_cols;
  const int       S = posterior_B.n_slices;
  
  cube            structural_shocks(N, T, S);
  
  for (int s=0; s<S; s++) {
    structural_shocks.slice(s)    = posterior_B.slice(s) * (Y - posterior_A.slice(s) * X);
  } // END s loop
  
  return structural_shocks;
} // END bsvars_structural_shocks



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::field<arma::cube> bsvars_hd (
    arma::field<arma::cube>&    posterior_irf_T,    // output of bsvars_irf with irfs at T horizons
    arma::cube&                 structural_shocks,  // NxTxS output bsvars_structural_shocks
    const bool                  show_progress = true
) {
  
  const int       N = structural_shocks.n_rows;
  const int       T = structural_shocks.n_cols;
  const int       S = structural_shocks.n_slices;
  
  // Progress bar setup
  vec prog_rep_points = arma::round(arma::linspace(0, S, 50));
  if (show_progress) {
    Rcout << "**************************************************|" << endl;
    Rcout << "bsvars: Bayesian Structural Vector Autoregressions|" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Computing historical decomposition               |" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " This might take a little while :)                " << endl;
    Rcout << "**************************************************|" << endl;
  }
  Progress p(50, show_progress);
  
  
  field<cube>     hds(S);
  cube            aux_hds(N, N, T);
  mat             posterior_irf_t(N, N);
  
  for (int s=0; s<S; s++) {
    
    // Increment progress bar
    if (any(prog_rep_points == s)) p.increment();
    // Check for user interrupts
    if (s % 200 == 0) checkUserInterrupt();
    
    for (int t=0; t<T; t++) {
      
      cube        hds_at_t(N, N, t + 1);
      for (int i=0; i<t; i++) {
        posterior_irf_t   = posterior_irf_T(s).slice(t - i - 1);
        hds_at_t.slice(i) = posterior_irf_t.each_col() % structural_shocks.slice(s).col(i);
      } // END i loop
      
      aux_hds.slice(t) = sum(hds_at_t, 2);
    } // END t loop
    
    hds(s)          = aux_hds;
  } // END s loop
  
  return hds;
} // END bsvars_hd



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::cube bsvars_fitted_values (
  arma::cube&     posterior_A,        // NxKxS
  arma::cube&     posterior_B,        // NxNxS
  arma::cube&     posterior_sigma,    // NxTxS
  arma::mat&      X                   // KxT
) {
  
  const int   N = posterior_A.n_rows;
  const int   S = posterior_A.n_slices;
  const int   T = X.n_cols;
  
  cube    fitted_values(N, T, S, fill::randn);
  
  for (int s=0; s<S; s++) {
    mat Binv_sigma_norm    = solve(posterior_B.slice(s), posterior_sigma.slice(s) % fitted_values.slice(s));
    fitted_values.slice(s) = posterior_A.slice(s) * X + Binv_sigma_norm; 
  } // END s loop
  
  return fitted_values;
} // END bsvars_fitted_values



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::cube bsvars_filter_forecast_smooth (
  Rcpp::List&       posterior,
  const arma::mat&  Y,
  const arma::mat&  X,
  const bool        forecasted,
  const bool        smoothed
) {
  
  cube  posterior_B       = as<cube>(posterior["B"]);
  cube  posterior_A       = as<cube>(posterior["A"]);
  cube  posterior_sigma2  = as<cube>(posterior["sigma2"]);
  cube  posterior_PR_TR   = as<cube>(posterior["PR_TR"]);
  mat   posterior_pi_0    = as<mat>(posterior["pi_0"]);
  
  const int   N           = Y.n_rows;
  const int   M           = posterior_PR_TR.n_rows;
  const int   T           = Y.n_cols;
  const int   S           = posterior_B.n_slices;
  
  cube  filtered_probabilities(M, T, S);
  cube  for_smo_probabilities(M, T, S);
  mat   shocks(N, T);
  
  for (int s=0; s<S; s++) {
    shocks                            = posterior_B.slice(s) * ( Y - posterior_A.slice(s) * X );
    
    filtered_probabilities.slice(s)   = filtering_msh(
      shocks, 
      posterior_sigma2.slice(s), 
      posterior_PR_TR.slice(s), 
      posterior_pi_0.col(s)
    );
    
    if (forecasted) {
      for_smo_probabilities.slice(s)  = posterior_PR_TR.slice(s) * filtered_probabilities.slice(s);
    } else if (smoothed) {
      for_smo_probabilities.slice(s)  = smoothing_msh(shocks, posterior_PR_TR.slice(s), filtered_probabilities.slice(s));
    }
  } // END s loop
  
  cube  out     = filtered_probabilities;
  if (forecasted || smoothed) {
    out         = for_smo_probabilities;
  }
  
  return out;
} // END bsvars_filter_forecast_smooth


