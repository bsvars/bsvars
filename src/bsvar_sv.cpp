
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "sample_ABhyper.h"
#include "sv.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List bsvar_sv_cpp (
    const int&                    S,          // No. of posterior draws
    const arma::mat&              Y,          // NxT dependent variables
    const arma::mat&              X,          // KxT explanatory variables
    const Rcpp::List&             prior,      // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,         // restrictions on B0
    const Rcpp::List&             starting_values, 
    const int                     thin = 100, // introduce thinning
    const bool                    centred_sv = false,
    const bool                    show_progress = true
) {
  // Progress bar setup
  vec prog_rep_points = arma::round(arma::linspace(0, S, 50));
  
  std::string oo = "";
  if ( thin != 1 ) {
    oo      = ordinal(thin) + " ";
  }
  
  std::string       name_model = "";
  if ( centred_sv ) {
    name_model        = "    Centred";
  } else {
    name_model        = "Non-centred";
  }
  
  if (show_progress) {
    Rcout << "**************************************************|" << endl;
    Rcout << "bsvars: Bayesian Structural Vector Autoregressions|" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Gibbs sampler for the SVAR-SV model              |" << endl;
    Rcout << "   " << name_model << " SV model is estimated             |" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Progress of the MCMC simulation for " << S << " draws" << endl;
    Rcout << "    Every " << oo << "draw is saved via MCMC thinning" << endl;
    Rcout << " Press Esc to interrupt the computations" << endl;
    Rcout << "**************************************************|" << endl;
  }
  Progress p(50, show_progress);
  
  const int   T     = Y.n_cols;
  const int   N     = Y.n_rows;
  const int   K     = X.n_rows;
  
  mat   aux_B       = as<mat>(starting_values["B"]);
  mat   aux_A       = as<mat>(starting_values["A"]);
  mat   aux_hyper   = as<mat>(starting_values["hyper"]);  
  mat   aux_h       = as<mat>(starting_values["h"]);
  vec   aux_rho     = as<vec>(starting_values["rho"]);
  vec   aux_omega   = as<vec>(starting_values["omega"]);
  vec   aux_sigma2v = as<vec>(starting_values["sigma2v"]);
  umat  aux_S       = as<umat>(starting_values["S"]);
  vec   aux_sigma2_omega = as<vec>(starting_values["sigma2_omega"]);
  vec   aux_s_      = as<vec>(starting_values["s_"]);
  mat   aux_sigma(N, T);
  
  if ( centred_sv ) {
    for (int n=0; n<N; n++) {
      aux_sigma.row(n) = exp(0.5 * aux_h.row(n));
    }
  } else {
    for (int n=0; n<N; n++) {
      aux_sigma.row(n) = exp(0.5 * aux_omega(n) * aux_h.row(n));
    }
  }
  
  const int   SS     = floor(S / thin);
  
  cube  posterior_B(N, N, SS);
  cube  posterior_A(N, K, SS);
  cube  posterior_hyper(2 * N + 1, 2, SS);
  cube  posterior_h(N, T, SS);
  mat   posterior_rho(N, SS);
  mat   posterior_omega(N, SS);
  mat   posterior_sigma2v(N, SS);
  ucube posterior_S(N, T, SS);
  mat   posterior_sigma2_omega(N, SS);
  mat   posterior_s_(N, SS);
  cube  posterior_sigma(N, T, SS);
  
  int   ss = 0;
  
  for (int s=0; s<S; s++) {
    
    // Increment progress bar
    if (any(prog_rep_points == s)) p.increment();
    // Check for user interrupts
    if (s % 200 == 0) checkUserInterrupt();
    
    // sample aux_hyper
    sample_hyperparameters( aux_hyper, aux_B, aux_A, VB, prior);
    
    // sample aux_B
    sample_B_heterosk1(aux_B, aux_A, aux_hyper, aux_sigma, Y, X, prior, VB);
    
    // sample aux_A
    sample_A_heterosk1(aux_A, aux_B, aux_hyper, aux_sigma, Y, X, prior);
    
    // sample aux_h, aux_omega and aux_S, aux_sigma2_omega
    mat U = aux_B * (Y - aux_A * X);
    
    for (int n=0; n<N; n++) {
      rowvec  h_tmp     = aux_h.row(n);
      double  rho_tmp   = aux_rho(n);
      double  omega_tmp = aux_omega(n);
      double  sigma2v_tmp = aux_sigma2v(n);
      urowvec S_tmp     = aux_S.row(n);
      rowvec  U_tmp     = U.row(n);
      double  s2o_tmp   = aux_sigma2_omega(n);
      double  s_n       = aux_s_(n);
      
      List sv_n;
      if ( centred_sv ) {
        sv_n            = svar_ce1( h_tmp, rho_tmp, omega_tmp, sigma2v_tmp, s2o_tmp, s_n, S_tmp, U_tmp, prior, true );
      } else {
        sv_n            = svar_nc1( h_tmp, rho_tmp, omega_tmp, sigma2v_tmp, s2o_tmp, s_n, S_tmp, U_tmp, prior, true );
      }

      aux_h.row(n)      = as<rowvec>(sv_n["aux_h_n"]);
      aux_rho(n)        = as<double>(sv_n["aux_rho_n"]);
      aux_omega(n)      = as<double>(sv_n["aux_omega_n"]);
      aux_sigma2v(n)    = as<double>(sv_n["aux_sigma2v_n"]);
      aux_S.row(n)      = as<urowvec>(sv_n["aux_S_n"]);
      aux_sigma2_omega(n)         = as<double>(sv_n["aux_sigma2_omega_n"]);
      aux_s_(n)         = as<double>(sv_n["aux_s_n"]);

      if ( centred_sv ) {
        aux_sigma.row(n)  = exp(0.5 * aux_h.row(n));
      } else {
        aux_sigma.row(n)  = exp(0.5 * aux_omega(n) * aux_h.row(n));
      }
    }
    
    if (s % thin == 0) {
      posterior_B.slice(ss)          = aux_B;
      posterior_A.slice(ss)          = aux_A;
      posterior_hyper.slice(ss)      = aux_hyper;
      posterior_h.slice(ss)          = aux_h;
      posterior_rho.col(ss)          = aux_rho;
      posterior_omega.col(ss)        = aux_omega;
      posterior_sigma2v.col(ss)       = aux_sigma2v;
      posterior_S.slice(ss)          = aux_S;
      posterior_sigma2_omega.col(ss) = aux_sigma2_omega;
      posterior_s_.col(ss)           = aux_s_;
      posterior_sigma.slice(ss)      = aux_sigma;
      ss++;
    }
  } // END s loop
  
  return List::create(
    _["last_draw"]  = List::create(
      _["B"]        = aux_B,
      _["A"]        = aux_A,
      _["hyper"]    = aux_hyper,
      _["h"]        = aux_h,
      _["rho"]      = aux_rho,
      _["omega"]    = aux_omega,
      _["sigma2v"]  = aux_sigma2v,
      _["S"]        = aux_S,
      _["sigma2_omega"] = aux_sigma2_omega,
      _["s_"]       = aux_s_,
      _["sigma"]    = aux_sigma
    ),
    _["posterior"]  = List::create(
      _["B"]        = posterior_B,
      _["A"]        = posterior_A,
      _["hyper"]    = posterior_hyper,
      _["h"]        = posterior_h,
      _["rho"]      = posterior_rho,
      _["omega"]    = posterior_omega,
      _["sigma2v"]  = posterior_sigma2v,
      _["S"]        = posterior_S,
      _["sigma2_omega"] = posterior_sigma2_omega,
      _["s_"]        = posterior_s_,
      _["sigma"]    = posterior_sigma
    )
  );
} // END bsvar_sv_cpp


