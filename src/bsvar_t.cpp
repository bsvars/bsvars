
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "sample_ABhyper.h"
#include "sample_t.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List bsvar_t_cpp(
  const int&  S,                        // number of draws from the posterior
  const arma::mat&  Y,                  // NxT dependent variables
  const arma::mat&  X,                  // KxT dependent variables
  const arma::field<arma::mat>& VB,     // N-list
  const Rcpp::List& prior,              // a list of priors
  const Rcpp::List& starting_values,    // a list of starting values
  const arma::vec&  adptive_alpha_gamma,// a 2x1 vector of adaptive MH tuning parameters: target acceptance and discounting factor
  const int         thin = 100,         // introduce thinning
  const bool        show_progress = true
) {

  std::string oo = "";
  if ( thin != 1 ) {
    oo      = ordinal(thin) + " ";
  }
  
  // Progress bar setup
  vec prog_rep_points = arma::round(arma::linspace(0, S, 50));
  if (show_progress) {
    Rcout << "**************************************************|" << endl;
    Rcout << "bsvars: Bayesian Structural Vector Autoregressions|" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Gibbs sampler for the SVAR model                 |" << endl;
    Rcout << "    with t-distributed structural skocks          |" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Progress of the MCMC simulation for " << S << " draws" << endl;
    Rcout << "    Every " << oo << "draw is saved via MCMC thinning" << endl;
    Rcout << " Press Esc to interrupt the computations" << endl;
    Rcout << "**************************************************|" << endl;
  }
  Progress p(50, show_progress);
  
  const int T         = Y.n_cols;
  const int N         = Y.n_rows;
  const int K         = X.n_rows;
  
  mat     aux_B       = as<mat>(starting_values["B"]);
  mat     aux_A       = as<mat>(starting_values["A"]);
  mat     aux_hyper   = as<mat>(starting_values["hyper"]);
  vec     aux_lambda  = as<vec>(starting_values["lambda"]);
  double  aux_df      = as<double>(starting_values["df"]);
  
  const int   SS    = floor(S / thin);
  
  cube  posterior_B(N, N, SS);
  cube  posterior_A(N, K, SS);
  cube  posterior_hyper(2 * N + 1, 2, SS);
  mat   posterior_lambda(T, SS);
  vec   posterior_df(SS);
  mat   tmp_lambda_sqrt(N, T);
  
  int   ss = 0;
  
  // the initial value for the adaptive_scale is set to the negative inverse of 
  // Hessian for the posterior log_kenel for df evaluated at df = 30
  double adaptive_scale = pow(0.25 * T * R::psigamma(15, 1) - T * pow(17, -2) - 2 * pow(16, -2), -1);
  
  for (int s=0; s<S; s++) {
  
    // Increment progress bar
    if (any(prog_rep_points == s)) p.increment();
    // Check for user interrupts
    if (s % 200 == 0) checkUserInterrupt();
    
    vec df_tmp      = sample_df ( aux_df, adaptive_scale, aux_lambda, s, adptive_alpha_gamma );
    aux_df          = df_tmp(0); 
    adaptive_scale  = df_tmp(1);
    
    aux_lambda      = sample_lambda ( aux_df, aux_B, aux_A, Y, X );
    tmp_lambda_sqrt.each_row() = sqrt(aux_lambda.t());
    
    aux_hyper       = sample_hyperparameters(aux_hyper, aux_B, aux_A, VB, prior);
    aux_A           = sample_A_heterosk1 ( aux_A, aux_B, aux_hyper, tmp_lambda_sqrt, Y, X, prior);
    aux_B           = sample_B_heterosk1 ( aux_B, aux_A, aux_hyper, tmp_lambda_sqrt, Y, X, prior, VB );
    
    if (s % thin == 0) {
      posterior_B.slice(ss)     = aux_B;
      posterior_A.slice(ss)     = aux_A;
      posterior_hyper.slice(ss) = aux_hyper;
      posterior_lambda.col(ss)  = aux_lambda;
      posterior_df(ss)          = aux_df;
      ss++;
    }
  } // END s loop
  
  return List::create(
    _["last_draw"]  = List::create(
      _["B"]        = aux_B,
      _["A"]        = aux_A,
      _["hyper"]    = aux_hyper,
      _["lambda"]   = aux_lambda,
      _["df"]       = aux_df
    ),
    _["posterior"]  = List::create(
      _["B"]        = posterior_B,
      _["A"]        = posterior_A,
      _["hyper"]    = posterior_hyper,
      _["lambda"]   = posterior_lambda,
      _["df"]       = posterior_df
    )
  );
} // END bsvar_t_cpp
