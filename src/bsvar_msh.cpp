
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "sample_ABhyper.h"
#include "msh.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List bsvar_msh_cpp (
    const int&              S,              // No. of posterior draws
    const arma::mat&        Y,              // NxT dependent variables
    const arma::mat&        X,              // KxT explanatory variables
    const Rcpp::List&       prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,       // restrictions on B0
    const Rcpp::List&       starting_values,
    const int               thin = 100,     // introduce thinning
    const bool              finiteM = true,
    const bool              MSnotMIX = true,
    const std::string       name_model = "",// just 3 characters
    const bool              show_progress = true
) {
  // Progress bar setup
  vec prog_rep_points = arma::round(arma::linspace(0, S, 50));
  if (show_progress) {
    Rcout << "**************************************************|" << endl;
    Rcout << "bsvars: Bayesian Structural Vector Autoregressions|" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Gibbs sampler for the SVAR-" << name_model <<" model             |" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Progress of the MCMC simulation for " << S << " draws" << endl;
    Rcout << "    Every " << thin << "th draw is saved via MCMC thinning" << endl;
    Rcout << " Press Esc to interrupt the computations" << endl;
    Rcout << "**************************************************|" << endl;
  }
  Progress p(50, show_progress);
  
  const int   T     = Y.n_cols;
  const int   N     = Y.n_rows;
  const int   K     = X.n_rows;

  mat   aux_B       = as<mat>(starting_values["B"]);
  mat   aux_A       = as<mat>(starting_values["A"]);
  mat   aux_sigma2  = as<mat>(starting_values["sigma2"]);
  mat   aux_sigma(N, T);
  mat   aux_PR_TR   = as<mat>(starting_values["PR_TR"]);
  vec   aux_pi_0    = as<vec>(starting_values["pi_0"]);
  mat   aux_xi      = as<mat>(starting_values["xi"]);
  vec   aux_hyper   = as<vec>(starting_values["hyper"]);  // 5x1 (gamma_0, gamma_+, s_0, s_+, s_)
  
  const int   M     = aux_PR_TR.n_rows;
  
  const int   SS     = floor(S / thin);
  
  cube  posterior_B(N, N, SS);
  cube  posterior_A(N, K, SS);
  cube  posterior_sigma2(N, M, SS);
  cube  posterior_PR_TR(M, M, SS);
  mat   posterior_pi_0(M, SS);
  cube  posterior_xi(M, T, SS);
  mat   posterior_hyper(5, SS);
  
  int   ss = 0;
  
  for (int s=0; s<S; s++) {
    
    // Increment progress bar
    if (any(prog_rep_points == s)) p.increment();
    // Check for user interrupts
    if (s % 200 == 0) checkUserInterrupt();
    
    // sample aux_hyper
    sample_hyperparameters(aux_hyper, aux_B, aux_A, VB, prior);
    
    // sample aux_B
    for (int t=0; t<T; t++) {
      aux_sigma.col(t)    = pow( aux_sigma2.col(aux_xi.col(t).index_max()) , 0.5 );
    }
    sample_B_heterosk1(aux_B, aux_A, aux_hyper, aux_sigma, Y, X, prior, VB);
    
    // sample aux_A
    sample_A_heterosk1(aux_A, aux_B, aux_hyper, aux_sigma, Y, X, prior);
      
    // sample aux_xi
    mat U = aux_B * (Y - aux_A * X);
    sample_Markov_process_msh(aux_xi, U, aux_sigma2, aux_PR_TR, aux_pi_0, finiteM);
    
    // sample aux_PR_TR
    sample_transition_probabilities(aux_PR_TR, aux_pi_0, aux_xi, prior, MSnotMIX);
    
    // sample aux_sigma2
    sample_variances_msh(aux_sigma2, aux_B, aux_A, Y, X, aux_xi, prior);
    
    if (s % thin == 0) {
      posterior_B.slice(ss)      = aux_B;
      posterior_A.slice(ss)      = aux_A;
      posterior_sigma2.slice(ss) = aux_sigma2;
      posterior_PR_TR.slice(ss)  = aux_PR_TR;
      posterior_pi_0.col(ss)     = aux_pi_0;
      posterior_xi.slice(ss)     = aux_xi;
      posterior_hyper.col(ss)    = aux_hyper;
      ss++;
    }
  } // END s loop
  
  return List::create(
    _["last_draw"]  = List::create(
      _["B"]        = aux_B,
      _["A"]        = aux_A,
      _["sigma2"]   = aux_sigma2,
      _["PR_TR"]    = aux_PR_TR,
      _["pi_0"]     = aux_pi_0,
      _["xi"]       = aux_xi,
      _["hyper"]    = aux_hyper
    ),
    _["posterior"]  = List::create(
      _["B"]        = posterior_B,
      _["A"]        = posterior_A,
      _["sigma2"]   = posterior_sigma2,
      _["PR_TR"]    = posterior_PR_TR,
      _["pi_0"]     = posterior_pi_0,
      _["xi"]       = posterior_xi,
      _["hyper"]    = posterior_hyper
    )
  );
} // END bsvar_msh

