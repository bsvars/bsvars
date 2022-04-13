
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "sample_ABhyper.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
Rcpp::List bsvar_cpp(
  const int&  S,                        // number of draws from the posterior
  const arma::mat&  Y,                  // NxT dependent variables
  const arma::mat&  X,                  // KxT dependent variables
  const arma::field<arma::mat>& VB,     // N-list
  const Rcpp::List& prior,              // a list of priors
  const Rcpp::List& starting_values     // a list of starting values
) {

  // Progress bar setup
  vec prog_rep_points = arma::round(arma::linspace(0, S, 50));
  Rcout << "**************************************************|" << endl;
  Rcout << " Gibbs sampler for the SVAR model                 |" << endl;
  Rcout << "**************************************************|" << endl;
  Rcout << " Progress of the MCMC simulation for " << S << " draws" << endl;
  Rcout << " Press Esc to interrupt the computations" << endl;
  Rcout << "**************************************************|" << endl;
  Progress p(50, true);
  
  const int N       = Y.n_rows;
  const int K       = X.n_rows;
  
  mat   aux_B       = as<mat>(starting_values["B"]);
  mat   aux_A       = as<mat>(starting_values["A"]);
  vec   aux_hyper   = as<vec>(starting_values["hyper"]);
  
  cube  posterior_B(N, N, S);
  cube  posterior_A(N, K, S);
  mat   posterior_hyper(5, S);
  
  for (int s=0; s<S; s++) {
  
    // Increment progress bar
    if (any(prog_rep_points == s)) p.increment();
    // Check for user interrupts
    if (s % 200 == 0) checkUserInterrupt();
    
    sample_hyperparameters(aux_hyper, aux_B, aux_A, VB, prior);
    sample_A_homosk1(aux_A, aux_B, aux_hyper, Y, X, prior);
    sample_B_homosk1(aux_B, aux_A, aux_hyper, Y, X, prior, VB);
    
    posterior_B.slice(s)    = aux_B;
    posterior_A.slice(s)    = aux_A;
    posterior_hyper.col(s)  = aux_hyper;
  } // END s loop
  
  return List::create(
    _["last_draw"]  = List::create(
      _["B"]        = aux_B,
      _["A"]        = aux_A,
      _["hyper"]    = aux_hyper
    ),
    _["posterior"]  = List::create(
      _["B"]        = posterior_B,
      _["A"]        = posterior_A,
      _["hyper"]    = posterior_hyper
    )
  );
} // END bsvar_cpp
