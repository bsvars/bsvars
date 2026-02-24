
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "sample_ABhyper.h"
#include "msh.h"
#include "sample_t.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List bsvar_hmsh_cpp (
    const int&              S,              // No. of posterior draws
    const arma::mat&        Y,              // NxT dependent variables
    const arma::mat&        X,              // KxT explanatory variables
    const Rcpp::List&       prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,       // restrictions on B0
    const arma::field<arma::mat>& VA,       // N-list
    const Rcpp::List&       starting_values,
    const bool              normal = true,
    const int               thin = 100,     // introduce thinning
    const bool              finiteM = true,
    const bool              MSnotMIX = true,
    const std::string       name_model = "",// just 3 characters
    const bool              show_progress = true
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
    Rcout << " Gibbs sampler for the SVAR-" << name_model <<" model" << endl;
    Rcout << "**************************************************|" << endl;
    Rcout << " Progress of the MCMC simulation for " << S << " draws" << endl;
    Rcout << "    Every " << oo << "draw is saved via MCMC thinning" << endl;
    Rcout << " Press Esc to interrupt the computations" << endl;
    Rcout << "**************************************************|" << endl;
  }
  Progress p(50, show_progress);
  
  const vec adptive_alpha_gamma = as<vec>(NumericVector::create(0.44, 0.6));
  
  const int   T     = Y.n_cols;
  const int   N     = Y.n_rows;
  const int   K     = X.n_rows;

  mat   aux_B       = as<mat>(starting_values["B"]);
  mat   aux_A       = as<mat>(starting_values["A"]);
  mat   aux_sigma2  = as<mat>(starting_values["sigma2"]);     // NxM
  mat   aux_sigma(N, T);
  cube  aux_PR_TR   = as<cube>(starting_values["PR_TR"]);     // MxMxN
  mat   aux_pi_0    = as<mat>(starting_values["pi_0"]);       // MxN
  cube  aux_xi      = as<cube>(starting_values["xi"]);        // MxTxN
  mat   aux_hyper   = as<mat>(starting_values["hyper"]);
  mat   aux_lambda  = as<mat>(starting_values["lambda"]);
  mat   aux_lambda_sqrt(N, T, fill::ones);
  mat   aux_hetero(N, T, fill::ones);
  vec   aux_df      = as<vec>(starting_values["df"]);
  mat   U;
  
  const int   M     = aux_PR_TR.n_rows;
  
  const int   SS     = floor(S / thin);
  
  cube  posterior_B(N, N, SS);
  cube  posterior_A(N, K, SS);
  cube  posterior_sigma2(N, M, SS);
  field<cube> posterior_PR_TR(SS);                // (S)(M, M, N) 
  cube  posterior_pi_0(M, N, SS);
  field<cube> posterior_xi(SS);                   // (S)(M, T, N)
  cube  posterior_hyper(2 * N + 1, 2, SS);
  cube  posterior_sigma(N, T, SS);
  cube  posterior_lambda(N, T, SS);
  mat   posterior_df(N, SS);
  
  int   ss = 0;
  for (int t=0; t<T; t++) {
    for (int n=0; n<N; n++) {
      aux_sigma(n,t)  = sqrt( aux_sigma2(n, aux_xi.slice(n).col(t).index_max()) );
    }
  }
  
  aux_hetero = aux_sigma % aux_lambda_sqrt;
  
  // the initial value for the adaptive_scale is set to the negative inverse of 
  // Hessian for the posterior log_kenel for df evaluated at df = 30
  double  adaptive_scale_init = abs(pow(0.25 * T * R::psigamma(15, 1) - T * 29 * pow(28, -2) - 2 * pow(29, -2), -1));
  vec     adaptive_scale(N, fill::value(adaptive_scale_init));
  
  for (int s=0; s<S; s++) {
    
    // Increment progress bar
    if (any(prog_rep_points == s)) p.increment();
    // Check for user interrupts
    if (s % 200 == 0) checkUserInterrupt();
    
    // sample aux_df and aux_lambda
    if ( !normal ) {
      List df_tmp     = sample_df ( aux_df, adaptive_scale, aux_lambda, s, adptive_alpha_gamma );
      aux_df          = as<vec>(df_tmp["aux_df"]);
      adaptive_scale  = as<vec>(df_tmp["adaptive_scale"]);
      
      U               = aux_B * (Y - aux_A * X) / aux_sigma;
      aux_lambda      = sample_lambda ( aux_df, U );
      aux_lambda_sqrt = sqrt(aux_lambda);
      aux_hetero      = aux_sigma % aux_lambda_sqrt;
    }
    
    // sample aux_hyper
    // aux_hyper         = sample_hyperparameters(aux_hyper, aux_B, aux_A, VB, VA, prior);
    
    // sample aux_B
    aux_B             = sample_B_heterosk1(aux_B, aux_A, aux_hyper, aux_hetero, Y, X, prior, VB);
    
    // sample aux_A
    aux_A             = sample_A_heterosk1(aux_A, aux_B, aux_hyper, aux_hetero, Y, X, prior, VA);
      
    // sample aux_xi
    U                 = aux_B * (Y - aux_A * X) / aux_sigma;
    aux_xi            = sample_Markov_process_hmsh(aux_xi, U, aux_sigma2, aux_PR_TR, aux_pi_0, finiteM);
    
    // sample aux_PR_TR
    for (int n=0; n<N; n++) {
      List aux_PR_tmp     = sample_transition_probabilities(aux_PR_TR.slice(n), aux_pi_0.col(n), aux_xi.slice(n), prior, MSnotMIX);
      aux_PR_TR.slice(n)  = as<mat>(aux_PR_tmp["PR_TR"]);
      aux_pi_0.col(n)     = as<vec>(aux_PR_tmp["pi_0"]);
    }

    // sample aux_sigma2
    U                 = aux_B * (Y - aux_A * X) / aux_lambda_sqrt;
    aux_sigma2        = sample_variances_hmsh(aux_sigma2, U, aux_xi, prior);
    
    for (int t=0; t<T; t++) {
      for (int n=0; n<N; n++) {
        aux_sigma(n,t)  = sqrt( aux_sigma2(n, aux_xi.slice(n).col(t).index_max()) );
      }
    }
    aux_hetero      = aux_sigma % aux_lambda_sqrt;
    
    if (s % thin == 0) {
      posterior_B.slice(ss)      = aux_B;
      posterior_A.slice(ss)      = aux_A;
      posterior_sigma2.slice(ss) = aux_sigma2;
      posterior_PR_TR(ss)        = aux_PR_TR;
      posterior_pi_0.slice(ss)   = aux_pi_0;
      posterior_xi(ss)           = aux_xi;
      posterior_hyper.slice(ss)  = aux_hyper;
      posterior_sigma.slice(ss)  = aux_sigma;
      posterior_lambda.slice(ss) = aux_lambda;
      posterior_df.col(ss)       = aux_df;
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
      _["hyper"]    = aux_hyper,
      _["sigma"]    = aux_sigma,
      _["lambda"]   = aux_lambda,
      _["df"]       = aux_df
    ),
    _["posterior"]  = List::create(
      _["B"]        = posterior_B,
      _["A"]        = posterior_A,
      _["sigma2"]   = posterior_sigma2,
      _["PR_TR_cpp"]  = posterior_PR_TR,
      _["pi_0"]     = posterior_pi_0,
      _["xi_cpp"]   = posterior_xi,
      _["hyper"]    = posterior_hyper,
      _["sigma"]    = posterior_sigma,
      _["lambda"]   = posterior_lambda,
      _["df"]       = posterior_df
    )
  );
} // END bsvar_msh

