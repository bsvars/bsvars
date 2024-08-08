
#include <RcppArmadillo.h>
#include <RcppTN.h>

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::vec sample_lambda (
    const double&       aux_df,
    const arma::mat&    aux_B,      // NxN
    const arma::mat&    aux_A,      // NxK
    const arma::mat&    Y,          // NxT
    const arma::mat&    X           // KxT
) {
  const int N           = Y.n_rows;
  const int T           = Y.n_cols;
  
  mat       U           = aux_B * ( Y - aux_A * X );
  vec       s_lambda    = aux_df + 2 + trans(sum( pow(U, 2) ));
  double    nu_lambda   = aux_df + N;
  vec       aux_lambda  = s_lambda / as<vec>(Rcpp::rchisq(T, nu_lambda));
  
  return aux_lambda;
} // END sample_lambda


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
double log_kernel_df (
    const double&       aux_df,
    const arma::vec&    aux_lambda  // Tx1
) {
  
  const int T   = aux_lambda.n_elem;
  double lk_df  = 0;
  lk_df   -= T * lgamma(0.5 * aux_df);                        // lambda prior
  lk_df   += 0.5 * T * aux_df * log(0.5 * (aux_df + 2));      // lambda prior
  lk_df   -= 0.5 * (aux_df + 2) * accu(log(aux_lambda));      // lambda prior
  lk_df   -= 0.5 * (aux_df + 2) * accu(pow(aux_lambda, -1));  // lambda prior
  lk_df   -= 2 * log(aux_df + 1);                             // df prior
  
  return lk_df;
} // END log_kernel_df


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::vec sample_df (
    double&           aux_df,
    double&           adaptive_scale,
    const arma::vec&  aux_lambda,         // Tx1
    const int&        s,                  // MCMC iteration
    const arma::vec&  adptive_alpha_gamma // 2x1 vector with target acceptance rate and step size
) {
  
  // by sampling from truncated normal it is assumed that the asymmetry from truncation 
  // is negligible for alpha computation
  double aux_df_star  = RcppTN::rtn1( aux_df, adaptive_scale, 0, R_PosInf );
  
  double alpha        = 1;
  double kernel_ratio = exp( log_kernel_df(aux_df_star, aux_lambda) - log_kernel_df(aux_df, aux_lambda) );
  if ( kernel_ratio < 1 ) alpha = kernel_ratio;
  
  if ( R::runif(0, 1) < alpha ) {
    aux_df = aux_df_star;
  }
  
  if (s > 1) {
    adaptive_scale = exp( log(adaptive_scale) + 0.5 * log( 1 + pow(s, - adptive_alpha_gamma(1)) * (alpha - adptive_alpha_gamma(0))) );
  }
  
  vec out = {aux_df, adaptive_scale};
  return out;
} // END sample_df
