
#include <RcppArmadillo.h>
#include <RcppTN.h>

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::mat sample_lambda (
    const arma::vec&    aux_df,     // Nx1
    const arma::mat&    aux_B,      // NxN
    const arma::mat&    aux_A,      // NxK
    const arma::mat&    Y,          // NxT
    const arma::mat&    X           // KxT
) {
  const int N           = Y.n_rows;
  const int T           = Y.n_cols;
  
  mat       U           = aux_B * ( Y - aux_A * X );
  
  mat       s_lambda    = pow(U, 2);
  s_lambda.each_col()  += aux_df + 2;
  vec       nu_lambda   = aux_df + 1;
  
  mat       aux_lambda(N, T);
  for (int n=0; n<N; n++) {
    vec draw            = as<vec>(Rcpp::rchisq(T, nu_lambda(n)));
    aux_lambda.row(n)   = s_lambda.row(n) / draw.t();
  }
  
  return aux_lambda;
} // END sample_lambda


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
double log_kernel_df (
    const double&       aux_df,
    const arma::rowvec&    aux_lambda  // Tx1
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
Rcpp::List sample_df (
    arma::vec&        aux_df,             // Nx1
    arma::vec&        adaptive_scale,     // Nx1
    const arma::mat&  aux_lambda,         // NxT
    const int&        s,                  // MCMC iteration
    const arma::vec&  adptive_alpha_gamma // 2x1 vector with target acceptance rate and step size
) {
  int N = aux_df.n_elem;
  vec aux_df_star(N);
  vec alpha(N, fill::ones);
  
  // by sampling from truncated normal it is assumed that the asymmetry from truncation 
  // is negligible for alpha computation
  for (int n = 0; n < N; n++){
    aux_df_star(n)        = RcppTN::rtn1( aux_df(n), adaptive_scale(n), 0, R_PosInf );
    double kernel_ratio   = exp( log_kernel_df(aux_df_star(n), aux_lambda.row(n)) - log_kernel_df(aux_df(n), aux_lambda.row(n)) );
    
    if ( kernel_ratio < 1 ) alpha(n) = kernel_ratio;
    if ( R::runif(0, 1) < alpha(n) ) {
      aux_df(n) = aux_df_star(n);
    }
    if (s > 1) {
      adaptive_scale(n) = exp( log(adaptive_scale(n)) + 0.5 * log( 1 + pow(s, - adptive_alpha_gamma(1)) * (alpha(n) - adptive_alpha_gamma(0))) );
    }
  } // END n loop

  return List::create(
    _["aux_df"] = aux_df,
    _["adaptive_scale"] = adaptive_scale
  );
} // END sample_df
