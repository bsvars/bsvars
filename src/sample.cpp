
#include <RcppArmadilloExtensions/sample.h>
#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"

#include "utils.h"

using namespace Rcpp;
using namespace arma;

//---------------------------------------------------------------------------------------------------
// a transformed sample implementation taken from Rcpp Gallery:
// https://gallery.rcpp.org/articles/using-the-Rcpp-based-sample-implementation/
// fixed to one draw, sampling without replacement, and changed output type to int
// IMPORTANT: always #include <RcppArmadilloExtensions/sample.h>
//---------------------------------------------------------------------------------------------------
int csample_num1 (
    NumericVector x,
    NumericVector prob = NumericVector::create()
) {
  bool replace = false;
  NumericVector ret = Rcpp::RcppArmadillo::sample(x, 1, replace, prob);
  int out           = ret(0);
  return out;
} // END csample_num1




// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List logSDDR_homoskedasticity (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X,          // KxT explanatory variables
    const bool              sample_s_ = true
) {
  
  // read inputs
  const cube    posterior_B     = as<cube>(posterior["B"]);
  const cube    posterior_A     = as<cube>(posterior["A"]);
  const cube    posterior_h     = as<cube>(posterior["h"]);
  const cube    posterior_S     = as<cube>(posterior["S"]);
  const mat     posterior_sigma2_omega  = as<mat>(posterior["sigma2_omega"]);
  const mat     posterior_s_    = as<mat>(posterior["s_"]);
  
  const double  prior_a_        = as<double>(prior["sv_a_"]);
  const double  prior_s_        = as<double>(prior["sv_s_"]);
  
  const int     S               = posterior_sigma2_omega.n_cols;
  const int     T               = Y.n_cols;
  const int     N               = Y.n_rows;
  
  // fixed values for auxiliary mixture
  const NumericVector alpha_s = NumericVector::create(1.92677,1.34744,0.73504,0.02266,0-0.85173,-1.97278,-3.46788,-5.55246,-8.68384,-14.65000);
  const NumericVector sigma_s = NumericVector::create(0.11265,0.17788,0.26768,0.40611,0.62699,0.98583,1.57469,2.54498,4.16591,7.33342);
  
  if ( prior_a_ <= 0.5 ) {
    stop("'prior$sv_a_' must be greater than 0.5");
  }
  
  // compute denominator
  double inv_sqrt_s_      = 0.0;
  vec sample_prior_s_(S);
  if ( sample_s_ ) {
    sample_prior_s_       = prior_s_/as<vec>(Rcpp::rchisq(S, 3));
    inv_sqrt_s_           = as_scalar(mean(pow(sample_prior_s_, -0.5)));
  } else {
    inv_sqrt_s_           = pow(prior_s_, -0.5);
  }
  double  log_denominator     = - 0.5 * log(2 * M_PI) + log(inv_sqrt_s_) - log(pow(prior_a_, 2) - 0.25) + R::lgammafn(prior_a_ + 1.5) - R::lgammafn(prior_a_);
  
  // compute numerator
  mat     log_numerator_s(N, S);
  for (int s = 0; s < S; s++) {
    for (int n = 0; n < N; n++) {
      mat     residuals       = log(square(posterior_B.slice(s) * (Y - posterior_A.slice(s) * X)));
      
      rowvec  alpha_S(T);
      vec     sigma_S_inv(T);
      for (int t = 0; t < T; t++) {
        rowvec  post_S        = posterior_S.slice(s).row(n);
        alpha_S.col(t)        = alpha_s(post_S(t));
        sigma_S_inv.row(t)    = 1/sigma_s(post_S(t));
      } // END t loop
      
      double  V_omega         = pow(as_scalar(posterior_h.slice(s).row(n) * diagmat(sigma_S_inv) * trans(posterior_h.slice(s).row(n))) + pow(posterior_sigma2_omega(n, s), -1), -1);
      double  omega_bar       = V_omega * as_scalar(posterior_h.slice(s).row(n) * diagmat(sigma_S_inv) * trans(residuals.row(n) - alpha_S));
      log_numerator_s(n, s)   = R::dnorm(0, omega_bar, sqrt(V_omega), true);
    } // END n loop
  } // END s loop
  
  // compute the log of the mean numerator exp(log_numerator)
  vec log_numerator           = log_mean(log_numerator_s);
  
  
  mat se_components(N, 30);
  int nn                      = floor(S/30);
  NumericVector seq_1S        = wrap(seq_len(S) - 1);
  
  for (int i=0; i<30; i++) {
    // sub-sampling elements' indicators
    NumericVector indi_tmp    = Rcpp::RcppArmadillo::sample(seq_1S, nn, false, NumericVector::create());
    uvec          indi        = as<uvec>(indi_tmp);
    
    // log denominator
    inv_sqrt_s_      = 0.0;
    if ( sample_s_ ) {
      vec sample_prior_s_i    = prior_s_/sample_prior_s_.rows(indi);
      inv_sqrt_s_             = as_scalar(mean(pow(sample_prior_s_i, -0.5)));
    } else {
      inv_sqrt_s_             = pow(prior_s_, -0.5);
    }
    double log_denominator_i  = - 0.5 * log(2 * M_PI) + log(inv_sqrt_s_) - log(pow(prior_a_, 2) - 0.25) + R::lgammafn(prior_a_ + 1.5) - R::lgammafn(prior_a_);
    
    // log numerator
    se_components.col(i)      = log_mean(log_numerator_s.cols(indi)) - log_denominator_i;
  } // END i loop
  
  vec logSDDR_se              = stddev(se_components, 1, 1);
  
  // compute the standard error 
  return List::create(
    _["logSDDR"]     = log_numerator - log_denominator,
    _["log_SDDR_se"] = logSDDR_se,
    _["components"]  = List::create(
      _["log_denominator"]    = log_denominator,
      _["log_numerator"]      = log_numerator,
      _["log_numerator_s"]    = log_numerator_s,
      _["se_components"]      = se_components
    )
  );
} // END logSDDR_homoskedasticity
