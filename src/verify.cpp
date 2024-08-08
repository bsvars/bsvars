#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"

#include "utils.h"

using namespace Rcpp;
using namespace arma;


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List verify_volatility_sv_cpp (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X,          // KxT explanatory variables
    const bool              sample_s_ = true
) {
  // computes the log of SDDR for homoskedasticity hypothesis omega_n = 0
  // see Lütkepohl, Shang, Uzeda, Woźniak (2013)
  
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
    sample_prior_s_       = prior_s_/chi2rnd( 3, S );
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
  
  int   nse_subsamples        = 30;
  mat   se_components(N, nse_subsamples);
  int   nn                    = floor(S/nse_subsamples);
  uvec  seq_1S                = as<uvec>(wrap(seq_len(S) - 1));
  
  vec logSDDR_se(N);
  
  if ( S >= 60 ) {
    for (int i=0; i<nse_subsamples; i++) {
      // sub-sampling elements' indicators
      uvec          indi        = seq_1S.subvec(i*nn, (i+1)*nn-1);
      
      // log denominator
      inv_sqrt_s_      = 0.0;
      if ( sample_s_ ) {
        vec sample_prior_s_i    = prior_s_/sample_prior_s_.rows(indi);
        inv_sqrt_s_             = as_scalar(mean(pow(sample_prior_s_i, -0.5)));
      } else {
        inv_sqrt_s_             = pow(prior_s_, -0.5);
      }
      double log_denominator_i  = - 0.5 * log(2 * M_PI) + log(inv_sqrt_s_) - log(pow(prior_a_, 2) - 0.25) + R::lgammafn(prior_a_ + 1.5) - R::lgammafn(prior_a_);
      se_components.col(i)      = log_mean(log_numerator_s.cols(indi)) - log_denominator_i;
    } // END i loop
    
    logSDDR_se              = stddev(se_components, 1, 1);
  } // END if
  
  // compute the standard error 
  return List::create(
    _["logSDDR"]     = log_numerator - log_denominator,
    _["logSDDR_se"]  = logSDDR_se,
    _["components"]  = List::create(
      _["log_denominator"]    = log_denominator,
      _["log_numerator"]      = log_numerator,
      _["log_numerator_s"]    = log_numerator_s,
      _["se_components"]      = se_components
    )
  );
} // END verify_volatility_cpp



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
double dig2dirichlet (
    const arma::rowvec&  x,                // M-vector of positive rv summing up to 1
    const arma::rowvec&  a,                // M-vector of positive parameters
    const arma::rowvec&  b,                // M-vector of positive parameters
    const bool  logarithm = true  
) {
  // density ordinate of ig2-based Dirichlet distribution
  double constant_ig2d    = lgamma(accu(a) / 2) - accu(lgamma(a  / 2)) - accu(log(b));
  double kernel_ig2d      = accu(0.5 * (a + 2) % (log(b) - log(x))) - ( 0.5 * accu(a) * log(accu(b / x)) );
  double out              = constant_ig2d + kernel_ig2d;
  
  if ( !logarithm ) {
    out =  exp(out);
  }
  return out;
} // END dig2dirichlet



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
double ddirichlet (
    const arma::rowvec&  x,                // M-vector of positive rv summing up to 1
    const arma::rowvec&  a,                // M-vector of positive parameters
    const bool  logarithm = true  
) {
  // density ordinate of Dirichlet distribution
  double constant_d       = lgamma(accu(a)) - accu(lgamma(a));
  double kernel_d         = accu((a - 1) % log(x));
  double out              = constant_d + kernel_d;
  
  if ( !logarithm ) {
    out =  exp(out);
  }
  return out;
} // END ddirichlet



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List verify_volatility_msh_cpp (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X          // KxT explanatory variables
) {
  
  cube  posterior_sigma2    = posterior["sigma2"];
  cube  posterior_B         = posterior["B"];
  cube  posterior_A         = posterior["A"];
  cube  posterior_xi        = posterior["xi"];
  
  const int   M             = posterior_xi.n_rows;
  const int   N             = posterior_B.n_rows;
  const int   T             = Y.n_cols;
  const int   S             = posterior_B.n_slices;
  
  rowvec      homoskedasticity_hypothesis(M, fill::ones);
  
  // compute denominator
  
  rowvec  prior_nu(M, fill::value(as<double>(prior["sigma_nu"])));
  rowvec  prior_s(M, fill::value(as<double>(prior["sigma_s"])));
  
  double  log_denominator = dig2dirichlet( homoskedasticity_hypothesis, prior_nu, prior_s );
  
  // compute numerator
  mat     log_numerator_s(N, S);
  for (int s = 0; s < S; s++) {
    for (int n = 0; n < N; n++) {
      
      rowvec posterior_nu   = sum(posterior_xi.slice(s), 1).t() + as<double>(prior["sigma_nu"]);
      
      mat posterior_s(N, M);
      posterior_s.fill(prior["sigma_s"]);
      for (int m=0; m<M; m++) {
        for (int t=0; t<T; t++) {
          if (posterior_xi(m,t,s)==1) {
            posterior_s.col(m) += square(posterior_B.slice(s) * (Y.col(t) - posterior_A.slice(s) * X.col(t)));
          }
        }
      }
      
      log_numerator_s(n,s)  = M * log(M) * dig2dirichlet( homoskedasticity_hypothesis, posterior_nu, posterior_s.row(n) );
      
    } // END n loop
  } // END s loop
  
  // compute the log of the mean numerator exp(log_numerator)
  vec log_numerator           = log_mean(log_numerator_s);
  
  // NSE computations
  int   nse_subsamples        = 30;
  mat   se_components(N, nse_subsamples);
  int   nn                    = floor(S/nse_subsamples);
  uvec  seq_1S                = as<uvec>(wrap(seq_len(S) - 1));
  
  vec logSDDR_se(N);
  
  if ( S >= 60 ) {
    for (int i=0; i<nse_subsamples; i++) {
      // sub-sampling elements' indicators
      uvec          indi        = seq_1S.subvec(i*nn, (i+1)*nn-1);
      se_components.col(i)      = log_mean(log_numerator_s.cols(indi)) - log_denominator;
    } // END i loop
    
    logSDDR_se              = stddev(se_components, 1, 1);
  } // END if
  
  // compute the standard error 
  return List::create(
    _["logSDDR"]     = log_numerator - log_denominator,
    _["logSDDR_se"]  = logSDDR_se,
    _["components"]  = List::create(
      _["log_denominator"]    = log_denominator,
      _["log_numerator"]      = log_numerator,
      _["log_numerator_s"]    = log_numerator_s,
      _["se_components"]      = se_components
    )
  );
} // END verify_volatility_msh_cpp


// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
double dmvnorm_chol_precision (
    const arma::rowvec&   x,  
    const arma::rowvec&   location,  
    const arma::mat&      chol_precision, 
    const bool            logarithm = true
) { 
  // computes the density of a multivariate normal distribution with location
  // the Cholesky decomposition of precision matrix
  
  const int     N       = x.n_elem; 
  const double  log2pi  = std::log(2.0 * M_PI);
  
  double val;
  double sign;
  log_det(val, sign, chol_precision);
  
  double    const_norm  = -0.5 * N * log2pi + val;
  
  vec       quad_tmp    = trans(x * chol_precision) - solve(chol_precision, trans(location));
  double    kernel_norm = -0.5 * as_scalar(trans(quad_tmp) * quad_tmp);
  double    out         = const_norm + kernel_norm;
  
  if ( !logarithm ) {
    out =  exp(out);
  }
  return out;
} // END dmvnorm_precision



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
double dmvnorm_mean_var (
    const arma::vec&      x,  
    const arma::vec&      mean,  
    const arma::mat&      var, 
    const bool            logarithm = true
) { 
  // computes the density of a multivariate normal distribution with location
  // the Cholesky decomposition of precision matrix
  
  const int     N       = x.n_elem; 
  const double  log2pi  = std::log(2.0 * M_PI);
  
  double val;
  double sign;
  log_det(val, sign, var);
  
  double    const_norm  = -0.5 * N * log2pi - 0.5 * val;
  mat       prec        = inv_sympd(var);
  vec       loc         = x - mean;
  double    kernel_norm = -0.5 * as_scalar(trans(loc) * prec * loc);
  double    out         = const_norm + kernel_norm;
  
  if ( !logarithm ) {
    out =  exp(out);
  }
  return out;
} // END dmvnorm_mean_var



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List verify_autoregressive_heterosk_cpp (
    const arma::mat&        hypothesis, // an NxK matrix of values under the null; value 999 stands for not verivied
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X           // KxT explanatory variables
) {
  
  cube    posterior_A         = posterior["A"];
  cube    posterior_B         = posterior["B"];
  cube    posterior_hyper     = posterior["hyper"];
  cube    posterior_sigma     = posterior["sigma"];
  
  double  prior_hyper_nu_A    = as<double>(prior["hyper_nu_A"]);
  double  prior_hyper_a_A     = as<double>(prior["hyper_a_A"]);
  double  prior_hyper_s_AA    = as<double>(prior["hyper_s_AA"]);
  double  prior_hyper_nu_AA   = as<double>(prior["hyper_nu_AA"]);
  
  mat     prior_A             = as<mat>(prior["A"]);
  mat     prior_A_V_inv       = as<mat>(prior["A_V_inv"]);
  
  const int N                 = posterior_A.n_rows;
  const int K                 = posterior_A.n_cols;
  const int S                 = posterior_A.n_slices;
  const int T                 = Y.n_cols;
  const double log2pi         = std::log(2.0 * M_PI);
  
  mat aux_sigma(N, T, fill::ones);
  
  mat prior_A_mean            = as<mat>(prior["A"]);
  mat prior_A_Vinv            = as<mat>(prior["A_V_inv"]);
  rowvec    zerosA(K);
  
  // intermediate output matrices
  double    log_numerator = 0;
  vec       log_numerator_n(N);
  mat       log_numerator_s(N, S);
  double    log_denominator = 0;
  vec       log_denominator_n(N);
  mat       log_denominator_s(N, S);
  double    logSDDR_se = 0;
  
  // for NSE computations
  int   nse_subsamples        = 30;
  rowvec    se_components(nse_subsamples);
  int   nn                    = floor(S/nse_subsamples);
  uvec  seq_1S                = as<uvec>(wrap(seq_len(S) - 1));
  
  // level 1 prior - no need for loop
  mat hyper_sample(2 * N + 1, S);
  hyper_sample.row(2 * N)     = trans(prior_hyper_s_AA / chi2rnd( prior_hyper_nu_AA, S ));
  
  for (int n=0; n<N; n++) {

    // investigate hypothesis
    uvec  indi                = find( hypothesis.row(n) != 999 );
    if ( indi.n_elem == 0 ) {
      continue;
    }
          
    for (int s=0; s<S; s++) {
      
      // compute denominator
      double gamma_draw       = randg( distr_param(prior_hyper_a_A, hyper_sample(2 * N, s)) );
      hyper_sample(N + n, s)  = gamma_draw;
      hyper_sample(n, s)      = gamma_draw / chi2rnd( prior_hyper_nu_A );
      
      double const_prior      = - 0.5 * indi.n_elem * ( log2pi + log(hyper_sample(n, s)) );
      rowvec hypothesis_n     = hypothesis.row(n) - prior_A.row(n);
      double kernel_prior     = - 0.5 * pow(hyper_sample(n, s), -1) * accu( pow( hypothesis_n.cols(indi), 2)  );
      
      log_denominator_s(n, s) = const_prior + kernel_prior;
      
      // compute numerator
      aux_sigma               = posterior_sigma.slice(s);
      vec   sigma_vectorised  = vectorise(aux_sigma);
      mat   A0                = posterior_A.slice(s);
      A0.row(n)               = zerosA;
      mat   aux_B             = posterior_B.slice(s);
      vec   zn                = vectorise( aux_B * (Y - A0 * X) );
      mat   zn_sigma          = zn / sigma_vectorised;
      mat   Wn                = kron( trans(X), aux_B.col(n) );
      mat   Wn_sigma          = Wn.each_col() / sigma_vectorised;
      
      mat     precision_tmp   = (pow(posterior_hyper(n,1,s), -1) * prior_A_Vinv) + trans(Wn_sigma) * Wn_sigma;
      precision_tmp           = 0.5 * (precision_tmp + precision_tmp.t());
      rowvec  location_tmp    = prior_A_mean.row(n) * (pow(posterior_hyper(n,1,s), -1) * prior_A_Vinv) + trans(zn_sigma) * Wn_sigma;
      mat     variance_tmp    = inv_sympd(precision_tmp);
      vec     mean_tmp        = variance_tmp * location_tmp.t();
      mat     variance_marg   = variance_tmp.submat(indi, indi);
      vec     mean_marg       = mean_tmp.rows(indi);
      vec     hypothesisn     = trans(hypothesis.row(n));
      
      log_numerator_s(n,s)    = dmvnorm_mean_var( hypothesisn.rows(indi), mean_marg, variance_marg, true );
    } // END s loop
    
    // wrap up the SDDR computation
    log_numerator_n(n)        = as_scalar(log_mean(log_numerator_s.row(n)));
    log_denominator_n(n)      = as_scalar(log_mean(log_denominator_s.row(n)));
    
    log_numerator            += log_numerator_n(n);
    log_denominator          += log_denominator_n(n);
    
    // NSE computations
    if ( S >= 60) {
      for (int i=0; i<nse_subsamples; i++) {
        // sub-sampling elements' indicators
        uvec  indi              = seq_1S.subvec(i*nn, (i+1)*nn-1);
        
        rowvec log_numerator_s_subsample      = log_numerator_s.row(n);
        rowvec log_denominator_s_subsample    = log_denominator_s.row(n);
        
        se_components(i)       += as_scalar(log_mean(log_numerator_s_subsample.cols(indi)) 
                                              - log_mean(log_denominator_s_subsample.cols(indi)));
      } // END i loop
    } // END if  
  } // END n loop
  
  if ( S >= 60) {
    logSDDR_se           = stddev(se_components, 1);
  } // END if  
  
  return List::create(
    _["logSDDR"]     = log_numerator - log_denominator,
    _["log_SDDR_se"] = logSDDR_se,
    _["components"]  = List::create(
      _["log_denominator"]    = log_denominator,
      _["log_numerator"]      = log_numerator,
      _["log_numerator_s"]    = log_numerator_s,
      _["log_denominator_s"]  = log_denominator_s,
      _["se_components"]      = se_components
    )
  );
} // END verify_autoregressive_heterosk_cpp



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
Rcpp::List verify_autoregressive_homosk_cpp (
    const arma::mat&        hypothesis, // an NxK matrix of values under the null; value 999 stands for not verivied
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X           // KxT explanatory variables
) {
  
  cube    posterior_A         = posterior["A"];
  cube    posterior_B         = posterior["B"];
  cube    posterior_hyper     = posterior["hyper"];
  
  double  prior_hyper_nu_A    = as<double>(prior["hyper_nu_A"]);
  double  prior_hyper_a_A     = as<double>(prior["hyper_a_A"]);
  double  prior_hyper_s_AA    = as<double>(prior["hyper_s_AA"]);
  double  prior_hyper_nu_AA   = as<double>(prior["hyper_nu_AA"]);
  
  mat     prior_A             = as<mat>(prior["A"]);
  mat     prior_A_V_inv       = as<mat>(prior["A_V_inv"]);
  
  const int N                 = posterior_A.n_rows;
  const int K                 = posterior_A.n_cols;
  const int S                 = posterior_A.n_slices;
  const double log2pi         = std::log(2.0 * M_PI);
  
  mat prior_A_mean            = as<mat>(prior["A"]);
  mat prior_A_Vinv            = as<mat>(prior["A_V_inv"]);
  rowvec    zerosA(K);
  
  // intermediate output matrices
  double    log_numerator = 0;
  vec       log_numerator_n(N);
  mat       log_numerator_s(N, S);
  double    log_denominator = 0;
  vec       log_denominator_n(N);
  mat       log_denominator_s(N, S);
  double    logSDDR_se = 0;
  
  // for NSE computations
  int   nse_subsamples        = 30;
  rowvec    se_components(nse_subsamples);
  int   nn                    = floor(S/nse_subsamples);
  uvec  seq_1S                = as<uvec>(wrap(seq_len(S) - 1));
  
  // level 1 prior - no need for loop
  mat hyper_sample(2 * N + 1, S);
  hyper_sample.row(2 * N)     = trans(prior_hyper_s_AA / chi2rnd( prior_hyper_nu_AA, S ));
  
  for (int n=0; n<N; n++) {
    
    // investigate hypothesis
    uvec  indi                = find( hypothesis.row(n) != 999 );
    if ( indi.n_elem == 0 ) {
      continue;
    }
    
    for (int s=0; s<S; s++) {
      
      // compute denominator
      double gamma_draw       = randg( distr_param(prior_hyper_a_A, hyper_sample(2 * N, s)) );
      hyper_sample(N + n, s)  = gamma_draw;
      hyper_sample(n, s)      = gamma_draw / chi2rnd( prior_hyper_nu_A );
      
      double const_prior      = - 0.5 * indi.n_elem * ( log2pi + log(hyper_sample(n, s)) );
      rowvec hypothesis_n     = hypothesis.row(n) - prior_A.row(n);
      double kernel_prior     = - 0.5 * pow(hyper_sample(n, s), -1) * accu( pow( hypothesis_n.cols(indi), 2)  );
      
      log_denominator_s(n, s) = const_prior + kernel_prior;
      
      // compute numerator
      mat   A0          = posterior_A.slice(s);
      A0.row(n)         = zerosA;
      vec   zn          = vectorise( posterior_B.slice(s) * (Y - A0 * X) );
      mat   Wn          = kron( trans(X), posterior_B.slice(s).col(n) );
      
      mat     precision_tmp   = (pow(posterior_hyper(n,1,s), -1) * prior_A_Vinv) + trans(Wn) * Wn;
      rowvec  location_tmp    = prior_A_mean.row(n) * (pow(posterior_hyper(n,1,s), -1) * prior_A_Vinv) + trans(zn) * Wn;
      mat     variance_tmp    = inv_sympd(precision_tmp);
      vec     mean_tmp        = variance_tmp * location_tmp.t();
      mat     variance_marg   = variance_tmp.submat(indi, indi);
      vec     mean_marg       = mean_tmp.rows(indi);
      vec     hypothesisn     = trans(hypothesis.row(n));
        
      log_numerator_s(n,s)    = dmvnorm_mean_var( hypothesisn.rows(indi), mean_marg, variance_marg, true );
    } // END s loop
    
    // wrap up the SDDR computation
    log_numerator_n(n)        = as_scalar(log_mean(log_numerator_s.row(n)));
    log_denominator_n(n)      = as_scalar(log_mean(log_denominator_s.row(n)));
    
    log_numerator            += log_numerator_n(n);
    log_denominator          += log_denominator_n(n);
    
    // NSE computations
    if ( S >= 60 ) {
      for (int i=0; i<nse_subsamples; i++) {
        // sub-sampling elements' indicators
        uvec  indi              = seq_1S.subvec(i*nn, (i+1)*nn-1);
        
        rowvec log_numerator_s_subsample      = log_numerator_s.row(n);
        rowvec log_denominator_s_subsample    = log_denominator_s.row(n);
        
        se_components(i)       += as_scalar(log_mean(log_numerator_s_subsample.cols(indi)) 
                                              - log_mean(log_denominator_s_subsample.cols(indi)));
      } // END i loop
    } // END if
  } // END n loop
  
  if ( S >= 60 ) {
    logSDDR_se           = stddev(se_components, 1);
  } // END if
  
  return List::create(
    _["logSDDR"]     = log_numerator - log_denominator,
    _["log_SDDR_se"] = logSDDR_se,
    _["components"]  = List::create(
      _["log_denominator"]    = log_denominator,
      _["log_numerator"]      = log_numerator,
      _["log_numerator_s"]    = log_numerator_s,
      _["log_denominator_s"]  = log_denominator_s,
      _["se_components"]      = se_components
    )
  );
} // END verify_autoregressive_homosk_cpp
