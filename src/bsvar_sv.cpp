
#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "sample_ABhyper.h"
#include "sv.h"

using namespace Rcpp;
using namespace arma;



//' @title Bayesian estimation of a Structural Vector Autoregression with 
//' Stochastic Volatility heteroskedasticity via Gibbs sampler
//'
//' @description Estimates the SVAR with Stochastic Volatility (SV) heteroskedasticity proposed by Lütkepohl, Shang, Uzeda, and Woźniak (2022).
//' Implements the Gibbs sampler proposed by Waggoner & Zha (2003)
//' for the structural matrix \code{B} and the equation-by-equation sampler by Chan, Koop, & Yu (2021)
//' for the autoregressive slope parameters \code{A}. Additionally, the parameter matrices \code{A} and \code{B}
//' follow a Minnesota prior and generalised-normal prior distributions respectively with the matrix-specific
//' overall shrinkage parameters estimated thanks to a 3-level hierarchical prior distribution. The SV model 
//' is estimated in a non-centred parameterisation using a range of techniques including: 
//' simulation smoother, auxiliary mixture, ancillarity-sufficiency interweaving strategy, 
//' and generalised inverse Gaussian distribution summarised by Kastner & Frühwirth-Schnatter (2014). 
//' See section \bold{Details} for the model equations.
//' 
//' @details 
//' The heteroskedastic SVAR model is given by the reduced form equation:
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("Y = AX + E")}
//' where \code{Y} is an \code{NxT} matrix of dependent variables, \code{X} is a \code{KxT} matrix of explanatory variables, 
//' \code{E} is an \code{NxT} matrix of reduced form error terms, and \code{A} is an \code{NxK} matrix of autoregressive slope coefficients and parameters on deterministic terms in \code{X}.
//' 
//' The structural equation is given by
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("BE=U")}
//' where \code{U} is an \code{NxT} matrix of structural form error terms, and
//' \code{B} is an \code{NxN} matrix of contemporaneous relationships between structural shocks in the columns of matrix \code{U}.
//' 
//' Finally, the structural shocks, \code{U}, are temporally and contemporaneously independent and jointly normally distributed with zero mean.
//' The conditional variance of the \code{n}th shock at time \code{t} is given by:
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("Var_{t-1}[u_{n.t}] = exp(w_n h_{n.t})")}
//' where \code{w_n} is the estimated conditional standard deviation of the log-conditional variance
//' and the log-volatility process \code{h_{n.t}} follows an autoregressive process:
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("h_{n.t} = g_n h_{n.t-1} + v_{n.t}")}
//' where \code{h_{n.0}=0}, \code{g_n} is an autoregressive parameter and \code{v_{n.t}} is a standard normal error term.
//' 
//' @param S a positive integer, the number of posterior draws to be generated
//' @param Y an \code{NxT} matrix, the matrix containing \code{T} observations on \code{N} dependent time series variables
//' @param X a \code{KxT} matrix, the matrix containing \code{T} observations on \code{K = N*p+d} regressors including \code{p} lags of dependent variables and \code{d} deterministic terms
//' @param prior a list containing the following elements
//' \describe{
//'  \item{A}{an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \code{A}}
//'  \item{A_V_inv}{a \code{KxK} precision matrix of the normal prior distribution for each of the row of the parameter matrix \code{A}. This precision matrix is equation invariant.}
//'  \item{B_V_inv}{an \code{NxN} precision matrix of the generalised-normal prior distribution for the structural matrix \code{B}. This precision matrix is equation invariant.}
//'  \item{B_nu}{a positive integer greater of equal than \code{N}, a shape parameter of the generalised-normal prior distribution for the structural matrix \code{B}}
//'  \item{hyper_nu}{a positive scalar, the shape parameter of the inverted-gamma 2 prior distribution for the two overall shrinkage parameters for matrices \code{B} and \code{A}}
//'  \item{hyper_a}{a positive scalar, the shape parameter of the gamma prior for the two overall shrinkage parameters}
//'  \item{hyper_V}{a positive scalar,  the shape parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
//'  \item{hyper_S}{a positive scalar,  the scale parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
//'  \item{sv_a_}{a positive scalar, the shape parameter of the gamma prior in the hierarchial prior for \code{sigma2_omega}}
//'  \item{sv_s_}{a positive scalar, the scale parameter of the gamma prior in the hierarchial prior for \code{sigma2_omega}}
//' }
//' @param VB a list of \code{N} matrices determining the unrestricted elements of matrix \code{B}
//' @param starting_values a list containing the following elements:
//' \describe{
//'  \item{A}{an \code{NxK} matrix of starting values for the parameter \code{A}}
//'  \item{B}{an \code{NxN} matrix of starting values for the parameter \code{B}}
//'  \item{hyper}{a \code{5}-vector of starting values for the shrinkage hyper-parameters of the hierarchical prior distribution}
//'  \item{h}{an \code{NxT} matrix with the starting values of the log-volatility processes}
//'  \item{rho}{an \code{N}-vector with values of SV autoregressive parameters}
//'  \item{omega}{an \code{N}-vector with values of SV process conditional standard deviations}
//'  \item{S}{an \code{NxT} integer matrix with the auxiliary mixture component indicators}
//'  \item{sigma2_omega}{an \code{N}-vector with variances of the zero-mean normal prior for \code{omega}}
//'  \item{s_}{a positive scalar with the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
//' }
//' @param sample_s_ a logical value. If \code{TRUE} the hyper-parameter \code{s_} is estimated
//' 
//' @return A list containing two elements:
//' 
//'  \code{posterior} a list with a collection of \code{S} draws from the posterior distribution generated via Gibbs sampler containing:
//'  \describe{
//'  \item{A}{an \code{NxKxS} array with the posterior draws for matrix \code{A}}
//'  \item{B}{an \code{NxNxS} array with the posterior draws for matrix \code{B}}
//'  \item{hyper}{a \code{5xS} matrix with the posterior draws for the hyper-parameters of the hierarchical prior distribution}
//'  \item{h}{an \code{NxTxS} array with the posterior draws of the log-volatility processes}
//'  \item{rho}{an \code{NxS} matrix with the posterior draws of SV autoregressive parameters}
//'  \item{omega}{an \code{NxS} matrix with the posterior draws of SV process conditional standard deviations}
//'  \item{S}{an \code{NxTxS} array with the posterior draws of the auxiliary mixture component indicators}
//'  \item{sigma2_omega}{an \code{NxS} matrix with the posterior draws of the variances of the zero-mean normal prior for \code{omega}}
//'  \item{s_}{an \code{S}-vector with the posterior draws of the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
//' }
//' 
//' \code{last_draw} a list with the last draw of the simulation (to be provided as \code{starting_values} to the follow-up run of \code{bsvar}) containing the following objects:
//' \describe{
//'  \item{A}{an \code{NxK} matrix with the last MCMC draw of the parameter matrix \code{A}}
//'  \item{B}{an \code{NxN} matrix with the last MCMC draw of the parameter matrix \code{B}}
//'  \item{hyper}{a \code{5}-vector with the last MCMC draw of the hyper-parameter of the hierarchical prior distribution}
//'  \item{h}{an \code{NxT} matrix with last MCMC draw of the log-volatility processes}
//'  \item{rho}{an \code{N}-vector with last MCMC draw of SV autoregressive parameters}
//'  \item{omega}{an \code{N}-vector with last MCMC draw of SV process conditional standard deviations}
//'  \item{S}{an \code{NxT} matrix with last MCMC draw of the auxiliary mixture component indicators}
//'  \item{sigma2_omega}{an \code{N}-vector with last MCMC draw of the variances of the zero-mean normal prior for \code{omega}}
//'  \item{s_}{a scalar with the last MCMC draw of the scale of the gamma prior of the hierarchical prior for \code{sigma2_omega}}
//'  }
//'
//' @seealso [normalisation_wz2003()], [logSDDR_homoskedasticity()]
//'
//' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
//' 
//' @references The model, prior distributions, and estimation algorithms were proposed by
//' 
//' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2022) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference.
//' 
//' Sampling from the generalised-normal full conditional posterior distribution of matrix \code{B} is implemented using the Gibbs sampler by:
//' 
//' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control}, \bold{28}, 349--366, \doi{https://doi.org/10.1016/S0165-1889(02)00168-9}.
//'
//' Sampling from the multivariate normal full conditional posterior distribution of each of the \code{A} matrix row is implemented using the sampler by:
//' 
//' Chan, J.C.C., Koop, G, and Yu, X. (2021) Large Order-Invariant Bayesian VARs with Stochastic Volatility.
//' 
//' Many of the techniques employed for the estimation of the Stochastic Volatility model 
//' are summarised by:
//' 
//' Kastner, G. and Frühwirth-Schnatter, S. (2014) Ancillarity-Sufficiency Interweaving Strategy (ASIS) for Boosting MCMC 
//' Estimation of Stochastic Volatility Models. \emph{Computational Statistics & Data Analysis}, \bold{76}, 408--423, 
//' \doi{10.1016/j.csda.2013.01.002}.
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bsvar_sv (
    const int&                    S,          // No. of posterior draws
    const arma::mat&              Y,          // NxT dependent variables
    const arma::mat&              X,          // KxT explanatory variables
    const Rcpp::List&             prior,      // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,        // restrictions on B0
    const Rcpp::List&             starting_values, 
    const bool                    sample_s_ = true
) {
  // Progress bar setup
  vec prog_rep_points = arma::round(arma::linspace(0, S, 50));
  Rcout << "**************************************************|" << endl;
  Rcout << " Gibbs sampler for the SVAR-SV model              |" << endl;
  Rcout << "**************************************************|" << endl;
  Rcout << " Progress of the MCMC simulation for " << S << " draws" << endl;
  Rcout << " Press control+c to interrupt the computations" << endl;
  Rcout << "**************************************************|" << endl;
  Progress p(50, true);
  
  const int   T     = Y.n_cols;
  const int   N     = Y.n_rows;
  const int   K     = X.n_rows;
  
  mat   aux_B       = as<mat>(starting_values["B"]);
  mat   aux_A       = as<mat>(starting_values["A"]);
  vec   aux_hyper   = as<vec>(starting_values["hyper"]);  // 5x1 (gamma_0, gamma_+, s_0, s_+, s_)
  mat   aux_h       = as<mat>(starting_values["h"]);
  vec   aux_rho     = as<vec>(starting_values["rho"]);
  vec   aux_omega   = as<vec>(starting_values["omega"]);
  umat  aux_S       = as<umat>(starting_values["S"]);
  vec   aux_sigma2_omega = as<vec>(starting_values["sigma2_omega"]);
  vec   aux_s_      = as<vec>(starting_values["s_"]);
  mat   aux_sigma(N, T);
  
  for (int n=0; n<N; n++) {
    aux_sigma.row(n) = exp(0.5 * aux_omega(n) * aux_h.row(n));
  }
  
  cube  posterior_B(N, N, S);
  cube  posterior_A(N, K, S);
  mat   posterior_hyper(5, S);
  cube  posterior_h(N, T, S);
  mat   posterior_rho(N, S);
  mat   posterior_omega(N, S);
  ucube posterior_S(N, T, S);
  mat   posterior_sigma2_omega(N, S);
  mat   posterior_s_(N, S);
  cube  posterior_sigma(N, T, S);
  
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
      urowvec S_tmp     = aux_S.row(n);
      rowvec  U_tmp     = U.row(n);
      double  s2o_tmp   = aux_sigma2_omega(n);
      double  s_n       = aux_s_(n);
      
      List sv_n         = svar_nc1( h_tmp, rho_tmp, omega_tmp, s2o_tmp, s_n, S_tmp, U_tmp, prior, sample_s_ );

      aux_h.row(n)      = as<rowvec>(sv_n["aux_h_n"]);
      aux_rho(n)        = as<double>(sv_n["aux_rho_n"]);
      aux_omega(n)      = as<double>(sv_n["aux_omega_n"]);
      aux_S.row(n)      = as<urowvec>(sv_n["aux_S_n"]);
      aux_sigma2_omega(n)         = as<double>(sv_n["aux_sigma2_omega_n"]);
      aux_s_(n)         = as<double>(sv_n["aux_s_n"]);

      aux_sigma.row(n)  = exp(0.5 * aux_omega(n) * aux_h.row(n));
    }
    
    posterior_B.slice(s)          = aux_B;
    posterior_A.slice(s)          = aux_A;
    posterior_hyper.col(s)        = aux_hyper;
    posterior_h.slice(s)          = aux_h;
    posterior_rho.col(s)          = aux_rho;
    posterior_omega.col(s)        = aux_omega;
    posterior_S.slice(s)          = aux_S;
    posterior_sigma2_omega.col(s) = aux_sigma2_omega;
    posterior_s_.col(s)           = aux_s_;
    posterior_sigma.slice(s)      = aux_sigma;
  } // END s loop
  
  return List::create(
    _["last_draw"]  = List::create(
      _["B"]        = aux_B,
      _["A"]        = aux_A,
      _["hyper"]    = aux_hyper,
      _["h"]        = aux_h,
      _["rho"]      = aux_rho,
      _["omega"]    = aux_omega,
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
      _["S"]        = posterior_S,
      _["sigma2_omega"] = posterior_sigma2_omega,
      _["s_"]        = posterior_s_,
      _["sigma"]    = posterior_sigma
    )
  );
} // END bsvar_sv





//' @title The log of Bayes factor for the homoskedasticity hypothesis for each structural shock
//'
//' @description Computes the logarithm of Bayes factor for the homoskedasticity hypothesis 
//' for each of the structural shocks via Savage-Dickey Density Ration (SDDR).
//' The hypothesis is represented by restriction:
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("w_n = 0")}
//' The logarithm of Bayes factor for this hypothesis can be computed using the SDDR 
//' as the difference of logarithms of the marginal posterior distribution ordinate at the restriction 
//' less the marginal prior distribution ordinate at the same point:
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("log p(w_n = 0 | data) - log p(w_n = 0)")}
//' Therefore, a negative value of the difference is the evidence against 
//' homoskedasticity of the structural shock. The estimation of both elements of the difference requires 
//' numerical integration.
//' 
//' @param posterior the \code{posterior} element of the list from the estimation outcome obtained using function \code{bsvar_sv}
//' @param prior A list specifying the prior distribution. See argument \code{prior} of function \code{bsvar_sv}
//' @param Y an \code{NxT} matrix, the matrix containing \code{T} observations on \code{N} dependent time series variables
//' @param X a \code{KxT} matrix, the matrix containing \code{T} observations on \code{K = N*p+d} regressors including \code{p} lags of dependent variables and \code{d} deterministic terms
//' @param sample_s_ a logical value set to the same value as the corresponding argument of function \code{bsvar_sv}
//' 
//' @return A list of two components:
//' 
//' \code{logSDDR} an \code{N}-vector with values of the logarithm of the Bayes factors for 
//' the homoskedasticity hypothesis for each of the shocks
//' 
//' \code{components} a list of three components for the computation of the Bayes factor
//' \describe{
//'   \item{log_denominator}{an \code{N}-vector with values of the logarithm of the Bayes factor denominators}
//'   \item{log_numerator}{an \code{N}-vector with values of the logarithm of the Bayes factor numerators}
//'   \item{log_numerator_s}{an \code{NxS} matrix of the log-full conditional posterior density ordinates computed to estimate the numerator}
//' }
//' 
//' @seealso [bsvar_sv()]
//'
//' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
//' 
//' @references 
//' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2022) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference.
//'
//' @export
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
  if ( sample_s_ ) {
    vec sample_prior_s_   = prior_s_/as<vec>(Rcpp::rchisq(S, 3));
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
      
      double  V_omega         = as_scalar(posterior_h.slice(s).row(n) * diagmat(sigma_S_inv) * trans(posterior_h.slice(s).row(n))) + pow(posterior_sigma2_omega(n, s), -1);
      double  omega_bar       = V_omega * as_scalar(posterior_h.slice(s).row(n) * diagmat(sigma_S_inv) * trans(residuals.row(n) - alpha_S));
      log_numerator_s(n, s)   = R::dnorm(0, omega_bar, sqrt(V_omega), true);
    } // END n loop
  } // END s loop
  
  // compute the log of the mean numerator exp(log_numerator)
  vec c_log_numerator_s       = max(log_numerator_s, 1);
  vec log_numerator           = c_log_numerator_s - log(S) + log( sum( exp(log_numerator_s.each_col() - c_log_numerator_s), 1) );
  
  return List::create(
    _["logSDDR"]    = log_numerator - log_denominator,
    _["components"]  = List::create(
      _["log_denominator"]    = log_denominator,
      _["log_numerator"]      = log_numerator,
      _["log_numerator_s"]    = log_numerator_s
    )
  );
} // END logSDDR_homoskedasticity

