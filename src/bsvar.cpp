

#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"
#include "normalisation.h"

using namespace Rcpp;
using namespace arma;

/*______________________function sample_B_homosk1______________________*/
void sample_B_homosk1 (
    mat&        aux_B,          // NxN
    const mat&  aux_A,          // NxK
    const vec&  aux_hyper,      // NxM
    const mat&  Y,              // NxT dependent variables
    const mat&  X,              // KxT dependent variables
    const List& prior,          // a list of priors - original dimensions
    const field<mat>& VB        // restrictions on B0
) {
  // the function changes the value of aux_B by reference
  const int N               = aux_B.n_rows;
  const int T               = Y.n_cols;
  
  const int posterior_nu    = T + as<int>(prior["B_nu"]);
  mat prior_SS_inv          = pow(aux_hyper(0), -1) * as<mat>(prior["B_V_inv"]);
  mat shocks                = Y - aux_A * X;
  mat posterior_SS_inv      = prior_SS_inv + shocks * shocks.t();
  
  for (int n=0; n<N; n++) {
    mat posterior_S_inv     = VB(n) * posterior_SS_inv * VB(n).t();
    posterior_S_inv         = 0.5*( posterior_S_inv + posterior_S_inv.t() );
    
    // sample B
    mat Un                  = chol(posterior_nu * inv_sympd(posterior_S_inv));
    mat B_tmp               = aux_B;
    B_tmp.shed_row(n);
    rowvec w                = trans(orthogonal_complement_matrix_TW(B_tmp.t()));
    vec w1_tmp              = trans(w * VB(n).t() * Un.t());
    double w1w1_tmp         = as_scalar(sum(pow(w1_tmp, 2)));
    mat w1                  = w1_tmp.t()/sqrt(w1w1_tmp);
    mat Wn;
    const int rn            = VB(n).n_rows;
    if (rn==1) {
      Wn                    = w1;
    } else {
      Wn                    = join_rows(w1.t(), orthogonal_complement_matrix_TW(w1.t()));
    }
    
    vec   alpha(rn);
    vec   u                 = rnorm(posterior_nu + 1, 0, pow(posterior_nu, -0.5));
    alpha(0)                = sqrt(as_scalar(sum(square(u))));
    if (R::runif(0,1)<0.5) {
      alpha(0)       *= -1;
    }
    if (rn>1){
      vec nn                = rnorm(rn-1, 0, pow(posterior_nu, -0.5));
      alpha.rows(1,rn-1)    = nn;
    }
    rowvec b0n              = alpha.t() * Wn * Un;
    aux_B.row(n)            = b0n * VB(n);
  } // END n loop
} // END sample_B_homosk1



/*______________________function sample_A_homosk1______________________*/
void sample_A_homosk1 (
    mat&        aux_A,          // NxK
    const mat&  aux_B,          // NxN
    const vec&  aux_hyper,      // NxM
    const mat&  Y,              // NxT dependent variables
    const mat&  X,              // KxT dependent variables
    const List& prior           // a list of priors - original dimensions
) {
  // the function changes the value of aux_A by reference
  const int N         = aux_A.n_rows;
  const int K         = aux_A.n_cols;

  mat prior_A_mean    = as<mat>(prior["A"]);
  mat prior_A_Vinv    = pow(aux_hyper(1), -1) * as<mat>(prior["A_V_inv"]);
  rowvec    zerosA(K);
  
  for (int n=0; n<N; n++) {
    mat   A0          = aux_A;
    A0.row(n)         = zerosA;
    vec   zn          = vectorise( aux_B * (Y - A0 * X) );
    mat   Wn          = kron( trans(X), aux_B.col(n) );
    
    mat     precision = prior_A_Vinv + trans(Wn) * Wn;
    rowvec  location  = prior_A_mean.row(n) * prior_A_Vinv + trans(zn) * Wn;
    
    mat     precision_chol = trimatu(chol(precision));
    vec     draw      = solve(precision_chol, 
                                solve(trans(precision_chol), trans(location)) + as<vec>(rnorm(K)));
    aux_A.row(n)      = trans(draw);
  } // END n loop
} // END sample_A_homosk1



/*______________________function sample_hyperparameters______________________*/
void sample_hyperparameters (
    vec&              aux_hyper,
    const mat&        aux_B,
    const mat&        aux_A,
    const field<mat>& VB,
    const List&       prior
) {
  // the function changes the value of aux_hyper by reference (filling it with a new draw)
  const int N = aux_B.n_rows;
  const int K = aux_A.n_cols;
  int rn=0;
  for (int n=0; n<N; n++) {
    rn       += VB(n).n_rows;
  }
  aux_hyper(4)    = ( as<double>(prior["hyper_S"]) + aux_hyper(2) + aux_hyper(3) ) / R::rchisq(as<double>(prior["hyper_V"]) + 4*as<double>(prior["hyper_a"]));
  aux_hyper(3)    = R::rgamma( as<double>(prior["hyper_a"]) + 0.5*as<double>(prior["hyper_nu"]) ,
            1/((1/aux_hyper(4)) + (1/(2*aux_hyper(1)))));
  aux_hyper(2)    = R::rgamma( as<double>(prior["hyper_a"]) + 0.5*as<double>(prior["hyper_nu"]) ,
            1/((1/aux_hyper(4)) + (1/(2*aux_hyper(0)))) );
  aux_hyper(1)    = ( aux_hyper(3) + trace((aux_A - as<mat>(prior["A"])) * as<mat>(prior["A_V_inv"]) * trans(aux_A - as<mat>(prior["A"]))) ) /
    R::rchisq( as<double>(prior["hyper_nu"]) + N*K );
  aux_hyper(0)    = ( aux_hyper(2) + trace(aux_B * as<mat>(prior["B_V_inv"]) * trans(aux_B) )) /
    R::rchisq( as<double>(prior["hyper_nu"]) + rn );
} // END sample_hyperparameters


//' @title Bayesian estimation of a homoskedastic Structural Vector Autoregression via Gibbs sampler
//'
//' @description Estimates the the homoskedastic SVAR using the Gibbs sampler of Waggoner & Zha (2003)
//' for the structural matrix \code{B} and the equation-by-equation sampler by Chan, Koop, & Yu (2021)
//' for the autoregressive slope parameters \code{A}. Additionally, the parameter matrices \code{A} and \code{B}
//' follow the multivariateMinnesota prior and generalised-normal prior distributions with the matrix-specific
//' overall shrinkage parameters estimated thanks to a 3-level hierarchical prior distribution.
//' 
//' @details 
//' The homoskedastic SVAR model is given by the reduced form equation:
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("Y = AX + E")}
//' where \code{Y} is an \code{NxT} matrix of dependent variables, \code{X} is a \code{KxT} matrix of explanatory variables, 
//' \code{E} is am \code{NxT} matrix of reduced form error terms, and \code{A} is a \code{NxK} matrix of autoregressive slope coefficients and parameters on deterministic terms in \code{X}.
//' 
//' The structural equation is given by
//' \Sexpr[results=rd, stage=build]{katex::math_to_rd("BE=U")}
//' where \code{U} is am \code{NxT} matrix of structural form error terms, and
//' \code{B} is an \code{NxN} matrix of contemporaneous relationships between structural shocks in columns of matrix \code{U}.
//' 
//' Finally, the structural shocks are temporally and contemporaneously independent and jointly normally distributed with zero mean and unit variances.
//' 
//' @param S a positive integer, the number of posterior draws to be generated
//' @param Y an \code{NxT} matrix, the matrix containing \code{T} observations on \code{N} dependent time series variables
//' @param X a \code{KxT} matrix, the matrix containing \code{T} observations on \code{K = N*p+d} regressors including \code{p} lags of dependent variables and \code{d} deterministic terms
//' @param VB a list with \code{N} matrices determining the unrestricted emelents of matrix \code{B}
//' @param prior a list containing the following elements
//' \describe{
//'  \item{A}{an \code{NxK} matrix, the mean of the normal prior distribution for the parameter matrix \code{A}}
//'  \item{A_V_inv}{a \code{KxK} precision matrix of the normal prior distribution for each of the row of the parameter matrix \code{A}. This precision matrix is the same for each equation.}
//'  \item{B_V_inv}{a \code{NxN} precision matrix of the generalised-normal prior distribution for the structural matrix \code{B}. This precision matrix is the same for each equation.}
//'  \item{B_nu}{a positive integer greater of equal to \code{N}, a shape parameter of the generalised-normal prior distribution for the structural matrix \code{B}}
//'  \item{hyper_nu}{a positive scalar, the shape parameter of the inverted-gamma 2 prior distribution for the two overall shrinkage parameters for matrices \code{B} and \code{A}}
//'  \item{hyper_a}{a positive scalar, the shape parameter of the gamma prior for the two overall shrinkage parameters}
//'  \item{hyper_V}{a positive scalar,  the shape parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
//'  \item{hyper_S}{a positive scalar,  the scale parameter of the inverted-gamma 2 for the level 3 hierarchy of shrinkage parameters}
//' }
//' @param starting_values a list containing the following elements
//' \describe{
//'  \item{A}{an \code{NxK} matrix of the starting values for the parameter \code{A}}
//'  \item{B}{an \code{NxN} matrix of the starting values for the parameter \code{B}}
//'  \item{hyper}{a \code{5}-vector of the starting values for the shrinkage hyper-parameters of the hierarchical prior distribution}
//' }
//' 
//' @return A list containing two elements:
//' 
//'  \code{posterior} a list with a collection of \code{S} draws from the posterior distribution generated via Gibbs sampler containing:
//'  \describe{
//'  \item{A}{an \code{NxKxS} array with the posterior draws for matrix \code{A}}
//'  \item{B}{an \code{NxNxS} array with the posterior draws for matrix \code{B}}
//'  \item{hyper}{a \code{5xS} matrix with the posterior draws for the hyper-parameters of the hierarchical prior distribution}
//' }
//' 
//' \code{last_draw} a list with the last draw of the simulation (to be provided as \code{starting_values} to the follow-up run of \code{bsvar}) containing the following objects:
//' \describe{
//'  \item{A}{an \code{NxK} matrix with the last MCMC draw of the parameter matrix \code{A}}
//'  \item{B}{an \code{NxN} matrix with the last MCMC draw of the parameter matrix \code{B}}
//'  \item{hyper}{a \code{5}-vector with the last MCMC draw of the hyper-parameter of the hierarchical prior distribution}
//'  }
//'
//' @author Tomasz Woźniak <wozniak.tom@pm.me>
//' 
//' @references Sampling from the generalised-normal full conditional posterior distribution of matrix \code{B} is implemented using the Gibbs sampler by:
//' 
//' Waggoner, D.F., and Zha, T., (2003) A Gibbs sampler for structural vector autoregressions. \emph{Journal of Economic Dynamics and Control} \bold{28}, 349--366, \doi{https://doi.org/10.1016/S0165-1889(02)00168-9}.
//'
//' Sampling from the multivariate normal full conditional posterior distribution of each of the \code{A} matrix row is implemented using the sampler by:
//' 
//' Chan, J.C.C., Koop, G, and Yu, X. (2021) Large Order-Invariant Bayesian VARs with Stochastic Volatility.
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List bsvar(
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
} // END bsvar
