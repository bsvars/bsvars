
#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"

using namespace Rcpp;
using namespace arma;


/*______________________function normalisation_wz2003_s______________________*/
arma::rowvec normalisation_wz2003_s (
    const arma::mat& B,                   // NxN
    const arma::mat& B_hat_inv,           // NxN
    const arma::mat& Sigma_inv,           // NxN
    const arma::mat& diag_signs           // KxN
) {
  // returns a rowvec of signs
  const int N         = B.n_rows;
  const int K         = pow(2,N);
  vec       distance(K);
  
  for (int k=0; k<K; k++) {
    mat   B_tmp_inv   = inv(diagmat(diag_signs.row(k)) * B);
    mat   dist_tmp    = trans(B_tmp_inv - B_hat_inv);
    for (int n=0; n<N; n++) {
      distance(k)     += as_scalar(dist_tmp.row(n) * Sigma_inv * trans(dist_tmp.row(n)));
    } // END n loop
  } // END k loop
  
  rowvec out = diag_signs.row(distance.index_min());
  return out;
} // END normalisation_wz2003_s



//' @title Waggoner & Zha (2003) row signs normalisation of the posterior draws for matrix \code{B}
//'
//' @description Normalises the sign of rows of matrix \code{B} MCMC draws, 
//'  provided as the first argument \code{posterior_B}, relative to matrix
//'  \code{B_hat}, provided as the second argument of the function. The implemented
//'  procedure proposed by Waggoner, Zha (2003) normalises the MCMC output in an
//'  optimal way leading to the unimodal posterior. Only normalised MCMC output is 
//'  suitable for the computations of the posterior characteristics of the \code{B}
//'  matrix elements and their functions such as the impulse response functions and other 
//'  economically interpretable values. 
//' 
//' @param posterior_B an \code{NxNxS} array containing \code{S} draws from the posterior 
//' distribution of the \code{NxN} matrix of contemporaneous relationships \code{B}. 
//' These draws are to be normalised with respect to:
//' @param B_hat an \code{NxN} matrix specified by the user to have the desired row signs
//' 
//' @return Nothing. The normalised elements overwrite the corresponding elements of 
//' the first argument \code{posterior_B} by reference.
//' 
//' @seealso [bsvar()] [bsvar_msh()] [bsvar_sv()]
//'
//' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
//' 
//' @references 
//' Waggoner, D.F., and Zha, T., (2003) Likelihood Preserving Normalization in Multiple Equation Models. 
//' \emph{Journal of Econometrics}, \bold{114}(2), 329--47, \doi{https://doi.org/10.1016/S0304-4076(03)00087-3}.
//'
//' @export
// [[Rcpp::export]]
void normalisation_wz2003 (
    arma::cube& posterior_B,            // NxNxS
    const arma::mat& B_hat              // NxN
) {
  // changes posterior_B by reference filling it with normalised values
  const int   N       = posterior_B.n_rows;
  const int   K       = pow(2, N);
  const int   S       = posterior_B.n_slices;
  
  mat B_hat_inv       = inv(B_hat);
  mat Sigma_inv       = B_hat.t() * B_hat;
  
  // create matrix diag_signs whose rows contain all sign combinations
  mat diag_signs(K, N);
  vec omo             = as<vec>(NumericVector::create(-1,1));
  for (int n=0; n<N; n++) {
    vec os(pow(2, n), fill::ones);
    vec oos(pow(2, N-1-n), fill::ones);
    diag_signs.col(n) = kron(os, kron(omo, oos));
  }
  
  // normalisation
  for (int s=0; s<S; s++) {
    rowvec sss            = normalisation_wz2003_s(posterior_B.slice(s), B_hat_inv, Sigma_inv, diag_signs);
    mat B_norm            = diagmat(sss) * posterior_B.slice(s);
    posterior_B.slice(s)   = B_norm;
  }
} // END normalisation_wz2003

