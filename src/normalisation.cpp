
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


// [[Rcpp::interfaces(cpp)]]
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

