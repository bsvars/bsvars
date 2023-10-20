
#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"

using namespace Rcpp;
using namespace arma;


/*______________________function orthogonal_complement_matrix_TW______________________*/
// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::mat orthogonal_complement_matrix_TW (const arma::mat& x) {
  // # x is a mxn matrix and m>n
  // # the function returns a mx(m-n) matrix, out, that is an orthogonal complement of x, i.e.:
  // # t(x)%*%out = 0 and det(cbind(x,out))!=0
  int n_nrow     = x.n_rows;
  int n_ncol     = x.n_cols;
  mat Q;
  mat R;
  qr(Q, R, x);
  mat ocm = Q.tail_cols(n_nrow-n_ncol);
  return ocm;
} // END orthogonal_complement_matrix_TW



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::vec log_mean (
    arma::mat     log_density     // n x s matrix with log density ordinates
) {
  int S               = log_density.n_cols;
  vec c_log_density   = max(log_density, 1);
  vec log_numerator   = c_log_density - log(S) + log( sum( exp(log_density.each_col() - c_log_density), 1) );
  return log_numerator;
} // log_mean 



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
std::string ordinal(
    int n
) {
  std::string suffix;
  if (n % 10 == 1 && n % 100 != 11) {
    suffix = "st";
  } else if (n % 10 == 2 && n % 100 != 12) {
    suffix = "nd";
  } else if (n % 10 == 3 && n % 100 != 13) {
    suffix = "rd";
  } else {
    suffix = "th";
  }
  return std::to_string(n) + suffix;
} // END ordinal

