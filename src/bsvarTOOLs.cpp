#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::field<arma::cube> bsvars_ir (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    const int     horizon,
    const int     p
) {
  
  const int       N = posterior_B.n_rows;
  const int       S = posterior_B.n_slices;
  
  field<cube>     irfs(S);
  cube            aux_irfs(N, N, horizon + 1);  // + 0 horizons
  
  mat             A_bold_tmp(N * (p - 1), N * p, fill::eye);
  
  for (int s=0; s<S; s++) {
    mat   irf_0         = inv(posterior_B.slice((s)));
    irf_0               = irf_0 * diagmat(pow(diagvec(irf_0), -1));
    mat   A_bold        = join_cols(posterior_A.slice(s).cols(0, N * p - 1), A_bold_tmp);
    mat   A_bold_power  = A_bold;
    
    aux_irfs.slice(0)   = irf_0;
    
    for (int h=1; h<horizon + 1; h++) {
      aux_irfs.slice(h) = A_bold_power.submat(0, 0, N-1, N-1) * irf_0;
      A_bold_power      = A_bold_power * A_bold;
    } // END h loop
    
    irfs(s)             = aux_irfs;
  } // END s loop
  
  return irfs;
} // END bsvars_ir



// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
arma::field<arma::cube> bsvars_fevd (
    arma::field<arma::cube>   posterior_irf   // output of bsvars_irf
) {
  
  const int       N = posterior_irf(0).n_rows;
  const int       S = posterior_irf.size();
  const int       horizon = posterior_irf(0).n_slices;
  
  field<cube>     fevds(S);
  cube            aux_fevds(N, N, horizon);  // + 0 and inf horizons
  
  for (int s=0; s<S; s++) {
    for (int h=0; h<horizon; h++) {
      for (int n=0; n<N; n++) {
        for (int nn=0; nn<N; nn++) {
          aux_fevds.subcube(n, nn, h, n, nn, h) = accu(square(posterior_irf(s).subcube(n, nn, 0, n, nn, h)));
        }
      }
      aux_fevds.slice(h)  = diagmat(1/sum(aux_fevds.slice(h), 1)) * aux_fevds.slice(h);
    }
    aux_fevds            *= 100;
    fevds(s)              = aux_fevds;
  } // END s loop
  
  return fevds;
} // END bsvars_fevd
