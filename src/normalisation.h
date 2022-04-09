
#ifndef _NORMALISATION_H_
#define _NORMALISATION_H_

#include <RcppArmadillo.h>

arma::rowvec normalisation_wz2003_s (
    const arma::mat& B,                   // NxN
    const arma::mat& B_hat_inv,           // NxN
    const arma::mat& Sigma_inv,           // NxN
    const arma::mat& diag_signs           // KxN
);


void normalisation_wz2003 (
    arma::cube& posterior_B,            // NxNxS
    const arma::mat& B_hat              // NxN
);

#endif  // _NORMALISATION_H_