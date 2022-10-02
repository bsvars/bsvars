
#ifndef _BSVARTOOLS_H_
#define _BSVARTOOLS_H_

#include <RcppArmadillo.h>

arma::field<arma::cube> bsvars_ir (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    const int     horizon,
    const int     p
);


arma::field<arma::cube> bsvars_fevd (
    arma::field<arma::cube>   posterior_irf   // output of bsvars_irf
);


arma::cube bsvars_structural_shocks (
    const arma::cube&     posterior_B,    // (N, N, S)
    const arma::cube&     posterior_A,    // (N, K, S)
    const arma::mat&      Y,              // NxT dependent variables
    const arma::mat&      X               // KxT dependent variables
);


arma::field<arma::cube> bsvars_hd (
    arma::field<arma::cube>&    posterior_irf_T,    // output of bsvars_irf with irfs at T horizons
    arma::cube&                 structural_shocks   // NxTxS output bsvars_structural_shocks
);


arma::cube bsvars_fitted_values (
    arma::cube&     posterior_A,        // NxKxS
    arma::mat&      X                   // KxT
);

#endif  // _BSVARTOOLS_H_