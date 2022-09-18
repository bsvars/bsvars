
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


#endif  // _BSVARTOOLS_H_