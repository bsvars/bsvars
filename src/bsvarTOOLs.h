
#ifndef _BSVARTOOLS_H_
#define _BSVARTOOLS_H_

#include <RcppArmadillo.h>


arma::cube bsvars_ir1 (
    arma::mat&    aux_B,              // (N, N)
    arma::mat&    aux_A,              // (N, K)
    const int     horizon,
    const int     p,
    const bool    standardise = false
);


arma::field<arma::cube> bsvars_ir (
    arma::cube&   posterior_B,        // (N, N, S)
    arma::cube&   posterior_A,        // (N, K, S)
    const int     horizon,
    const int     p,
    const bool    standardise = false
);


arma::field<arma::cube> bsvars_fevd_homosk (
    arma::field<arma::cube>&    posterior_irf   // output of bsvars_irf
);


arma::field<arma::cube> bsvars_fevd_heterosk (
    arma::field<arma::cube>&    posterior_irf,    // output of bsvars_irf
    arma::cube&                 forecast_sigma2,  // (N, H, S) output from forecast_sigma2 or forecast_sigma2_msh
    arma::mat&                  sigma2_T          // (N, S) the last in the sample
);


arma::cube bsvars_structural_shocks (
    arma::cube&     posterior_B,    // (N, N, S)
    arma::cube&     posterior_A,    // (N, K, S)
    arma::mat&      Y,              // NxT dependent variables
    arma::mat&      X               // KxT dependent variables
);


arma::field<arma::cube> bsvars_hd (
    arma::field<arma::cube>&    posterior_irf_T,    // output of bsvars_irf with irfs at T horizons
    arma::cube&                 structural_shocks   // NxTxS output bsvars_structural_shocks
);


arma::cube bsvars_fitted_values (
    arma::cube&     posterior_A,        // NxKxS
    arma::cube&     posterior_B,        // NxNxS
    arma::cube&     posterior_sigma,    // NxTxS
    arma::mat&      X                   // KxT
);


arma::cube bsvars_filter_forecast_smooth (
    Rcpp::List&       posterior,
    const arma::mat&  Y,
    const arma::mat&  X,
    const bool        forecasted,
    const bool        smoothed
);


arma::field<arma::cube> bsvars_filter_forecast_smooth_hmsh (
    Rcpp::List&       posterior,
    const arma::mat&  Y,
    const arma::mat&  X,
    const bool        forecasted,
    const bool        smoothed
);


#endif  // _BSVARTOOLS_H_