
#ifndef _BSVAR_SV_H_
#define _BSVAR_SV_H_

#include <RcppArmadillo.h>

Rcpp::List bsvar_sv_cpp (
    const int&                    S,          // No. of posterior draws
    const arma::mat&              Y,          // NxT dependent variables
    const arma::mat&              X,          // KxT explanatory variables
    const Rcpp::List&             prior,      // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,         // restrictions on B0
    const Rcpp::List&             starting_values, 
    const int                     thin = 100, // introduce thinning
    const bool                    centred_sv = false,
    const bool                    show_progress = true
);


// Rcpp::List logSDDR_homoskedasticity (
//     const Rcpp::List&       posterior,  // a list of posteriors
//     const Rcpp::List&       prior,      // a list of priors - original dimensions
//     const arma::mat&        Y,          // NxT dependent variables
//     const arma::mat&        X,          // KxT explanatory variables
//     const bool              sample_s_ = true
// );

#endif  // _BSVAR_SV_H_