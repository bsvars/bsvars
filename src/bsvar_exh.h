
#ifndef _BSVAR_EXH_H_
#define _BSVAR_EXH_H_

#include <RcppArmadillo.h>


Rcpp::List bsvar_exh_cpp (
    const int&              S,              // No. of posterior draws
    const arma::mat&        Y,              // NxT dependent variables
    const arma::mat&        X,              // KxT explanatory variables
    const Rcpp::List&       prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,       // restrictions on B0
    const arma::field<arma::mat>& VA,       // N-list
    const Rcpp::List&       starting_values,
    const bool              normal = true,
    const int               thin = 100,     // introduce thinning
    const bool              show_progress = true
);


#endif  // _BSVAR_EXH_H_