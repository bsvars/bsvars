
#ifndef _BSVAR_MIX_H_
#define _BSVAR_MIX_H_

#include <RcppArmadillo.h>


Rcpp::List bsvar_mix (
    const int&              S,              // No. of posterior draws
    const arma::mat&        Y,              // NxT dependent variables
    const arma::mat&        X,              // KxT explanatory variables
    const Rcpp::List&       prior,          // a list of priors - original dimensions
    const arma::field<arma::mat>& VB,       // restrictions on B0
    const Rcpp::List&       starting_values
);


#endif  // _BSVAR_MIX_H_