
#ifndef _VERIFY_H_
#define _VERIFY_H_

#include <RcppArmadillo.h>


Rcpp::List verify_volatility_sv_cpp (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X,          // KxT explanatory variables
    const bool              sample_s_ = true
);


double dig2dirichlet (
    const arma::rowvec&  x,                // M-vector of positive rv summing up to 1
    const arma::rowvec&  a,                // M-vector of positive parameters
    const arma::rowvec&  b,                // M-vector of positive parameters
    const bool  logarithm = true  
);


double ddirichlet (
    const arma::rowvec&  x,                // M-vector of positive rv summing up to 1
    const arma::rowvec&  a,                // M-vector of positive parameters
    const bool  logarithm = true  
);


Rcpp::List verify_volatility_msh_cpp (
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X           // KxT explanatory variables
);


double dmvnorm_precision (
    const arma::rowvec&   x,  
    const arma::rowvec&   mean,  
    const arma::mat&      precision, 
    const bool            logarithm = true
);


Rcpp::List verify_autoregressive_heterosk_cpp (
    const arma::mat&        hypothesis, // an NxK matrix of values under the null; value 999 stands for not verivied
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X           // KxT explanatory variables
);


Rcpp::List verify_autoregressive_homosk_cpp (
    const arma::mat&        hypothesis, // an NxK matrix of values under the null; value 999 stands for not verivied
    const Rcpp::List&       posterior,  // a list of posteriors
    const Rcpp::List&       prior,      // a list of priors - original dimensions
    const arma::mat&        Y,          // NxT dependent variables
    const arma::mat&        X           // KxT explanatory variables
);

#endif  // _VERIFY_H_