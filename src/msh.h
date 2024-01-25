
#ifndef _MSH_H_
#define _MSH_H_

#include <RcppArmadillo.h>



arma::vec Ergodic_PR_TR (
    const arma::mat&  PR_TR           // MxM
);


arma::mat count_regime_transitions (
    const arma::mat& xi
);


arma::rowvec rDirichlet1 (
    const arma::rowvec&   alpha       // Kx1
);


arma::rowvec rIG2_Dirichlet1 (
    const arma::rowvec&  s,           // 1xM
    const arma::rowvec&  nu           // 1xM
);


arma::mat filtering_msh (
    const arma::mat&  U,              // NxT
    const arma::mat&  sigma,          // NxM
    const arma::mat&  PR_TR,          // MxM
    const arma::vec&  pi_0            // Mx1
);


arma::mat smoothing_msh (
    const arma::mat&  U,              // NxT
    const arma::mat&  PR_TR,          // MxM
    const arma::mat&  filtered        // MxT
);


arma::mat sample_Markov_process_msh (
    arma::mat&        aux_xi,             // MxT
    const arma::mat&  U,                  // NxT
    const arma::mat&  aux_sigma2,         // NxM
    const arma::mat&  aux_PR_TR,         // MxM
    const arma::vec&  aux_pi_0,          // Mx1
    const bool        finiteM = true
);


Rcpp::List sample_transition_probabilities (
    arma::mat           aux_PR_TR,    // MxM 
    arma::vec           aux_pi_0,     // Mx1
    const arma::mat&    aux_xi,       // MxT
    const Rcpp::List&   prior,         // a list of priors - original dimensions
    const bool          MSnotMIX = true
);


arma::mat sample_variances_msh (
    arma::mat&          aux_sigma2, // NxM
    const arma::mat&    aux_B,      // NxN
    const arma::mat&    aux_A,      // NxK
    const arma::mat&    Y,          // NxT dependent variables
    const arma::mat&    X,          // KxT explanatory variables
    const arma::mat&    aux_xi,     // MxT state variables
    const Rcpp::List&   prior       // a list of priors - original dimensions
);


#endif  // _MSH_H_