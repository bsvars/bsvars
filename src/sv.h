
#ifndef _SV_H_
#define _SV_H_

#include <RcppArmadillo.h>

double do_rgig1(
    double lambda, 
    double chi, 
    double psi
);

Rcpp::List cholesky_tridiagonal(
    const arma::vec&    omega_diag,
    const double&       omega_offdiag
);


arma::vec forward_algorithm(
    const arma::vec& chol_diag,
    const arma::vec& chol_offdiag,
    const arma::vec& covector
);


arma::vec backward_algorithm(
    const arma::vec& chol_diag,
    const arma::vec& chol_offdiag,
    const arma::vec& htmp
);


arma::vec precision_sampler_ar1(
    const arma::vec&    precision_diag,
    const double&       precision_offdiag,
    const arma::vec&    location
);


arma::uvec inverse_transform_sampling (
    const arma::vec&  mixprob,
    const int         T
);


arma::vec find_mixture_indicator_cdf (
    const arma::vec& datanorm           // provide all that is conditionally normal
);


Rcpp::List svar_nc1 (
    arma::rowvec&   aux_h_n,              // 1xT
    double&         aux_rho_n,
    double&         aux_omega_n,
    double&         aux_sigma2v_n,
    double&         aux_sigma2_omega_n,   // omega prior hyper-parameter 
    double&         aux_s_n,              // scale of IG2 prior for aux_sigma2_omega_n
    arma::urowvec&  aux_S_n,              // 1xT
    const arma::rowvec&   u,              // 1xT
    const Rcpp::List&     prior,
    bool            sample_s_ = true
);

Rcpp::List svar_ce1 (
    arma::rowvec&       aux_h_n,            // 1xT
    double&             aux_rho_n,
    double&             aux_omega_n,
    double&             aux_sigma2v_n,
    double&             aux_sigma2_omega_n, // omega prior hyper-parameter 
    double&             aux_s_n,             // scale of IG2 prior for aux_sigma2_omega_n
    arma::urowvec&      aux_S_n,            // 1xT
    const arma::rowvec& u,                  // 1xT
    const Rcpp::List&   prior,
    bool                sample_s_ = true
);

#endif  // _SV_H_