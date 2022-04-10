

#include <RcppArmadillo.h>
#include "progress.hpp"
#include "Rcpp/Rmath.h"

#include "utils.h"

using namespace Rcpp;
using namespace arma;



/*______________________function sample_A_homosk1______________________*/
void sample_A_homosk1 (
    mat&        aux_A,          // NxK
    const mat&  aux_B,          // NxN
    const vec&  aux_hyper,      // NxM
    const mat&  Y,              // NxT dependent variables
    const mat&  X,              // KxT dependent variables
    const List& prior           // a list of priors - original dimensions
) {
  // the function changes the value of aux_A by reference
  const int N         = aux_A.n_rows;
  const int K         = aux_A.n_cols;

  mat prior_A_mean    = as<mat>(prior["A"]);
  mat prior_A_Vinv    = pow(aux_hyper(1), -1) * as<mat>(prior["A_V_inv"]);
  rowvec    zerosA(K);
  
  for (int n=0; n<N; n++) {
    mat   A0          = aux_A;
    A0.row(n)         = zerosA;
    vec   zn          = vectorise( aux_B * (Y - A0 * X) );
    mat   Wn          = kron( trans(X), aux_B.col(n) );
    
    mat     precision = prior_A_Vinv + trans(Wn) * Wn;
    rowvec  location  = prior_A_mean.row(n) * prior_A_Vinv + trans(zn) * Wn;
    
    mat     precision_chol = trimatu(chol(precision));
    vec     draw      = solve(precision_chol, 
                                solve(trans(precision_chol), trans(location)) + as<vec>(rnorm(K)));
    aux_A.row(n)      = trans(draw);
  } // END n loop
} // END sample_A_homosk1



/*______________________function sample_A_heterosk1 ______________________*/
void sample_A_heterosk1 (
    mat&        aux_A,          // NxK
    const mat&  aux_B,          // NxN
    const vec&  aux_hyper,      // NxM
    const mat&  aux_sigma,      // NxT conditional STANDARD DEVIATIONS
    const mat&  Y,              // NxT dependent variables
    const mat&  X,              // KxT dependent variables
    const List& prior           // a list of priors - original dimensions
) {
  // the function changes the value of aux_A by reference
  const int N         = aux_A.n_rows;
  const int K         = aux_A.n_cols;
  
  mat prior_A_mean    = as<mat>(prior["A"]);
  mat prior_A_Vinv    = pow(aux_hyper(1), -1) * as<mat>(prior["A_V_inv"]);
  rowvec    zerosA(K);
  vec sigma_vectorised= vectorise(aux_sigma);
  
  for (int n=0; n<N; n++) {
    mat   A0          = aux_A;
    A0.row(n)         = zerosA;
    vec   zn          = vectorise( aux_B * (Y - A0 * X) );
    mat   zn_sigma    = zn / sigma_vectorised;
    mat   Wn          = kron( trans(X), aux_B.col(n) );
    mat   Wn_sigma    = Wn.each_col() / sigma_vectorised;
    
    mat     precision = prior_A_Vinv + trans(Wn_sigma) * Wn_sigma;
    precision         = 0.5 * (precision + precision.t());
    rowvec  location  = prior_A_mean.row(n) * prior_A_Vinv + trans(zn_sigma) * Wn_sigma;
    
    mat     precision_chol = trimatu(chol(precision));
    vec     draw      = solve(precision_chol, 
                              solve(trans(precision_chol), trans(location)) + as<vec>(rnorm(K)));
    aux_A.row(n)      = trans(draw);
  } // END n loop
} // END sample_A_heterosk1



/*______________________function sample_B_homosk1______________________*/
void sample_B_homosk1 (
    mat&        aux_B,          // NxN
    const mat&  aux_A,          // NxK
    const vec&  aux_hyper,      // NxM
    const mat&  Y,              // NxT dependent variables
    const mat&  X,              // KxT dependent variables
    const List& prior,          // a list of priors - original dimensions
    const field<mat>& VB        // restrictions on B0
) {
  // the function changes the value of aux_B by reference
  const int N               = aux_B.n_rows;
  const int T               = Y.n_cols;
  
  const int posterior_nu    = T + as<int>(prior["B_nu"]);
  mat prior_SS_inv          = pow(aux_hyper(0), -1) * as<mat>(prior["B_V_inv"]);
  mat shocks                = Y - aux_A * X;
  mat posterior_SS_inv      = prior_SS_inv + shocks * shocks.t();
  
  for (int n=0; n<N; n++) {
    mat posterior_S_inv     = VB(n) * posterior_SS_inv * VB(n).t();
    posterior_S_inv         = 0.5*( posterior_S_inv + posterior_S_inv.t() );
    
    // sample B
    mat Un                  = chol(posterior_nu * inv_sympd(posterior_S_inv));
    mat B_tmp               = aux_B;
    B_tmp.shed_row(n);
    rowvec w                = trans(orthogonal_complement_matrix_TW(B_tmp.t()));
    vec w1_tmp              = trans(w * VB(n).t() * Un.t());
    double w1w1_tmp         = as_scalar(sum(pow(w1_tmp, 2)));
    mat w1                  = w1_tmp.t()/sqrt(w1w1_tmp);
    mat Wn;
    const int rn            = VB(n).n_rows;
    if (rn==1) {
      Wn                    = w1;
    } else {
      Wn                    = join_rows(w1.t(), orthogonal_complement_matrix_TW(w1.t()));
    }
    
    vec   alpha(rn);
    vec   u                 = rnorm(posterior_nu + 1, 0, pow(posterior_nu, -0.5));
    alpha(0)                = sqrt(as_scalar(sum(square(u))));
    if (R::runif(0,1)<0.5) {
      alpha(0)       *= -1;
    }
    if (rn>1){
      vec nn                = rnorm(rn-1, 0, pow(posterior_nu, -0.5));
      alpha.rows(1,rn-1)    = nn;
    }
    rowvec b0n              = alpha.t() * Wn * Un;
    aux_B.row(n)            = b0n * VB(n);
  } // END n loop
} // END sample_B_homosk1



/*______________________function sample_B_heterosk1______________________*/
void sample_B_heterosk1 (
    mat&        aux_B,          // NxN
    const mat&  aux_A,          // NxK
    const vec&  aux_hyper,      // NxM
    const mat&  aux_sigma,      // NxT conditional STANDARD DEVIATIONS
    const mat&  Y,              // NxT dependent variables
    const mat&  X,              // KxT dependent variables
    const List& prior,          // a list of priors - original dimensions
    const field<mat>& VB        // restrictions on B0
) {
  // the function changes the value of aux_B0 and aux_Bplus by reference (filling it with a new draw)
  const int N               = aux_B.n_rows;
  const int T               = Y.n_cols;
  
  const int posterior_nu    = T + as<int>(prior["B_nu"]);
  mat prior_SS_inv          = pow(aux_hyper(0), -1) * as<mat>(prior["B_V_inv"]);
  mat shocks                = Y - aux_A * X;
  
  
  for (int n=0; n<N; n++) {
    
    // set scale matrix
    mat shocks_sigma        = shocks.each_row() / aux_sigma.row(n);
    mat posterior_SS_inv    = prior_SS_inv + shocks_sigma * shocks_sigma.t();
    mat posterior_S_inv     = VB(n) * posterior_SS_inv * VB(n).t();
    posterior_S_inv         = 0.5*( posterior_S_inv + posterior_S_inv.t() );
    
    // sample B
    mat Un                  = chol(posterior_nu * inv_sympd(posterior_S_inv));
    mat B_tmp               = aux_B;
    B_tmp.shed_row(n);
    rowvec w                = trans(orthogonal_complement_matrix_TW(B_tmp.t()));
    vec w1_tmp              = trans(w * VB(n).t() * Un.t());
    double w1w1_tmp         = as_scalar(sum(pow(w1_tmp, 2)));
    mat w1                  = w1_tmp.t()/sqrt(w1w1_tmp);
    mat Wn;
    const int rn            = VB(n).n_rows;
    if (rn==1) {
      Wn                    = w1;
    } else {
      Wn                    = join_rows(w1.t(), orthogonal_complement_matrix_TW(w1.t()));
    }
    
    vec   alpha(rn);
    vec   u                 = rnorm(posterior_nu+1, 0, pow(posterior_nu, -0.5));
    alpha(0)                = pow(as_scalar(sum(pow(u,2))), 0.5);
    if (R::runif(0,1)<0.5) {
      alpha(0)       *= -1;
    }
    if (rn>1){
      vec nn                = Rcpp::rnorm(rn-1, 0, pow(posterior_nu, -0.5));
      alpha.rows(1,rn-1)    = nn;
    }
    rowvec b0n              = alpha.t() * Wn * Un;
    aux_B.row(n)           = b0n * VB(n);
  } // END n loop
} // END sample_B_heterosk1



/*______________________function sample_hyperparameters______________________*/
void sample_hyperparameters (
    vec&              aux_hyper,
    const mat&        aux_B,
    const mat&        aux_A,
    const field<mat>& VB,
    const List&       prior
) {
  // the function changes the value of aux_hyper by reference (filling it with a new draw)
  const int N = aux_B.n_rows;
  const int K = aux_A.n_cols;
  int rn=0;
  for (int n=0; n<N; n++) {
    rn       += VB(n).n_rows;
  }
  aux_hyper(4)    = ( as<double>(prior["hyper_S"]) + aux_hyper(2) + aux_hyper(3) ) / R::rchisq(as<double>(prior["hyper_V"]) + 4*as<double>(prior["hyper_a"]));
  aux_hyper(3)    = R::rgamma( as<double>(prior["hyper_a"]) + 0.5*as<double>(prior["hyper_nu"]) ,
            1/((1/aux_hyper(4)) + (1/(2*aux_hyper(1)))));
  aux_hyper(2)    = R::rgamma( as<double>(prior["hyper_a"]) + 0.5*as<double>(prior["hyper_nu"]) ,
            1/((1/aux_hyper(4)) + (1/(2*aux_hyper(0)))) );
  aux_hyper(1)    = ( aux_hyper(3) + trace((aux_A - as<mat>(prior["A"])) * as<mat>(prior["A_V_inv"]) * trans(aux_A - as<mat>(prior["A"]))) ) /
    R::rchisq( as<double>(prior["hyper_nu"]) + N * K );
  aux_hyper(0)    = ( aux_hyper(2) + trace(aux_B * as<mat>(prior["B_V_inv"]) * trans(aux_B) )) /
    R::rchisq( as<double>(prior["hyper_nu"]) + rn );
} // END sample_hyperparameters


