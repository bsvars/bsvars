
#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"
#include <RcppTN.h>

using namespace Rcpp;
using namespace arma;



/*______________________function do_rgig1______________________*/
// utility function copied from package factorstochvol
double do_rgig1(
    double lambda, 
    double chi, 
    double psi
) { 
  SEXP (*fun)(int, double, double, double) = NULL;
  if (!fun) fun = (SEXP(*)(int, double, double, double)) R_GetCCallable("GIGrvg", "do_rgig");
  return as<double>(fun(1, lambda, chi, psi));
} // END do_rgig1



/*______________________function cholesky_tridiagonal______________________*/
// utility function from file precision_sampler.cpp
List cholesky_tridiagonal(
    const vec&    omega_diag,
    const double& omega_offdiag
) {
  const int T = omega_diag.n_elem - 1;
  vec chol_diag(T+1);
  vec chol_offdiag(T+1);
  chol_diag[0] = sqrt(omega_diag[0]);
  for (int j = 1; j < T+1; j++) {
    chol_offdiag[j-1] = omega_offdiag/chol_diag[j-1];
    chol_diag[j] = std::sqrt(omega_diag[j]-chol_offdiag[j-1]*chol_offdiag[j-1]);
  }
  return List::create(_["chol_diag"]=chol_diag, _["chol_offdiag"]=chol_offdiag);
} // END cholesky_tridiagonal



/*______________________function forward_algorithm______________________*/
// utility function from file precision_sampler.cpp
vec forward_algorithm(
    const vec& chol_diag,
    const vec& chol_offdiag,
    const vec& covector
) {
  const int T = chol_diag.n_elem - 1;
  vec htmp(T+1);
  htmp[0] = covector[0]/chol_diag[0];
  for (int j = 1; j < T+1; j++) {
    htmp[j] = (covector[j] - chol_offdiag[j-1]*htmp[j-1])/chol_diag[j];
  }
  return htmp;
} // END forward_algorithm



/*______________________function backward_algorithm______________________*/
// utility function from file precision_sampler.cpp
vec backward_algorithm(
    const vec& chol_diag,
    const vec& chol_offdiag,
    const vec& htmp
) {
  const int T = chol_diag.size() - 1;
  vec h(T+1);
  h[T] = htmp[T] / chol_diag[T];
  for (int j = T-1; j >= 0; j--) {
    h[j] = (htmp[j] - chol_offdiag[j] * h[j+1]) / chol_diag[j];
  }
  return h;
} // END backward_algorithm



/*______________________function precision_sampler_ar1______________________*/
// utility function from file precision_sampler.cpp
vec precision_sampler_ar1(
    const vec&     precision_diag,
    const double&  precision_offdiag,
    const vec&     location
) {
  int T               = location.n_rows;
  vec  epsilon        = rnorm(T);                             // sample normal draws using Rcpp::rnorm for compatibility with R's set.seed()
  List precision_chol = cholesky_tridiagonal(precision_diag, precision_offdiag);    // Cholesky decomposition using a dedicated technique
  vec  aa             = forward_algorithm(precision_chol["chol_diag"],              // this forward substitution can be performed outside of the loop
                                          precision_chol["chol_offdiag"],
                                                        location);
  vec draw_ssar1      = backward_algorithm(precision_chol["chol_diag"],
                                           precision_chol["chol_offdiag"],
                                                         aa + epsilon);     // this has to be done in the loop as function backward_algorithm requires covector to be a vector (not a matrix)
  return draw_ssar1;
} // END precision_sampler_ar1



/*______________________function inverse_transform_sampling______________________*/
// utility function from file utils_latent_states.cc from the source code of package stochvol
uvec inverse_transform_sampling (
    const vec&  mixprob,
    const int   T
) {
  uvec r(T);
  for (int j = 0; j < T; j++) {
    int index = (10-1)/2;  // start searching in the middle
    const double unnorm_cdf_value = R::unif_rand()*mixprob[9 + 10*j];  // current (non-normalized) value
    bool larger = false;  // indicates that we already went up
    bool smaller = false; // indicates that we already went down
    while(true) {
      if (unnorm_cdf_value > mixprob[index +  10*j]) {
        index++;
        if (smaller) {
          break;
        } else {
          larger = true;
        }
      } else if (larger || index == 0) {
        break;
      } else {
        index--;
        smaller = true;
      }
    }
    r[j] = index;
  }
  return r;
}



/*______________________function find_mixture_indicator_cdf______________________*/
// utility function from file utils_latent_states.cc from the source code of package stochvol
vec find_mixture_indicator_cdf (
  const vec& datanorm           // provide all that is conditionally normal
){
  // fixed values for auxiliary mixture
  const NumericVector alpha_s = NumericVector::create(1.92677,1.34744,0.73504,0.02266,0-0.85173,-1.97278,-3.46788,-5.55246,-8.68384,-14.65000);
  const NumericVector sigma_s = NumericVector::create(0.11265,0.17788,0.26768,0.40611,0.62699,0.98583,1.57469,2.54498,4.16591,7.33342);
  const NumericVector pr_s    = NumericVector::create(0.00609,0.04775,0.13057,0.20674,0.22715,0.18842,0.12047,0.05591,0.01575,0.00115);
  
  const int T = datanorm.n_elem;
  vec mixprob(10 * T);
  for (int j = 0; j < T; j++) {  // TODO slow (10*T calls to exp)!
    const int first_index = 10*j;
    mixprob(first_index) = std::exp(pr_s(0) - (datanorm(j) - alpha_s(0)) * (datanorm(j) - alpha_s(0)) / sigma_s(0) );
    for (int r = 1; r < 10; r++) {
      mixprob(first_index+r) = mixprob(first_index+r-1) + std::exp(pr_s(r) - (datanorm(j) - alpha_s(r)) * (datanorm(j) - alpha_s(r)) / sigma_s(r) );
    }
  }
  return mixprob;
}



/*______________________function svar_nc1______________________*/
List svar_nc1 (
  rowvec&         aux_h_n,            // 1xT
  double&         aux_rho_n,
  double&         aux_omega_n,
  double&         aux_sigma2_omega_n, // omega prior hyper-parameter 
  double&         aux_s_n,             // scale of IG2 prior for aux_sigma2_omega_n
  urowvec&        aux_S_n,            // 1xT
  const rowvec&   u,                  // 1xT
  const List&     prior,
  bool            sample_s_ = true
) {
  // fixed values for auxiliary mixture
  const NumericVector alpha_s = NumericVector::create(1.92677,1.34744,0.73504,0.02266,0-0.85173,-1.97278,-3.46788,-5.55246,-8.68384,-14.65000);
  const NumericVector sigma_s = NumericVector::create(0.11265,0.17788,0.26768,0.40611,0.62699,0.98583,1.57469,2.54498,4.16591,7.33342);
  const NumericVector pr_s    = NumericVector::create(0.00609,0.04775,0.13057,0.20674,0.22715,0.18842,0.12047,0.05591,0.01575,0.00115);
  const double        ccc     = 0.000000001;      // a constant to make log((u+ccc)^2) feasible
  
  // sample h and omega of the non-centered SV including ASIS step
  const int     T = u.n_cols;
  const rowvec  U = log(pow(u + ccc, 2));
  
  const double  prior_sv_a_ = prior["sv_a_"];
  const double  prior_sv_s_ = prior["sv_s_"];
  
  mat           H_rho(T, T, fill::eye);
  H_rho.diag(-1)       -= aux_rho_n;
  mat           HH_rho  = H_rho.t() * H_rho;
  
  // sample auxiliary mixture states aux_S
  const vec   mixprob   = find_mixture_indicator_cdf(trans(U - aux_omega_n*aux_h_n));
  aux_S_n               = trans(inverse_transform_sampling(mixprob, T));
  
  rowvec    alpha_S(T);
  rowvec    sigma_S_inv(T);
  for (int t=0; t<T; t++) {
    alpha_S.col(t)      = alpha_s(aux_S_n(t));
    sigma_S_inv.col(t)  = 1/sigma_s(aux_S_n(t));
  }
  
  // sample aux_s_n
  if ( sample_s_ ) {
    aux_s_n               = (prior_sv_s_ + 2 * aux_sigma2_omega_n)/R::rchisq(3 + 2 * prior_sv_a_);
  }
  
  // sample aux_sigma2_omega
  aux_sigma2_omega_n    = do_rgig1( prior_sv_a_-0.5, pow(aux_omega_n,2), 2/aux_s_n );
  
  // sample aux_rho
  rowvec    hm1         = aux_h_n.cols(0,T-2);
  double    aux_rho_var = as_scalar(pow(hm1*hm1.t(), -1));
  double    aux_rho_mean = as_scalar(aux_rho_var * hm1*aux_h_n.cols(1,T-1).t());
  double    upper_bound = pow(1-aux_sigma2_omega_n, 0.5);
  aux_rho_n             = RcppTN::rtn1(aux_rho_mean, pow(aux_rho_var, -0.5),-upper_bound,upper_bound);
  
  mat       H_rho_new(T, T, fill::eye);
  H_rho_new.diag(-1)   -= aux_rho_n;
  H_rho                 = H_rho_new;
  HH_rho                = H_rho_new.t() * H_rho_new;
  
  // sample aux_omega
  double    V_omega_inv = 1/( as_scalar(aux_h_n * diagmat(sigma_S_inv) * aux_h_n.t()) + pow(aux_sigma2_omega_n, -1) );
  double    omega_bar   = as_scalar(aux_h_n * diagmat(sigma_S_inv) * (U - alpha_S).t());
  double    omega_aux   = R::rnorm(V_omega_inv*omega_bar, sqrt(V_omega_inv) );
  
  // sample aux_h
  mat       V_h         = pow(omega_aux, 2) * diagmat(sigma_S_inv) + HH_rho;
  vec       h_bar       = omega_aux * diagmat(sigma_S_inv) * (U - alpha_S).t();
  rowvec    h_aux       = trans(precision_sampler_ar1( V_h.diag(), V_h(1, 0), h_bar));
  
  // ASIS
  rowvec    aux_h_tilde = omega_aux * h_aux;
  double    hHHh        = as_scalar( aux_h_tilde * HH_rho * aux_h_tilde.t() );
  double    sigma2_aux  = do_rgig1( -0.5*(T-1), hHHh, 1/aux_sigma2_omega_n );
  int       ss=1;
  if (R::runif(0,1)<0.5) ss *= -1;
  aux_omega_n           = ss * sqrt(sigma2_aux);
  aux_h_n               = aux_h_tilde / aux_omega_n;
  
  return List::create(
    _["aux_h_n"]              = aux_h_n,
    _["aux_rho_n"]            = aux_rho_n,
    _["aux_omega_n"]          = aux_omega_n,
    _["aux_sigma2_omega_n"]   = aux_sigma2_omega_n,
    _["aux_s_n"]              = aux_s_n,
    _["aux_S_n"]              = aux_S_n
  );
} // END sv_nc1


