
#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"
#include <RcppTN.h>

using namespace Rcpp;
using namespace arma;


/*______________________function univar_rgig_newapproach1______________________*/
// utility function from file rgig1.cpp used in function rgig1
double univar_rgig_newapproach1 (
    double lambda, 
    double lambda_old, 
    double omega, 
    double alpha
){
  double A[3], Atot;
  double k0;
  double k1, k2;
  double xm;
  double x0;
  double a;
  double U, V, X;
  double hx;
  int count = 0;
  double res;
  
  if (lambda >= 1. || omega >1.)
    throw ("invalid parameters");
  xm = omega / (std::sqrt((1.-lambda)*(1.-lambda) + omega*omega)+(1.-lambda));
  x0 = omega/(1.-lambda);
  k0 = std::exp((lambda-1.)*std::log(xm) - 0.5*omega*(xm + 1./xm));
  A[0] = k0 * x0;
  if (x0 >= 2./omega) {
    k1 = 0.;
    A[1] = 0.;
    k2 = pow(x0, lambda-1.);
    A[2] = k2 * 2. * std::exp(-omega*x0/2.)/omega;
  } else {
    k1 = std::exp(-omega);
    A[1] = (lambda == 0.)
      ? k1 * std::log(2./(omega*omega))
        : k1 / lambda * ( std::pow(2./omega, lambda) - std::pow(x0, lambda) );
    k2 = std::pow(2/omega, lambda-1.);
    A[2] = k2 * 2 * std::exp(-1.)/omega;
  }
  Atot = A[0] + A[1] + A[2];
  do {
    ++count;
    V = Atot * R::runif(0, 1);
    do {
      if (V <= A[0]) {
        X = x0 * V / A[0];
        hx = k0;
        break;
      }
      V -= A[0];
      if (V <= A[1]) {
        if (lambda == 0.) {
          X = omega * std::exp(std::exp(omega)*V);
          hx = k1 / X;
        }
        else {
          X = std::pow(std::pow(x0, lambda) + (lambda / k1 * V), 1./lambda);
          hx = k1 * std::pow(X, lambda-1.);
        }
        break;
      }
      V -= A[1];
      a = (x0 > 2./omega) ? x0 : 2./omega;
      X = -2./omega * std::log(std::exp(-omega/2. * a) - omega/(2.*k2) * V);
      hx = k2 * std::exp(-omega/2. * X);
      break;
    } while(0);
    U = R::runif(0, 1) * hx;
    if (std::log(U) <= (lambda-1.) * std::log(X) - omega/2. * (X+1./X)) {
      res = (lambda_old < 0.) ? (alpha / X) : (alpha * X);
      break;
    }
  } while(1);
  return res;
} // END univar_rgig_newapproach1



/*______________________function rgig1______________________*/
// utility function from file rgig1.cpp
double rgig1(
    double lambda,
    double chi,
    double psi
) {
  if (chi == 0){
    chi = DBL_MIN;
  }
  if ( !(R_FINITE(lambda) && R_FINITE(chi) && R_FINITE(psi)) ||
       (chi <  0. || psi < 0)      ||
       (chi == 0. && lambda <= 0.) ||
       (psi == 0. && lambda >= 0.) ) {
    throw std::bad_function_call();
  }
  double res;
  // circumvent GIGrvg in these cases
  if ((chi < (11 * DBL_EPSILON)) & (lambda != 0)) {
    /* special cases which are basically Gamma and Inverse Gamma distribution */
    if (lambda > 0.0) {
      res = R::rgamma(lambda, 2.0/psi);
    }
    else {
      res = 1.0/R::rgamma(-lambda, 2.0/chi); // fixed
    }
  }
  
  else if ((psi < (11 * DBL_EPSILON)) & (lambda != 0)) {
    /* special cases which are basically Gamma and Inverse Gamma distribution */
    if (lambda > 0.0) {
      res = R::rgamma(lambda, 2.0/psi);  // fixed
    }
    else {
      res = 1.0/R::rgamma(-lambda, 2.0/chi); // fixed
    }
    
  } else if ((lambda == 0) & (sqrt(psi*chi) > 0) & (sqrt(psi*chi) < 1)) {
    res = univar_rgig_newapproach1(lambda, lambda, sqrt(psi*chi), sqrt(chi/psi));
  } else {
    SEXP (*fun)(int, double, double, double) = NULL;
    if (!fun) fun = (SEXP(*)(int, double, double, double)) R_GetCCallable("GIGrvg", "do_rgig");
    
    res = as<double>(fun(1, lambda, chi, psi));
  }
  return res;
} // END rgig1



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
  aux_sigma2_omega_n    = rgig1( prior_sv_a_-0.5, pow(aux_omega_n,2), 2/aux_s_n );
  
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
  double    sigma2_aux  = rgig1( -0.5*(T-1), hHHh, 1/aux_sigma2_omega_n );
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


