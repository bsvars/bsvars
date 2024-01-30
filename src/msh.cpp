
#include <RcppArmadillo.h>
#include "Rcpp/Rmath.h"

#include "sample.h"

using namespace Rcpp;
using namespace arma;



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::vec Ergodic_PR_TR (
  const arma::mat&  PR_TR               // MxM
) {
  const int   M = PR_TR.n_rows;
  
  rowvec  i_mat(M, fill::ones);
  mat     A   = join_cols(eye(M,M)-PR_TR.t(),i_mat);
  mat     aj  = eye(M+1,M+1);
  
  vec     ergodic   = solve(A.t() * A, A.t() * aj.col(M));
  return ergodic;
} // END Ergodic_PR_TR
  
  
  
// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::mat count_regime_transitions (
    const arma::mat& xi
) {
  const int M = xi.n_rows;
  const int T = xi.n_cols;
  
  mat count(M, M);
  urowvec s   = index_max( xi, 0 );
  for (int t=1; t<T; t++) {
    count( s(t-1), s(t))++;
  }
  return count;
} // END count_regime_transitions



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::rowvec rDirichlet1 (
    const arma::rowvec&   alpha     // Kx1
) {
  const int K   = alpha.size();
  rowvec    draw(K);
  for (int k=0; k<K; k++) {
    draw(k)     = randg(distr_param(alpha(k), 1.0));
  }
  return draw/sum(draw);
} // END rDirichlet1



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::rowvec rIG2_Dirichlet1 (
    const arma::rowvec&  s,           // 1xM
    const arma::rowvec&  nu           // 1xM
) {
  const int   M     = s.n_cols;
  rowvec      draw  = s;
  for (int m=0; m<M; m++) {
    draw(m)        /= chi2rnd(nu(m));
  }
  return draw/sum(draw);
} // END rIG2_Dirichlet1



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::mat filtering_msh (
  const arma::mat&  U,                  // NxT
  const arma::mat&  sigma,              // NxM
  const arma::mat&  PR_TR,              // MxM
  const arma::vec&  pi_0                // Mx1
) {
  const int   T = U.n_cols;
  const int   N = U.n_rows;
  const int   M = PR_TR.n_rows;
  
  mat         eta_t(M, T);
  mat         xi_t_t(M, T);
  
  // This loop evaluates mvnormal pdf at U - simplified operations for zero-mean diagonal-covariance case
  for (int m=0; m<M; m++) {
    rowvec log_d    = -0.5 * sum(pow( pow(sigma.col(m), -0.5) % U.each_col(), 2), 0);
    log_d          += -0.5 * N * log(2*M_PI) - 0.5 * log(prod(sigma.col(m)));
    NumericVector   exp_log_d   = wrap(exp(log_d));
    exp_log_d[exp_log_d==0]     = 1e-300;
    eta_t.row(m)    = as<rowvec>(exp_log_d);
  } // END m loop
  
  vec xi_tm1_tm1    = pi_0;
  
  for (int t=0; t<T; t++) {
    vec     num     = eta_t.col(t) % (PR_TR.t() * xi_tm1_tm1);
    double  den     = sum(num);
    xi_t_t.col(t)   = num/den;
    xi_tm1_tm1      = xi_t_t.col(t);
  } // END t loop

  return xi_t_t;
} // END filtering_msh



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::mat smoothing_msh (
  const arma::mat&  U,                  // NxT
  const arma::mat&  PR_TR,              // MxM
  const arma::mat&  filtered            // MxT
) {
  const int   T = U.n_cols;
  const int   M = PR_TR.n_rows;
  
  mat   smoothed(M, T);
  smoothed.col(T-1)   = filtered.col(T-1);
  
  for (int t=T-2; t>=0; --t) {
    smoothed.col(t)   = (PR_TR * (smoothed.col(t+1)/(PR_TR.t() * filtered.col(t)) )) % filtered.col(t);
    if (any(smoothed.col(t) < 0) || any(smoothed.col(t) > 1)) {
      for (int m=0; m<M; m++) {
        if (smoothed(m,t) > 1) {smoothed(m,t) = 1;}
        if (smoothed(m,t) < 0) {smoothed(m,t) = 0;}
      }
      smoothed.col(t) = smoothed.col(t) / accu(smoothed.col(t));
    }
  } // END t loop
  
  return smoothed;
} // smoothing_msh



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::mat sample_Markov_process_msh (
    arma::mat&        aux_xi,             // MxT
    const arma::mat&  U,                  // NxT
    const arma::mat&  aux_sigma2,         // NxM
    const arma::mat&  aux_PR_TR,         // MxM
    const arma::vec&  aux_pi_0,          // Mx1
    const bool        finiteM = true
) {
  // the function changes the value of aux_xi by reference (filling it with a new draw)
  
  int minimum_regime_occurrences = 0;
  int max_iterations = 1;
  if ( finiteM ) {
    minimum_regime_occurrences = 2;
    max_iterations = 10;
  }
  
  const int   T   = U.n_cols;
  const int   M   = aux_PR_TR.n_rows;
  mat aux_xi_tmp  = aux_xi;
  
  mat filtered    = filtering_msh(U, aux_sigma2, aux_PR_TR, aux_pi_0);
  mat smoothed    = smoothing_msh(U, aux_PR_TR, filtered);
  mat     aj      = eye(M, M);
  
  mat xi(M, T);
  int draw        = csample_num1(wrap(seq_len(M)), wrap(smoothed.col(T-1)));
  aux_xi_tmp.col(T-1)     = aj.col(draw-1);
  
  if ( minimum_regime_occurrences==0 ) {
    for (int t=T-2; t>=0; --t) {
      vec xi_Tmj    = (aux_PR_TR * (aux_xi.col(t+1)/(aux_PR_TR.t() * filtered.col(t)))) % filtered.col(t);
      draw          = csample_num1(wrap(seq_len(M)), wrap(xi_Tmj));
      aux_xi_tmp.col(t)   = aj.col(draw-1);
    }
    aux_xi = aux_xi_tmp;
  } else {
    int regime_occurrences  = 1;
    int iterations  = 1;
    while ( (regime_occurrences<minimum_regime_occurrences) & (iterations<max_iterations) ) {
      for (int t=T-2; t>=0; --t) {
        vec xi_Tmj    = (aux_PR_TR * (aux_xi.col(t+1)/(aux_PR_TR.t() * filtered.col(t)))) % filtered.col(t);
        draw          = csample_num1(wrap(seq_len(M)), wrap(xi_Tmj));
        aux_xi_tmp.col(t)   = aj.col(draw-1);
      }
      mat transitions       = count_regime_transitions(aux_xi_tmp);
      regime_occurrences    = min(transitions.diag());
      iterations++;
    } // END while
    if ( iterations<max_iterations ) aux_xi = aux_xi_tmp;
  }
  
  return aux_xi;
} // END sample_Markov_process_msh











// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::cube sample_Markov_process_hmsh (
    arma::cube&       aux_xi,             // MxTxN
    const arma::mat&  U,                  // NxT
    const arma::mat&  aux_sigma2,         // NxM
    const arma::cube& aux_PR_TR,          // MxMxN
    const arma::mat&  aux_pi_0,           // MxN
    const bool        finiteM = true
) {
  
  int minimum_regime_occurrences = 0;
  int max_iterations = 1;
  if ( finiteM ) {
    minimum_regime_occurrences = 2;
    max_iterations = 10;
  }
  
  const int   T   = U.n_cols;
  const int   N   = U.n_rows;
  const int   M   = aux_PR_TR.n_cols;
  cube aux_xi_tmp = aux_xi;
  mat xi(M, T);
  
  mat     aj      = eye(M, M);
  
  for (int n=0; n<N; n++) {
    mat filtered    = filtering_msh(U.row(n), aux_sigma2.row(n), aux_PR_TR.slice(n), aux_pi_0.col(n));
    mat smoothed    = smoothing_msh(U.row(n), aux_PR_TR.slice(n), filtered);
    int draw        = csample_num1(wrap(seq_len(M)), wrap(smoothed.col(T-1)));
    aux_xi_tmp.slice(n).col(T-1)     = aj.col(draw-1);
    
    if ( minimum_regime_occurrences==0 ) {
      for (int t=T-2; t>=0; --t) {
        vec xi_Tmj    = (aux_PR_TR.slice(n) * (aux_xi.slice(n).col(t+1)/(aux_PR_TR.slice(n).t() * filtered.col(t)))) % filtered.col(t);
        draw          = csample_num1(wrap(seq_len(M)), wrap(xi_Tmj));
        aux_xi_tmp.slice(n).col(t)   = aj.col(draw-1);
      }
      aux_xi = aux_xi_tmp;
    } else {
      int regime_occurrences  = 1;
      int iterations  = 1;
      while ( (regime_occurrences<minimum_regime_occurrences) & (iterations<max_iterations) ) {
        for (int t=T-2; t>=0; --t) {
          vec xi_Tmj    = (aux_PR_TR.slice(n) * (aux_xi.slice(n).col(t+1)/(aux_PR_TR.slice(n).t() * filtered.col(t)))) % filtered.col(t);
          draw          = csample_num1(wrap(seq_len(M)), wrap(xi_Tmj));
          aux_xi_tmp.slice(n).col(t)   = aj.col(draw-1);
        } // END t loop
        
        mat transitions       = count_regime_transitions(aux_xi_tmp.slice(n));
        regime_occurrences    = min(transitions.diag());
        iterations++;
      } // END while
      
      if ( iterations<max_iterations ) aux_xi = aux_xi_tmp;
    } // END if else
  } // END n loop
  
  return aux_xi;
} // END sample_Markov_process_hmsh














// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
Rcpp::List sample_transition_probabilities (
    arma::mat           aux_PR_TR,    // MxM 
    arma::vec           aux_pi_0,     // Mx1
    const arma::mat&    aux_xi,       // MxT
    const Rcpp::List&   prior,         // a list of priors - original dimensions
    const bool          MSnotMIX = true
) {
  // the function changes the value of aux_PR_TR and aux_pi_0 by reference (filling it with a new draw)
  const int   M           = aux_PR_TR.n_rows;
  const mat   prior_PR_TR = as<mat>(prior["PR_TR"]);
  
  if ( MSnotMIX ) {
    mat transitions       = count_regime_transitions(aux_xi);
    mat posterior_alpha   = transitions + prior_PR_TR;
    
    for (int m=0; m<M; m++) {
      aux_PR_TR.row(m)    = rDirichlet1(posterior_alpha.row(m));
    }
    vec prob_xi1          = aux_PR_TR *aux_xi.col(0);
    prob_xi1             /= sum(prob_xi1);
    int S0_draw           = csample_num1(wrap(seq_len(M)), wrap(prob_xi1));
    rowvec posterior_alpha_0(M, fill::value((1.0)));
    posterior_alpha_0(S0_draw-1)++;
    aux_pi_0              = trans(rDirichlet1(posterior_alpha_0));
  } else {
    rowvec occurrences    = trans(sum(aux_xi, 1));
    rowvec posterior_alpha= occurrences + prior_PR_TR.row(0);
    aux_pi_0              = trans(rDirichlet1(posterior_alpha));
    for (int m=0; m<M; m++) {
      aux_PR_TR.row(m)    = aux_pi_0.t();
    }
  }
  
  return List::create(
    _["PR_TR"]        = aux_PR_TR,
    _["pi_0"]         = aux_pi_0
  );
} // END sample_transition_probabilities



// [[Rcpp::interfaces(cpp, r)]]
// [[Rcpp::export]]
arma::mat sample_variances_msh (
    arma::mat&          aux_sigma2, // NxM
    const arma::mat&    aux_B,      // NxN
    const arma::mat&    aux_A,      // NxK
    const arma::mat&    Y,          // NxT dependent variables
    const arma::mat&    X,          // KxT explanatory variables
    const arma::mat&    aux_xi,     // MxT state variables
    const Rcpp::List&   prior       // a list of priors - original dimensions
) {
  // the function changes the value of aux_sigma2 by reference (filling it with a new draw)
  const int   M     = aux_xi.n_rows;
  const int   N     = aux_B.n_rows;
  const int   T     = Y.n_cols;
  const double MM   = M;
  
  rowvec posterior_nu   = sum(aux_xi, 1).t() + as<double>(prior["sigma_nu"]);
  mat posterior_s(N, M);
  posterior_s.fill(prior["sigma_s"]);
  for (int m=0; m<M; m++) {
    for (int t=0; t<T; t++) {
      if (aux_xi(m,t)==1) {
        posterior_s.col(m) += square(aux_B * (Y.col(t) - aux_A * X.col(t)));
      }
    }
  }
  // This is the version with restriction sum(aux_sigma2,0) = M
  for (int n=0; n<N; n++) {
    aux_sigma2.row(n)     = MM*rIG2_Dirichlet1( posterior_s.row(n), posterior_nu);
  }
  
  return aux_sigma2;
} // END sample_variances_msh

