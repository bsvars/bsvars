
#' @title Provides posterior summary of homoskedastic Structural VAR estimation
#'
#' @description Provides posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper parameters.
#' 
#' @param object an object of class PosteriorBSVAR obtained using the
#' \code{estimate()} function applied to homoskedastic Bayesian Structural VAR
#' model specification set by function \code{specify_bsvar$new()} containing 
#' draws from the  posterior distribution of the parameters. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper-parameters.
#' 
#' @method summary PosteriorBSVAR
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' summary(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   summary()
#' 
#' @export
summary.PosteriorBSVAR = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of the parameters             |\n",
    "**************************************************|\n"
  )

  N         = dim(object$posterior$A)[1]
  p         = object$last_draw$p
  K         = dim(object$last_draw$data_matrices$X)[1]
  d         = K - N * p
  
  out       = list()
  out$B     = list()
  out$A     = list()
  out$hyper = list()
  
  param     = c("B", "A")
  
  for (n in 1:N) {
    which_par = which(colSums(object$last_draw$identification$VB[[n]]) == 1)
    out$B[[n]] = matrix(
      cbind(
        apply(object$posterior$B[n,,], 1, mean),
        apply(object$posterior$B[n,,], 1, sd),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.05),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.95)
      )[which_par,],
      ncol = 4
    )
    colnames(out$B[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$B[[n]]) = paste0("B[", n, ",", which_par, "]")  
    
    out$A[[n]] = cbind(
      apply(object$posterior$A[n,,], 1, mean),
      apply(object$posterior$A[n,,], 1, sd),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.05),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.95)
    )
    colnames(out$A[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    
    Anames  = c(
      paste0(
        rep("lag", p * N),
        kronecker((1:p), rep(1, N)),
        rep("_var", p * N),
        kronecker((1:N), rep(1, p))
      ),
      "const"
    )
    if (d > 1) {
      Anames = c(Anames, paste0("exo", 1:(d - 1)))
    }
    rownames(out$A[[n]]) = Anames
  } # END n loop
  
  names(out$B) = paste0("equation", 1:N)
  names(out$A) = paste0("equation", 1:N)
  
  for (i in 1:2) {
    out$hyper[[i]] = cbind(
      apply(object$posterior$hyper[,i,], 1, mean),
      apply(object$posterior$hyper[,i,], 1, sd),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.05),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.95)
    )
  
    colnames(out$hyper[[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$hyper[[i]]) = c(
      paste0(
        rep(param[i], N),
        "[",
        kronecker(rep(1, 2), (1:N)),
        c(rep(",]_shrinkage", N), rep(",]_shrinkage_scale", N))
      ),
      paste0(param[i], "_global_scale")
    )
  } # END i loop
  names(out$hyper) = param
  
  return(out)
} # END summary.PosteriorBSVAR







#' @title Provides posterior summary of heteroskedastic Structural VAR estimation
#'
#' @description Provides posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper parameters.
#' 
#' @param object an object of class PosteriorBSVARSV obtained using the
#' \code{estimate()} function applied to heteroskedastic Bayesian Structural VAR
#' model specification set by function \code{specify_bsvar_sv$new()} containing 
#' draws from the  posterior distribution of the parameters. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper-parameters.
#' 
#' @method summary PosteriorBSVARSV
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_sv}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' summary(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   summary()
#' 
#' @export
summary.PosteriorBSVARSV = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of the parameters             |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object$posterior$A)[1]
  p         = object$last_draw$p
  K         = dim(object$last_draw$data_matrices$X)[1]
  d         = K - N * p
  
  out       = list()
  out$B     = list()
  out$A     = list()
  out$hyper = list()
  
  param     = c("B", "A")
  
  for (n in 1:N) {
    which_par = which(colSums(object$last_draw$identification$VB[[n]]) == 1)
    out$B[[n]] = matrix(
      cbind(
        apply(object$posterior$B[n,,], 1, mean),
        apply(object$posterior$B[n,,], 1, sd),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.05),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.95)
      )[which_par,],
      ncol = 4
    )
    colnames(out$B[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$B[[n]]) = paste0("B[", n, ",", which_par, "]")  
    
    out$A[[n]] = cbind(
      apply(object$posterior$A[n,,], 1, mean),
      apply(object$posterior$A[n,,], 1, sd),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.05),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.95)
    )
    colnames(out$A[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    
    Anames  = c(
      paste0(
        rep("lag", p * N),
        kronecker((1:p), rep(1, N)),
        rep("_var", p * N),
        kronecker((1:N), rep(1, p))
      ),
      "const"
    )
    if (d > 1) {
      Anames = c(Anames, paste0("exo", 1:(d - 1)))
    }
    rownames(out$A[[n]]) = Anames
  } # END n loop
  
  names(out$B) = paste0("equation", 1:N)
  names(out$A) = paste0("equation", 1:N)
  
  for (i in 1:2) {
    out$hyper[[i]] = cbind(
      apply(object$posterior$hyper[,i,], 1, mean),
      apply(object$posterior$hyper[,i,], 1, sd),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.05),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.95)
    )
    
    colnames(out$hyper[[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$hyper[[i]]) = c(
      paste0(
        rep(param[i], N),
        "[",
        kronecker(rep(1, 2), (1:N)),
        c(rep(",]_shrinkage", N), rep(",]_shrinkage_scale", N))
      ),
      paste0(param[i], "_global_scale")
    )
  } # END i loop
  names(out$hyper) = param
  
  return(out)
} # END summary.PosteriorBSVARSV










#' @title Provides posterior summary of heteroskedastic Structural VAR estimation
#'
#' @description Provides posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper parameters.
#' 
#' @param object an object of class PosteriorBSVARMSH obtained using the
#' \code{estimate()} function applied to heteroskedastic Bayesian Structural VAR
#' model specification set by function \code{specify_bsvar_msh$new()} containing 
#' draws from the  posterior distribution of the parameters. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper-parameters.
#' 
#' @method summary PosteriorBSVARMSH
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_msh}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' summary(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   summary()
#' 
#' @export
summary.PosteriorBSVARMSH = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of the parameters             |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object$posterior$A)[1]
  p         = object$last_draw$p
  K         = dim(object$last_draw$data_matrices$X)[1]
  d         = K - N * p
  
  out       = list()
  out$B     = list()
  out$A     = list()
  out$hyper = list()
  
  param     = c("B", "A")
  
  for (n in 1:N) {
    which_par = which(colSums(object$last_draw$identification$VB[[n]]) == 1)
    out$B[[n]] = matrix(
      cbind(
        apply(object$posterior$B[n,,], 1, mean),
        apply(object$posterior$B[n,,], 1, sd),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.05),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.95)
      )[which_par,],
      ncol = 4
    )
    colnames(out$B[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$B[[n]]) = paste0("B[", n, ",", which_par, "]")  
    
    out$A[[n]] = cbind(
      apply(object$posterior$A[n,,], 1, mean),
      apply(object$posterior$A[n,,], 1, sd),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.05),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.95)
    )
    colnames(out$A[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    
    Anames  = c(
      paste0(
        rep("lag", p * N),
        kronecker((1:p), rep(1, N)),
        rep("_var", p * N),
        kronecker((1:N), rep(1, p))
      ),
      "const"
    )
    if (d > 1) {
      Anames = c(Anames, paste0("exo", 1:(d - 1)))
    }
    rownames(out$A[[n]]) = Anames
  } # END n loop
  
  names(out$B) = paste0("equation", 1:N)
  names(out$A) = paste0("equation", 1:N)
  
  for (i in 1:2) {
    out$hyper[[i]] = cbind(
      apply(object$posterior$hyper[,i,], 1, mean),
      apply(object$posterior$hyper[,i,], 1, sd),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.05),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.95)
    )
    
    colnames(out$hyper[[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$hyper[[i]]) = c(
      paste0(
        rep(param[i], N),
        "[",
        kronecker(rep(1, 2), (1:N)),
        c(rep(",]_shrinkage", N), rep(",]_shrinkage_scale", N))
      ),
      paste0(param[i], "_global_scale")
    )
  } # END i loop
  names(out$hyper) = param
  
  return(out)
} # END summary.PosteriorBSVARMSH









#' @title Provides posterior summary of non-normal Structural VAR estimation
#'
#' @description Provides posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper parameters.
#' 
#' @param object an object of class PosteriorBSVARMIX obtained using the
#' \code{estimate()} function applied to non-normal Bayesian Structural VAR
#' model specification set by function \code{specify_bsvar_mix$new()} containing 
#' draws from the  posterior distribution of the parameters. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, and hyper-parameters.
#' 
#' @method summary PosteriorBSVARMIX
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_mix}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' summary(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   summary()
#' 
#' @export
summary.PosteriorBSVARMIX = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of the parameters             |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object$posterior$A)[1]
  p         = object$last_draw$p
  K         = dim(object$last_draw$data_matrices$X)[1]
  d         = K - N * p
  
  out       = list()
  out$B     = list()
  out$A     = list()
  out$hyper = list()
  
  param     = c("B", "A")
  
  for (n in 1:N) {
    which_par = which(colSums(object$last_draw$identification$VB[[n]]) == 1)
    out$B[[n]] = matrix(
      cbind(
        apply(object$posterior$B[n,,], 1, mean),
        apply(object$posterior$B[n,,], 1, sd),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.05),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.95)
      )[which_par,],
      ncol = 4
    )
    colnames(out$B[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$B[[n]]) = paste0("B[", n, ",", which_par, "]")  
    
    out$A[[n]] = cbind(
      apply(object$posterior$A[n,,], 1, mean),
      apply(object$posterior$A[n,,], 1, sd),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.05),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.95)
    )
    colnames(out$A[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    
    Anames  = c(
      paste0(
        rep("lag", p * N),
        kronecker((1:p), rep(1, N)),
        rep("_var", p * N),
        kronecker((1:N), rep(1, p))
      ),
      "const"
    )
    if (d > 1) {
      Anames = c(Anames, paste0("exo", 1:(d - 1)))
    }
    rownames(out$A[[n]]) = Anames
  } # END n loop
  
  names(out$B) = paste0("equation", 1:N)
  names(out$A) = paste0("equation", 1:N)
  
  for (i in 1:2) {
    out$hyper[[i]] = cbind(
      apply(object$posterior$hyper[,i,], 1, mean),
      apply(object$posterior$hyper[,i,], 1, sd),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.05),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.95)
    )
    
    colnames(out$hyper[[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$hyper[[i]]) = c(
      paste0(
        rep(param[i], N),
        "[",
        kronecker(rep(1, 2), (1:N)),
        c(rep(",]_shrinkage", N), rep(",]_shrinkage_scale", N))
      ),
      paste0(param[i], "_global_scale")
    )
  } # END i loop
  names(out$hyper) = param
  
  return(out)
} # END summary.PosteriorBSVARMIX



#' @title Provides posterior summary of Structural VAR with t-distributed shocks estimation
#'
#' @description Provides posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, hyper-parameters, and Student-t degrees-of-freedom 
#' parameter \eqn{\nu}.
#' 
#' @param object an object of class PosteriorBSVART obtained using the
#' \code{estimate()} function applied to homoskedastic Bayesian Structural VAR
#' model specification set by function \code{specify_bsvar$new()} containing 
#' draws from the  posterior distribution of the parameters. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 5 and 95 
#' percentiles of the parameters: the structural matrix \eqn{B}, autoregressive 
#' parameters \eqn{A}, hyper-parameters, and Student-t degrees-of-freedom 
#' parameter \eqn{\nu}.
#' 
#' @method summary PosteriorBSVART
#' 
#' @seealso \code{\link{estimate}}, \code{\link{specify_bsvar_t}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' summary(posterior)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   summary()
#' 
#' @export
summary.PosteriorBSVART = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of the parameters             |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object$posterior$A)[1]
  p         = object$last_draw$p
  K         = dim(object$last_draw$data_matrices$X)[1]
  d         = K - N * p
  
  out       = list()
  out$B     = list()
  out$A     = list()
  out$hyper = list()
  
  param     = c("B", "A")
  
  for (n in 1:N) {
    which_par = which(colSums(object$last_draw$identification$VB[[n]]) == 1)
    out$B[[n]] = matrix(
      cbind(
        apply(object$posterior$B[n,,], 1, mean),
        apply(object$posterior$B[n,,], 1, sd),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.05),
        apply(object$posterior$B[n,,], 1, quantile, probs = 0.95)
      )[which_par,],
      ncol = 4
    )
    colnames(out$B[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$B[[n]]) = paste0("B[", n, ",", which_par, "]")  
    
    out$A[[n]] = cbind(
      apply(object$posterior$A[n,,], 1, mean),
      apply(object$posterior$A[n,,], 1, sd),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.05),
      apply(object$posterior$A[n,,], 1, quantile, probs = 0.95)
    )
    colnames(out$A[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    
    Anames  = c(
      paste0(
        rep("lag", p * N),
        kronecker((1:p), rep(1, N)),
        rep("_var", p * N),
        kronecker((1:N), rep(1, p))
      ),
      "const"
    )
    if (d > 1) {
      Anames = c(Anames, paste0("exo", 1:(d - 1)))
    }
    rownames(out$A[[n]]) = Anames
  } # END n loop
  
  names(out$B) = paste0("equation", 1:N)
  names(out$A) = paste0("equation", 1:N)
  
  for (i in 1:2) {
    out$hyper[[i]] = cbind(
      apply(object$posterior$hyper[,i,], 1, mean),
      apply(object$posterior$hyper[,i,], 1, sd),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.05),
      apply(object$posterior$hyper[,i,], 1, quantile, probs = 0.95)
    )
    
    colnames(out$hyper[[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out$hyper[[i]]) = c(
      paste0(
        rep(param[i], N),
        "[",
        kronecker(rep(1, 2), (1:N)),
        c(rep(",]_shrinkage", N), rep(",]_shrinkage_scale", N))
      ),
      paste0(param[i], "_global_scale")
    )
  } # END i loop
  names(out$hyper) = param
  
  out$df = c(
    mean(object$posterior$df),
    sd(object$posterior$df),
    quantile(object$posterior$df, probs = 0.05),
    quantile(object$posterior$df, probs = 0.95)
  )
  names(out$df) = c("mean", "sd", "5% quantile", "95% quantile")
  
  return(out)
} # END summary.PosteriorBSVART






#' @title Provides posterior summary of structural shocks' conditional standard 
#' deviations
#'
#' @description Provides posterior summary of structural shocks' conditional 
#' standard deviations including their mean, standard deviations, as well as 
#' 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorSigma obtained using the
#' \code{compute_conditional_sd()} function containing posterior draws of 
#' conditional standard deviations of structural shocks.
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the structural shocks' conditional standard deviations
#' for each of the shocks and periods.
#' 
#' @method summary PosteriorSigma
#' 
#' @seealso \code{\link{compute_conditional_sd}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 5)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 5)
#' 
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' sigma_summary  = summary(sigma)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_conditional_sd() |>
#'   summary() -> sigma_summary
#' 
#' @export
summary.PosteriorSigma = function(
    object,
    ...
) {
  
  stopifnot("The model is homoskedastic. Conditional sd is equal to 1 for all variables and periods." = any(object != 1))
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of structural shocks'         |\n",
    "    conditional standard deviations               |\n",
    "**************************************************|\n"
  )
  
  posterior_sigma = object
  
  N         = dim(posterior_sigma)[1]
  T         = dim(posterior_sigma)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(posterior_sigma[n,,], 1, mean),
      apply(posterior_sigma[n,,], 1, sd),
      t(apply(posterior_sigma[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("shock", 1:N)
  
  return(out)
} # END summary.PosteriorSigma






#' @title Provides posterior summary of variables' fitted values
#'
#' @description Provides posterior summary of the fitted values including their 
#' mean, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorFitted obtained using the
#' \code{compute_fitted_values()} function containing draws the predictive 
#' density of the sample data. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the fitted values for each of the shocks and periods.
#' 
#' @method summary PosteriorFitted
#' 
#' @seealso \code{\link{compute_fitted_values}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute fitted values
#' fitted         = compute_fitted_values(posterior)
#' fitted_summary = summary(fitted)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_fitted_values() |>
#'   summary() -> fitted_summary
#' 
#' @export
summary.PosteriorFitted = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of fitted values              |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object)[1]
  T         = dim(object)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(object[n,,], 1, mean),
      apply(object[n,,], 1, sd),
      t(apply(object[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.PosteriorFitted











#' @title Provides posterior summary of historical decompositions
#'
#' @description Provides posterior means of the historical decompositions variable
#' by variable.
#' 
#' @param object an object of class PosteriorHD obtained using the
#' \code{compute_historical_decompositions()} function containing posterior draws
#'  of historical decompositions. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior means of historical decompositions for
#' each of the variables.
#' 
#' @method summary PosteriorHD
#' 
#' @seealso \code{\link{compute_historical_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(diff(us_fiscal_lsuw))
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute historical decompositions
#' hds            = compute_historical_decompositions(posterior)
#' hds_summary    = summary(hds)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' diff(us_fiscal_lsuw) |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_historical_decompositions() |>
#'   summary() -> hds_summary
#' 
#' @export
summary.PosteriorHD = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior means of historical decompositions    |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object)[1]
  T         = dim(object)[3]
  
  out       = list()
  hds       = apply(object, 1:3, mean)
  for (n in 1:N) {
    out[[n]]    = t(hds[n,,])
    colnames(out[[n]]) = paste0("shock ", 1:N)
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.PosteriorHD







#' @title Provides posterior summary of impulse responses
#'
#' @description Provides posterior summary of the impulse responses of each 
#' variable to each of the shocks at all horizons. Includes their posterior 
#' means, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorIR obtained using the
#' \code{compute_impulse_responses()} function containing draws from the posterior
#' distribution of the impulse responses. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the impulse responses of each variable to each of the 
#' shocks at all horizons.
#' 
#' @method summary PosteriorIR
#' 
#' @seealso \code{\link{compute_impulse_responses}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute impulse responses
#' irf            = compute_impulse_responses(posterior, horizon = 4)
#' irf_summary    = summary(irf)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_impulse_responses(horizon = 4) |>
#'   summary() -> irf_summary
#' 
#' @export
summary.PosteriorIR = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of impulse responses          |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object)[1]
  H         = dim(object)[3] - 1
  
  out       = list()
  for (n in 1:N) {
    out[[n]] = list()
    for (i in 1:N) {
      out[[n]][[i]]    = cbind(
        apply(object[i,n,,], 1, mean),
        apply(object[i,n,,], 1, sd),
        t(apply(object[i,n,,], 1, quantile, probs = c(0.05, 0.95)))
      )
      colnames(out[[n]][[i]]) = c("mean", "sd", "5% quantile", "95% quantile")
      rownames(out[[n]][[i]]) = 0:H
    } # END i loop
    names(out[[n]]) = paste0("variable", 1:N)
  } # END n loop
  names(out) = paste0("shock", 1:N)
  
  return(out)
} # END summary.PosteriorIR






#' @title Provides posterior summary of regime probabilities
#'
#' @description Provides posterior summary of regime probabilities 
#' including their mean, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorRegimePr obtained using the
#' \code{compute_regime_probabilities()} function containing posterior draws of 
#' regime allocations.
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean and standard deviations of the
#' regime probabilities.
#' 
#' @method summary PosteriorRegimePr
#' 
#' @seealso \code{\link{compute_regime_probabilities}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute regime probabilities
#' rp             = compute_regime_probabilities(posterior)
#' rp_summary     = summary(rp)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_regime_probabilities() |>
#'   summary() -> rp_summary
#' 
#' @export
summary.PosteriorRegimePr = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of regime probabilities       |\n",
    "**************************************************|\n"
  )
  
  M         = dim(object)[1]
  T         = dim(object)[2]
  
  out       = list()
  for (m in 1:M) {
    out[[m]]    = cbind(
      apply(object[m,,], 1, mean),
      apply(object[m,,], 1, sd)
    )
    colnames(out[[m]]) = c("mean", "sd")
    rownames(out[[m]]) = 1:T
  } # END n loop
  
  names(out) = paste0("regime", 1:M)
  
  return(out)
} # END summary.PosteriorRegimePr






#' @title Provides posterior summary of structural shocks
#'
#' @description Provides posterior summary of the structural shocks including their 
#' mean, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class PosteriorShocks obtained using the
#' \code{compute_structural_shocks()} function containing draws the posterior
#' distribution of the structural shocks. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the structural shocks for each of the equations and periods.
#' 
#' @method summary PosteriorShocks
#' 
#' @seealso \code{\link{compute_structural_shocks}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute structural shocks
#' shocks         = compute_structural_shocks(posterior)
#' shocks_summary = summary(shocks)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_structural_shocks() |>
#'   summary() -> shocks_summary
#' 
#' @export
summary.PosteriorShocks = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of structural shocks          |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object)[1]
  T         = dim(object)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(object[n,,], 1, mean),
      apply(object[n,,], 1, sd),
      t(apply(object[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:T
  } # END n loop
  
  names(out) = paste0("shock", 1:N)
  
  return(out)
} # END summary.PosteriorShocks







#' @title Provides posterior summary of forecast error variance decompositions
#'
#' @description Provides posterior means of the forecast error variance 
#' decompositions of each variable at all horizons.
#' 
#' @param object an object of class PosteriorFEVD obtained using the
#' \code{compute_variance_decompositions()} function containing draws from the 
#' posterior distribution of the forecast error variance decompositions. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean of the forecast error variance 
#' decompositions of each variable at all horizons.
#' 
#' @method summary PosteriorFEVD
#' 
#' @seealso \code{\link{compute_variance_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute forecast error variance decompositions
#' fevd           = compute_variance_decompositions(posterior, horizon = 4)
#' fevd_summary   = summary(fevd)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   compute_variance_decompositions(horizon = 4) |>
#'   summary() -> fevd_summary
#' 
#' @export
summary.PosteriorFEVD = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior means of forecast error               |\n",
    "    variance decompositions                       |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object)[1]
  H         = dim(object)[3] - 1
  
  fevd      = apply(object, 1:3, mean)
  out       = list()
  for (n in 1:N) {
    out[[n]] = t(fevd[n,,])
    colnames(out[[n]]) = paste0("shock", 1:N)
    rownames(out[[n]]) = 0:H
  } # END n loop
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.PosteriorFEVD






#' @title Provides posterior summary of Forecasts
#'
#' @description Provides posterior summary of the forecasts including their 
#' mean, standard deviations, as well as 5 and 95 percentiles.
#' 
#' @param object an object of class Forecasts obtained using the
#' \code{forecast()} function containing draws the predictive density. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A list reporting the posterior mean, standard deviations, as well as 
#' 5 and 95 percentiles of the forecasts for each of the variables and forecast 
#' horizons.
#' 
#' @method summary Forecasts
#' 
#' @seealso \code{\link{forecast}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar$new(us_fiscal_lsuw)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # forecast
#' fore           = forecast(posterior, horizon = 2)
#' fore_summary   = summary(fore)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) |> 
#'   forecast(horizon = 2) |>
#'   summary() -> fore_summary
#' 
#' @export
summary.Forecasts = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Posterior summary of forecasts                  |\n",
    "**************************************************|\n"
  )
  
  N         = dim(object$forecasts)[1]
  H         = dim(object$forecasts)[2]
  
  out       = list()
  for (n in 1:N) {
    out[[n]]    = cbind(
      apply(object$forecasts[n,,], 1, mean),
      apply(object$forecasts[n,,], 1, sd),
      t(apply(object$forecasts[n,,], 1, quantile, probs = c(0.05, 0.95)))
    )
    colnames(out[[n]]) = c("mean", "sd", "5% quantile", "95% quantile")
    rownames(out[[n]]) = 1:H
  } # END n loop
  
  names(out) = paste0("variable", 1:N)
  
  return(out)
} # END summary.Forecasts





#' @title Provides summary of verifying homoskedasticity
#'
#' @description Provides summary of the Savage-Dickey density ratios
#' for verification of structural shocks homoskedasticity.
#' 
#' @param object an object of class \code{SDDRvolatility} obtained using the
#' \code{verify_volatility()} function. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A table reporting the logarithm of Bayes factors of homoskedastic to
#' heteroskedastic posterior odds \code{"log(SDDR)"} for each structural shock, 
#' their numerical standard errors \code{"NSE"}, and the implied posterior 
#' probability of the homoskedasticity and heteroskedasticity hypothesis, 
#' \code{"Pr[homoskedasticity|data]"} and \code{"Pr[heteroskedasticity|data]"} 
#' respectively.
#' 
#' @method summary SDDRvolatility
#' 
#' @seealso \code{\link{verify_volatility}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 1, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_volatility(posterior)
#' summary(sddr)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_volatility() |> 
#'   summary() -> sddr_summary
#' 
#' @export
summary.SDDRvolatility = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Summary of structural shocks                    |\n",
    "     homoskedasticity verification                |\n",
    "**************************************************|\n"
  )
  
  N         = nrow(object$logSDDR)
  exp_sddr  = exp(object$logSDDR)
  
  out = cbind(
    object$logSDDR,
    object$logSDDR_se,
    exp_sddr / (1 + exp_sddr),
    1 / (1 + exp_sddr)
  )
  colnames(out) = c("log(SDDR)", "NSE", "Pr[homoskedasticity|data]", "Pr[heteroskedasticity|data]")
  rownames(out) = paste0("shock ", 1:N)
  
  return(out)
} # END summary.SDDRvolatility




#' @title Provides summary of verifying hypotheses about autoregressive parameters
#'
#' @description Provides summary of the Savage-Dickey density ratios
#' for verification of hypotheses about autoregressive parameters.
#' 
#' @param object an object of class \code{SDDRautoregression} obtained using the
#' \code{verify_autoregression()} function. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A table reporting the logarithm of Bayes factors of the restriction
#' against no restriction posterior odds in \code{"log(SDDR)"}, 
#' its numerical standard error \code{"NSE"}, and the implied posterior 
#' probability of the restriction holding or not hypothesis, 
#' \code{"Pr[H0|data]"} and \code{"Pr[H1|data]"} 
#' respectively.
#' 
#' @method summary SDDRautoregression
#' 
#' @seealso \code{\link{verify_autoregression}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 1)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify autoregression
#' H0             = matrix(NA, ncol(us_fiscal_lsuw), ncol(us_fiscal_lsuw) + 1)
#' H0[1,3]        = 0        # a hypothesis of no Granger causality from gdp to ttr
#' sddr           = verify_autoregression(posterior, H0)
#' summary(sddr)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   verify_autoregression(hypothesis = H0) |> 
#'   summary() -> sddr_summary
#' 
#' @export
summary.SDDRautoregression = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Summary of hypothesis verification              |\n",
    "     for autoregressive parameters                |\n",
    "**************************************************|\n"
  )
  
  exp_sddr  = exp(object$logSDDR)
  
  out = cbind(
    object$logSDDR,
    object$log_SDDR_se,
    exp_sddr / (1 + exp_sddr),
    1 / (1 + exp_sddr)
  )
  colnames(out) = c("log(SDDR)", "NSE", "Pr[H0|data]", "Pr[H1|data]")
  rownames(out) = ""
  
  return(out)
} # END summary.SDDRautoregression





#' @title Provides summary of verifying homoskedasticity
#'
#' @description Provides summary of the Savage-Dickey density ratios
#' for verification of structural shocks homoskedasticity. The outcomes can be
#' used to make probabilistic statements about identification through 
#' heteroskedasticity following Lütkepohl, Shang, Uzeda & Woźniak (2024).
#' 
#' @param object an object of class \code{SDDRidSV} obtained using the
#' \code{\link{verify_identification.PosteriorBSVARSV}} function. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A table reporting the logarithm of Bayes factors of homoskedastic to
#' heteroskedastic posterior odds \code{"log(SDDR)"} for each structural shock, 
#' their numerical standard errors \code{"NSE"}, and the implied posterior 
#' probability of the homoskedasticity and heteroskedasticity hypothesis, 
#' \code{"Pr[homoskedasticity|data]"} and \code{"Pr[heteroskedasticity|data]"} 
#' respectively.
#' 
#' @references 
#' Lütkepohl, H., Shang, F., Uzeda, L., and Woźniak, T. (2024) Partial Identification of Heteroskedastic Structural VARs: Theory and Bayesian Inference. \emph{University of Melbourne Working Paper}, 1--57, \doi{10.48550/arXiv.2404.11057}.
#' 
#' @method summary SDDRidSV
#' 
#' @seealso \code{\link{verify_identification.PosteriorBSVARSV}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' summary(sddr)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new() |>
#'   estimate(S = 10) |> 
#'   verify_identification() |> 
#'   summary() -> sddr_summary
#' 
#' @export
summary.SDDRidSV = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Summary of identification verification          |\n",
    "    H0: omega_n = 0  [homoskedasticity]           |\n",
    "    H1: omega_n != 0 [heteroskedasticity]         |\n",
    "**************************************************|\n"
  )
  
  N         = nrow(object$logSDDR)
  exp_sddr  = exp(object$logSDDR)
  
  out = cbind(
    object$logSDDR,
    object$logSDDR_se,
    exp_sddr / (1 + exp_sddr),
    1 / (1 + exp_sddr)
  )
  colnames(out) = c("log(SDDR)", "NSE", "Pr[H0|data]", "Pr[H1|data]")
  rownames(out) = paste0("shock ", 1:N)
  
  return(out)
} # END summary.SDDRidSV




#' @title Provides summary of verifying homoskedasticity
#'
#' @description Provides summary of the Savage-Dickey density ratios
#' for verification of structural shocks homoskedasticity. The outcomes can be
#' used to make probabilistic statements about identification through 
#' heteroskedasticity closely following ideas by Lütkepohl& Woźniak (2020).
#' 
#' @param object an object of class \code{SDDRidMSH} obtained using the
#' \code{\link{verify_identification.PosteriorBSVARMSH}} function. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A table reporting the logarithm of Bayes factors of homoskedastic to
#' heteroskedastic posterior odds \code{"log(SDDR)"} for each structural shock, 
#' their numerical standard errors \code{"NSE"}, and the implied posterior 
#' probability of the homoskedasticity and heteroskedasticity hypothesis, 
#' \code{"Pr[homoskedasticity|data]"} and \code{"Pr[heteroskedasticity|data]"} 
#' respectively.
#' 
#' @references 
#' Lütkepohl, H., and Woźniak, T., (2020) Bayesian Inference for Structural Vector Autoregressions Identified by Markov-Switching Heteroskedasticity. \emph{Journal of Economic Dynamics and Control} \bold{113}, 103862, \doi{10.1016/j.jedc.2020.103862}.
#' 
#' @method summary SDDRidMSH
#' 
#' @seealso \code{\link{verify_identification.PosteriorBSVARMSH}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' summary(sddr)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_identification() |> 
#'   summary() -> sddr_summary
#' 
#' @export
summary.SDDRidMSH = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Summary of identification verification          |\n",
    "  H0: s^2_nm  = 1 for all m  [homoskedasticity]   |\n",
    "  H1: s^2_nm != 1 for some m [heteroskedasticity] |\n",
    "**************************************************|\n"
  )
  
  N         = nrow(object$logSDDR)
  exp_sddr  = exp(object$logSDDR)
  
  out = cbind(
    object$logSDDR,
    object$logSDDR_se,
    exp_sddr / (1 + exp_sddr),
    1 / (1 + exp_sddr)
  )
  colnames(out) = c("log(SDDR)", "NSE", "Pr[H0|data]", "Pr[H1|data]")
  rownames(out) = paste0("shock ", 1:N)
  
  return(out)
} # END summary.SDDRidMSH



#' @title Provides summary of verifying shocks' normality
#'
#' @description Provides summary of the Savage-Dickey density ratios
#' for verification of structural shocks normality. The outcomes can be
#' used to make probabilistic statements about identification through 
#' non-normality.
#' 
#' @param object an object of class \code{SDDRidMIX} obtained using the
#' \code{\link{verify_identification.PosteriorBSVARMIX}} function. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A table reporting the logarithm of Bayes factors of normal to
#' non-normal shocks posterior odds \code{"log(SDDR)"} for each structural shock, 
#' their numerical standard errors \code{"NSE"}, and the implied posterior 
#' probability of the normality and non-normality hypothesis, 
#' \code{"Pr[normal|data]"} and \code{"Pr[non-normal|data]"} 
#' respectively.
#' 
#' @method summary SDDRidMIX
#' 
#' @seealso \code{\link{verify_identification.PosteriorBSVARMIX}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, M = 2)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' summary(sddr)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(M = 2) |>
#'   estimate(S = 10) |> 
#'   verify_identification() |> 
#'   summary() -> sddr_summary
#' 
#' @export
summary.SDDRidMIX = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Summary of identification verification          |\n",
    "  H0: s^2_nm  = 1 for all m  [normal]             |\n",
    "  H1: s^2_nm != 1 for some m [non-normal]         |\n",
    "**************************************************|\n"
  )
  
  N         = nrow(object$logSDDR)
  exp_sddr  = exp(object$logSDDR)
  
  out = cbind(
    object$logSDDR,
    object$logSDDR_se,
    exp_sddr / (1 + exp_sddr),
    1 / (1 + exp_sddr)
  )
  colnames(out) = c("log(SDDR)", "NSE", "Pr[H0|data]", "Pr[H1|data]")
  rownames(out) = paste0("shock ", 1:N)
  
  return(out)
} # END summary.SDDRidMIX



#' @title Provides summary of verifying shocks' normality
#'
#' @description Provides summary of the Savage-Dickey density ratios
#' for verification of structural shocks normality. The outcomes can be
#' used to make probabilistic statements about identification through 
#' non-normality.
#' 
#' @param object an object of class \code{SDDRidT} obtained using the
#' \code{\link{verify_identification.PosteriorBSVART}} function. 
#' @param ... additional arguments affecting the summary produced.
#' 
#' @return A table reporting the Bayes factor of normal to
#' Student-t shocks posterior odds \code{"SDDR"} as well as its logarithm 
#' \code{"log(SDDR)"}for each structural shock, and the implied posterior 
#' probability of the normality and Student-t hypothesis, 
#' \code{"Pr[normal|data]"} and \code{"Pr[Student-t|data]"} 
#' respectively.
#' 
#' @method summary SDDRidT
#' 
#' @seealso \code{\link{verify_identification.PosteriorBSVART}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw)
#' set.seed(123)
#' 
#' # estimate the model
#' posterior      = estimate(specification, 10)
#' 
#' # verify heteroskedasticity
#' sddr           = verify_identification(posterior)
#' summary(sddr)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_t$new() |>
#'   estimate(S = 10) |> 
#'   verify_identification() |> 
#'   summary() -> sddr_summary
#' 
#' @export
summary.SDDRidT = function(
    object,
    ...
) {
  
  cat(
    " **************************************************|\n",
    "bsvars: Bayesian Structural Vector Autoregressions|\n",
    "**************************************************|\n",
    "  Summary of identification verification          |\n",
    "  H0: df = Inf    [normal shocks]                 |\n",
    "  H1: df != Inf   [Student-t shocks]              |\n",
    "**************************************************|\n"
  )
  
  exp_sddr  = object$SDDR
  
  out = cbind(
    object$logSDDR,
    object$SDDR,
    exp_sddr / (1 + exp_sddr),
    1 / (1 + exp_sddr)
  )
  colnames(out) = c("log(SDDR)", "SDDR", "Pr[H0|data]", "Pr[H1|data]")
  rownames(out) = ""
  
  return(out)
} # END summary.SDDRidT
