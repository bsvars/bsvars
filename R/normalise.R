

#' @title Waggoner & Zha (2003) row signs normalisation of the posterior draws 
#' for the structural matrix \eqn{B}
#'
#' @description Normalises the sign of rows of matrix \eqn{B} MCMC draws, 
#'  relative to matrix \code{B_benchmark}, provided as the second argument. The implemented
#'  procedure proposed by Waggoner, Zha (2003) normalises the MCMC output in an
#'  optimal way leading to the unimodal posterior. Only normalised MCMC output is 
#'  suitable for the computations of the posterior characteristics of the \eqn{B}
#'  matrix elements and their functions such as the impulse response functions and other 
#'  economically interpretable values. 
#' 
#' @param posterior posterior estimation outcome obtained using function 
#' \code{estimate()} containing, amongst other draws, the \code{S} draws from 
#' the posterior distribution of the \code{NxN} structural matrix of 
#' contemporaneous relationships \eqn{B}. These draws are to be normalised with 
#' respect to the matrix \code{B_benchmark}.
#' @param B_benchmark the benchmark \code{NxN} structural matrix specified by 
#' the user to have the desired row signs
#' 
#' @return An object of the same class as that provided as the input argument 
#' \code{posterior} containing the posterior draws including the draws of the 
#' normalised structural matrix.
#' 
#' @seealso \code{\link{estimate}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Waggoner, D.F., and Zha, T., (2003) Likelihood Preserving Normalization in Multiple Equation Models. 
#' \emph{Journal of Econometrics}, \bold{114}(2), 329--47, \doi{10.1016/S0304-4076(03)00087-3}.
#'
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)        # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise <- function(posterior, B_benchmark = NULL) {
  
  if (!is.null(B_benchmark)) {
    stopifnot(
      "The argument B_benchmark must be a square numeric matrix without missing values." =
      is.numeric(B_benchmark) & all(!is.na(B_benchmark)) & nrow(B_benchmark) == ncol(B_benchmark)
    )
  }
  
  UseMethod("normalise", posterior)
}




#' @inherit normalise
#' @method normalise PosteriorBSVAR
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVAR} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @export
normalise.PosteriorBSVAR <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}



#' @inherit normalise
#' @method normalise PosteriorBSVAREXH
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVAREXH} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @examples
#' specification  = specify_bsvar_exh$new(us_fiscal_lsuw)    # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise.PosteriorBSVAREXH <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}


#' @inherit normalise
#' @method normalise PosteriorBSVARHMSH
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVARHMSH} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @examples
#' specification  = specify_bsvar_hmsh$new(us_fiscal_lsuw)    # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise.PosteriorBSVARHMSH <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}


#' @inherit normalise
#' @method normalise PosteriorBSVARMIX
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVARMIX} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @examples
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw)    # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise.PosteriorBSVARMIX <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}


#' @inherit normalise
#' @method normalise PosteriorBSVARMSH
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVARMSH} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @examples
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw)    # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise.PosteriorBSVARMSH <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}



#' @inherit normalise
#' @method normalise PosteriorBSVARSV
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVARSV} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @examples
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw)     # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise.PosteriorBSVARSV <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}


#' @inherit normalise
#' @method normalise PosteriorBSVART
#' @param posterior posterior estimation outcome of class \code{PosteriorBSVART} 
#' generated using the \code{estimate()} function, amongst other draws, 
#' the \code{S} draws from the posterior distribution of the \code{NxN} 
#' structural matrix of contemporaneous relationships \eqn{B}. These draws are 
#' to be normalised with respect to the matrix \code{B_benchmark}.
#' 
#' @examples
#' specification  = specify_bsvar_t$new(us_fiscal_lsuw)     # specify the model
#' burn_in        = estimate(specification, 5)               # run the burn-in
#' posterior      = estimate(burn_in, 5)                     # estimate the model
#' 
#' # normalise the posterior
#' BB             = posterior$last_draw$starting_values$B    # get the last draw of B
#' B_benchmark          = diag((-1) * sign(diag(BB))) %*% BB       # set negative diagonal elements
#' posterior      = normalise(posterior, B_benchmark)              # draws in posterior are normalised
#' 
#' @export
normalise.PosteriorBSVART <- function(posterior, B_benchmark = NULL) {
  
  if (is.null(B_benchmark)) {
    B_benchmark         = posterior$last_draw$starting_values$B
    B_benchmark         = diag(sign(diag(B_benchmark))) %*% B_benchmark
  }
  
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  
  posterior_B     = .Call(`_bsvars_normalisation_wz2003`, posterior_B, B_benchmark)
  last_draw_B     = .Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_benchmark)
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
  
  return(posterior)
}
