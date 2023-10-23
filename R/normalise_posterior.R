

#' @title Waggoner & Zha (2003) row signs normalisation of the posterior draws for matrix \eqn{B}
#'
#' @description Normalises the sign of rows of matrix \eqn{B} MCMC draws, 
#'  provided as the first argument \code{posterior_B}, relative to matrix
#'  \code{B_hat}, provided as the second argument of the function. The implemented
#'  procedure proposed by Waggoner, Zha (2003) normalises the MCMC output in an
#'  optimal way leading to the unimodal posterior. Only normalised MCMC output is 
#'  suitable for the computations of the posterior characteristics of the \eqn{B}
#'  matrix elements and their functions such as the impulse response functions and other 
#'  economically interpretable values. 
#' 
#' @param posterior posterior estimation outcome - an object of either of classes: 
#' PosteriorBSVAR, PosteriorBSVARMSH, PosteriorBSVARMIX, or PosteriorBSVARSV
#' containing, amongst other draws, the \code{S} draws from the posterior 
#' distribution of the \code{NxN} matrix of contemporaneous relationships \eqn{B}. 
#' These draws are to be normalised with respect to:
#' @param B_hat an \code{NxN} matrix specified by the user to have the desired row signs
#' 
#' @return Nothing. The normalised elements overwrite the corresponding elements of 
#' the first argument \code{posterior_B} by reference.
#' 
#' @seealso \code{\link{estimate}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Waggoner, D.F., and Zha, T., (2003) Likelihood Preserving Normalization in Multiple Equation Models. 
#' \emph{Journal of Econometrics}, \bold{114}(2), 329--47, \doi{https://doi.org/10.1016/S0304-4076(03)00087-3}.
#'
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw, p = 4)
#' set.seed(123)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 10, thin = 1)
#' 
#' # normalise the posterior
#' BB            = posterior$last_draw$starting_values$B      # get the last draw of B
#' B_hat         = diag((-1) * sign(diag(BB))) %*% BB         # set negative diagonal elements
#' normalise_posterior(posterior, B_hat)                      # draws in posterior are normalised
#' 
#' @export
normalise_posterior <- function(posterior, B_hat) {
  
  stopifnot("Argument posterior must contain estimation output from the estimate function." = any(class(posterior)[1] == c("PosteriorBSVAR", "PosteriorBSVARMSH", "PosteriorBSVARMIX", "PosteriorBSVARSV")))
  posterior_B     = posterior$posterior$B
  N               = dim(posterior_B)[1]
  last_draw_B     = array(NA, c(N, N, 1))
  last_draw_B[,,1] = posterior$last_draw$starting_values$B
  stopifnot("Argument B_hat must be a numeric matrix of dimensions NxN." = all(dim(posterior_B)[1:2] ==  dim(B_hat)) & is.numeric(B_hat))
  
  invisible(.Call(`_bsvars_normalisation_wz2003`, posterior_B, B_hat))
  invisible(.Call(`_bsvars_normalisation_wz2003`, last_draw_B, B_hat))
  
  posterior$posterior$B                 = posterior_B
  posterior$last_draw$starting_values$B = last_draw_B[,,1]
  posterior$set_normalised()
}
