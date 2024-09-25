
#' @title Computes posterior draws of regime probabilities
#'
#' @description Each of the draws from the posterior estimation of a model is transformed into
#' a draw from the posterior distribution of the regime probabilities. These represent either
#' the realisations of the regime indicators, when \code{type = "realized"}, filtered probabilities,
#' when \code{type = "filtered"}, forecasted regime probabilities, when \code{type = "forecasted"},
#' or the smoothed probabilities, when \code{type = "smoothed"}, .
#' 
#' @param posterior posterior estimation outcome of regime-dependent heteroskedastic models 
#' - an object of either of the classes: PosteriorBSVARMSH, or PosteriorBSVARMIX
#' obtained by running the \code{estimate} function.
#' @param type one of the values \code{"realized"}, \code{"filtered"}, \code{"forecasted"}, or \code{"smoothed"}
#' denoting the type of probabilities to be computed.
#' 
#' @return An object of class PosteriorRegimePr, that is, an \code{MxTxS} array with attribute PosteriorRegimePr 
#' containing \code{S} draws of the regime probabilities.
#'
#' @seealso \code{\link{estimate}}, \code{\link{summary}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Song, Y., and Woźniak, T., (2021) Markov Switching. \emph{Oxford Research Encyclopedia of Economics and Finance}, Oxford University Press, \doi{10.1093/acrefore/9780190625979.013.174}.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 2, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute the posterior draws of realized regime indicators
#' regimes        = compute_regime_probabilities(posterior)
#' 
#' # compute the posterior draws of filtered probabilities
#' filtered       = compute_regime_probabilities(posterior, "filtered")
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) -> posterior
#' regimes        = compute_regime_probabilities(posterior)
#' filtered       = compute_regime_probabilities(posterior, "filtered")
#' 
#' @export
compute_regime_probabilities <- function(posterior, type = c("realized", "filtered", "forecasted", "smoothed")) {
  UseMethod("compute_regime_probabilities", posterior)
}




#' @inherit compute_regime_probabilities
#' @method compute_regime_probabilities PosteriorBSVARMSH
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMSH} obtained by running the \code{estimate} function.
#' 
#' @export
compute_regime_probabilities.PosteriorBSVARMSH <- function(posterior, type = c("realized", "filtered", "forecasted", "smoothed")) {
  
  type          = match.arg(type)
  
  posteriors    = posterior$posterior
  Y             = posterior$last_draw$data_matrices$Y
  X             = posterior$last_draw$data_matrices$X
  
  if (type == "realized") {
    probs       = posteriors$xi
  } else {
    if (type == "filtered") {
      forecasted  = FALSE
      smoothed    = FALSE
    } else if (type == "forecasted") {
      forecasted  = TRUE
      smoothed    = FALSE
    } else if (type == "smoothed") {
      forecasted  = FALSE
      smoothed    = TRUE
    }
    
    probs       = .Call(`_bsvars_bsvars_filter_forecast_smooth`, posteriors, Y, X, forecasted, smoothed)
  }
  
  class(probs)  = "PosteriorRegimePr"
  M                 = dim(posterior$posterior$xi)[1]
  S                 = dim(posterior$posterior$xi)[3]
  dimnames(probs)   = list(1:M, colnames(Y), 1:S)
  
  return(probs)
}









#' @inherit compute_regime_probabilities
#' @method compute_regime_probabilities PosteriorBSVARMIX
#' @param posterior posterior estimation outcome - an object of class 
#' \code{PosteriorBSVARMIX} obtained by running the \code{estimate} function.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_mix$new(us_fiscal_lsuw, p = 2, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#' 
#' # compute the posterior draws of realized regime indicators
#' regimes        = compute_regime_probabilities(posterior)
#' 
#' # compute the posterior draws of filtered probabilities
#' filtered       = compute_regime_probabilities(posterior, "filtered")
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_mix$new(p = 1, M = 2) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20) -> posterior
#' regimes        = compute_regime_probabilities(posterior)
#' filtered       = compute_regime_probabilities(posterior, "filtered")
#' 
#' @export
compute_regime_probabilities.PosteriorBSVARMIX <- function(posterior, type = c("realized", "filtered", "forecasted", "smoothed")) {
  
  type          = match.arg(type)
  
  posteriors    = posterior$posterior
  Y             = posterior$last_draw$data_matrices$Y
  X             = posterior$last_draw$data_matrices$X
  
  if (type == "realized") {
    probs       = posteriors$xi
  } else {
    if (type == "filtered") {
      forecasted  = FALSE
      smoothed    = FALSE
    } else if (type == "forecasted") {
      forecasted  = TRUE
      smoothed    = FALSE
    } else if (type == "smoothed") {
      forecasted  = FALSE
      smoothed    = TRUE
    }
    
    probs       = .Call(`_bsvars_bsvars_filter_forecast_smooth`, posteriors, Y, X, forecasted, smoothed)
  }
  
  class(probs)  = "PosteriorRegimePr"
  M                 = dim(posterior$posterior$xi)[1]
  S                 = dim(posterior$posterior$xi)[3]
  dimnames(probs)   = list(1:M, colnames(Y), 1:S)
  
  return(probs)
}
