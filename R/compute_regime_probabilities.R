
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
#' obtained by running one of the \code{estimate_bsvar_*} functions.
#' @param type one of the values \code{"realized"}, \code{"filtered"}, \code{"forecasted"}, or \code{"smoothed"}
#' denoting the type of probabilities to be computed.
#' 
#' @return An object of class PosteriorRegimePr, that is, an \code{MxTxS} array with attribute PosteriorRegimePr 
#' containing \code{S} draws of the regime probabilities.
#'
#' @seealso \code{\link{estimate_bsvar_msh}}, \code{\link{estimate_bsvar_mix}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @references 
#' Song, Y., and Woźniak, T., (2021) Markov Switching. \emph{Oxford Research Encyclopedia of Economics and Finance}, Oxford University Press, \doi{https://doi.org/10.1093/acrefore/9780190625979.013.174}.
#' 
#' @examples
#' # upload data
#' data(us_fiscal_lsuw)
#' 
#' # specify the model and set seed
#' set.seed(123)
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw, p = 4, M = 2)
#' 
#' # run the burn-in
#' burn_in        = estimate_bsvar_msh(specification, 10)
#' 
#' # estimate the model
#' posterior      = estimate_bsvar_msh(burn_in$get_last_draw(), 50)
#' 
#' # compute the posterior draws of realized regime indicators
#' regimes        = compute_regime_probabilities(posterior)
#' 
#' # compute the posterior draws of filtered probabilities
#' filtered       = compute_regime_probabilities(posterior, "filtered")
#' 
#' @export
compute_regime_probabilities <- function(posterior, type = c("realized", "filtered", "forecasted", "smoothed")) {
  
  stopifnot("Argument posterior must contain estimation output from one of the estimate_bsvar* functions for regime change or mixture models." = any(class(posterior)[1] == c("PosteriorBSVARMSH", "PosteriorBSVARMIX")))
  
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
  
  return(probs)
}

