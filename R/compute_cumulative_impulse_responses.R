#' @title Computes posterior draws of cumulative impulse responses
#'
#' @description Each of the draws from the posterior estimation of the model
#' is transformed into a draw from the posterior distribution of the cumulative
#' impulse responses, that is, the impulse responses summed over horizons from
#' 0 to \code{h}. For variables in first differences, these are the impulse
#' responses of their levels.
#'
#' @inheritParams compute_impulse_responses
#'
#' @return An object of class PosteriorIR, that is, an \code{NxNx(horizon+1)xS} array
#' containing \code{S} draws of the cumulative impulse responses.
#'
#' @seealso \code{\link{compute_impulse_responses}}, \code{\link{estimate}}, \code{\link{normalise}}, \code{\link{summary}}
#'
#' @references
#' Kilian, L., & Lütkepohl, H. (2017). Structural VAR Tools, Chapter 4, In: Structural vector autoregressive analysis. Cambridge University Press.
#'
#' @examples
#' # specify the model for first differences
#' specification  = specify_bsvar$new(diff(us_fiscal_lsuw), p = 1)
#'
#' # run the burn-in
#' burn_in        = estimate(specification, 10)
#'
#' # estimate the model
#' posterior      = estimate(burn_in, 20)
#'
#' # compute cumulative impulse responses 2 years ahead
#' cirf           = compute_cumulative_impulse_responses(posterior, horizon = 8)
#' plot(cirf, main = "Cumulative Impulse Responses")
#'
#' # workflow with the pipe |>
#' ############################################################
#' diff(us_fiscal_lsuw) |>
#'   specify_bsvar$new(p = 1) |>
#'   estimate(S = 10) |>
#'   estimate(S = 20) |>
#'   compute_cumulative_impulse_responses(horizon = 8) -> cirf
#'
#' @export
compute_cumulative_impulse_responses <- function(posterior, horizon, standardise = FALSE) {

  irfs            = compute_impulse_responses(posterior, horizon, standardise)
  for (h in 1:horizon) irfs[,,h + 1,] = irfs[,,h,] + irfs[,,h + 1,]

  attr(irfs, "cumulative") = TRUE

  return(irfs)
}
