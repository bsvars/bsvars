
#' @title Plots the median and an interval between two specified percentiles 
#' for a sequence of \code{K} random variables
#'
#' @description Plots the median and an interval between two specified percentiles 
#' for a sequence of \code{K} random variables based on the \code{S} posterior 
#' draws provided for each of them.
#' 
#' @param draws a \code{K x S} matrix with \code{S} posterior draws of 
#' \code{K} random variables, or a \code{K x S x N} array with \code{N} such matrices
#' @param probability a number from interval \code{(0,1)} denoting the probability 
#' content of the plotted interval. The interval stretches from the 
#' \code{0.5 * (1 - probability)} to \code{1 - 0.5 * (1 - probability)} percentile 
#' of the posterior distribution.
#' @param col a colour of the plot
#' @param ylim the range of the \code{y} axis
#' @param ylab the label of the \code{y} axis
#' @param xlab the label of the \code{x} axis
#' @param start_at an integer to denote the beginning of the \code{x} axis range
#' @param add a logical value. If \code{TRUE} the current ribbon plot is added 
#' to an existing plot
#' @param ... other graphical parameters to be passed to \code{base::plot}
#' 
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                               # upload data
#' set.seed(123)                                                      # set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw)                 # specify model
#' 
#' burn_in        = estimate(specification, 10)                       # run the burn-in
#' posterior      = estimate(burn_in, 30, thin = 1)                   # estimate the model
#' irf            = compute_impulse_responses(posterior, horizon = 4) # impulse responses
#' plot_ribbon(irf[1,1,,])
#' 
#' @export
plot_ribbon = function(
    draws,              # K x S x N
    probability = 0.9,
    col         = "#ff69b4",
    ylim,
    ylab,
    xlab,
    start_at    = 0,
    add         = FALSE,
    ...
) {
  
  stopifnot("Argument draws must be a matrix or an array." = any(class(draws) == "matrix") || any(class(draws) == "array"))
  stopifnot("Argument probability must be a number from interval (0,1)." = is.numeric(probability) & length(probability) == 1 & probability > 0 & probability < 1)
  stopifnot("Argument start_at must be a matrix or an integer number." = start_at %% 1 == 0)
  stopifnot("Argument add must be a logical value." = is.logical(add) & length(add) == 1)
  
  mat_or_array_tmp  = length(dim(draws))
  
  K               = dim(draws)[1]
  S               = dim(draws)[2]
  if ( mat_or_array_tmp == 2 ) {
    N               = 1
    draws_tmp       = array(NA, c(K, S, 1)) 
    draws_tmp[,,1]  = draws
    draws           = draws_tmp
  } else if ( mat_or_array_tmp == 3 ) {
    N               = dim(draws)[3]
  }
  # compute characteristics of draws
  draws_median    = apply(draws, c(1,3), stats::median) # K x N
  draws_lb        = apply(draws, c(1,3), stats::quantile, probs = 0.5 * (1 - probability)) # K x N  
  draws_ub        = apply(draws, c(1,3), stats::quantile, probs = 1 - 0.5 * (1 - probability)) # K x N
  draws_range     = range(draws_lb, draws_ub)
  
  # create col_ribbon
  col_ribbon_rgb  = grDevices::col2rgb(col)
  col_ribbon      = grDevices::rgb(col_ribbon_rgb[1], col_ribbon_rgb[2], col_ribbon_rgb[3], 100, maxColorValue = 255)
  
  # manage the arguments
  if ( missing(ylim) ) ylim = draws_range
  if ( missing(ylab) ) ylab = ""
  if ( missing(xlab) ) xlab = ""
  
  if ( !add ) {
    base::plot(
      x      = start_at:(K - 1 + start_at), 
      y      = draws_median[,1],
      type   = "n",
      ylim   = ylim,
      ylab   = ylab,
      xlab   = xlab,
      ...
    )
  }
  for (n in 1:N) {
    graphics::polygon(
      x      = c(start_at:(K - 1 + start_at), (K - 1 + start_at):start_at),
      y      = c(draws_lb[1:K, n], draws_ub[K:1, n]),
      col    = col_ribbon,
      border = col_ribbon
    )
    graphics::lines(
      x      = start_at:(K - 1 + start_at), 
      y      = draws_median[,n],
      type   = "l",
      lwd    = 2,
      col    = col
    )
  }
}







#' @title Plots structural shocks' conditional standard deviations
#'
#' @description Plots of structural shocks' conditional standard deviations 
#' including their median and percentiles.
#' 
#' @param x an object of class PosteriorSigma obtained using the
#' \code{compute_conditional_sd()} function containing posterior draws of 
#' conditional standard deviations of structural shocks.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorSigma
#' 
#' @seealso \code{\link{compute_conditional_sd}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw) # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute structural shocks' conditional standard deviations
#' sigma          = compute_conditional_sd(posterior)
#' plot(sigma)                                            # plot conditional sds
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_conditional_sd() |>
#'   plot()
#' 
#' @export
plot.PosteriorSigma = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    ...
) {

  if ( missing(main) ) main = "Strictural shocks' conditional standard deviations"
  
  N = dim(x)[1]
  
  oldpar <- graphics::par( mfrow = c(N,1) )
  
  for (n in 1:N) {
    
    if (n > 1) main = ""
    
    plot_ribbon(
      x[n,,],
      probability = probability,
      col         = col,
      main = main,
      ylab = "sd",
      xlab = "time",
      start_at    = 1,
      ...
    )
    graphics::abline(h = 1)
  } # END n loop
  
  graphics::par(oldpar)
  invisible(x)
} # END plot.PosteriorSigma




#' @title Plots fitted values of dependent variables
#'
#' @description Plots of fitted values of dependent variables including their 
#' median and percentiles.
#' 
#' @param x an object of class PosteriorFitted obtained using the
#' \code{compute_fitted_values()} function containing posterior draws of 
#' fitted values of dependent variables.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorFitted
#' 
#' @seealso \code{\link{compute_fitted_values}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar_sv$new(us_fiscal_lsuw) # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute fitted values
#' fitted         = compute_fitted_values(posterior)
#' plot(fitted)                                          # plot fitted values
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_sv$new(p = 1) |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_fitted_values() |>
#'   plot()
#' 
#' @export
plot.PosteriorFitted = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    ...
) {
  
  if ( missing(main) ) main = "Fitted values of dependent variables"
  
  N = dim(x)[1]
  
  oldpar <- graphics::par( mfrow = c(N,1) )
  
  for (n in 1:N) {
    
    if (n > 1) main = ""
    
    plot_ribbon(
      x[n,,],
      probability = probability,
      col         = col,
      main = main,
      ylab = paste0("variable ", n),
      xlab = "time",
      start_at    = 1,
      ...
    )
    graphics::abline(h = 1)
  } # END n loop
  
  graphics::par(oldpar)
  invisible(x)
} # END plot.PosteriorFitted
