
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
#' posterior      = estimate(burn_in, 20, thin = 1)                   # estimate the model
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
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
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
#' burn_in        = estimate(specification, 5)           # run the burn-in
#' posterior      = estimate(burn_in, 5)                 # estimate the model
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
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_conditional_sd() |>
#'   plot()
#' 
#' @export
plot.PosteriorSigma = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {

  if ( missing(main) ) main = "Strictural Shocks' Conditional Standard Deviations"
  if ( missing(xlab) ) xlab = "time"
  
  N = dim(x)[1]
  
  oldpar <- graphics::par( 
    mfrow = c(N, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
    
    plot_ribbon(
      x[n,,],
      probability = probability,
      col         = col,
      main = "",
      ylab = paste("shock", n),
      xlab = "",
      start_at    = 1,
      bty = "n",
      axes = FALSE,
      ...
    )
    
    graphics::axis(1, labels = if (n == N) TRUE else FALSE)
    graphics::axis(2)
    
    graphics::abline(h = 1)
    
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
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
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
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
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
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
#'   specify_bsvar$new() |>
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
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  if ( missing(main) ) main = "Fitted Values of Dependent Variables"
  if ( missing(xlab) ) xlab = "time"
  
  N = dim(x)[1]
  
  oldpar <- graphics::par( 
    mfrow = c(N, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
    
    plot_ribbon(
      x[n,,],
      probability = probability,
      col         = col,
      main = "",
      ylab = paste("variable", n),
      xlab = "",
      start_at    = 1,
      bty = "n",
      axes = FALSE,
      ...
    )
    
    graphics::axis(1, labels = if (n == N) TRUE else FALSE)
    graphics::axis(2)
    
    graphics::abline(h = 0)
    
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.PosteriorFitted













#' @title Plots impulse responses
#'
#' @description Plots of of all variables to all shocks including their 
#' median and percentiles.
#' 
#' @param x an object of class PosteriorIR obtained using the
#' \code{compute_impulse_responses()} function containing posterior draws of 
#' impulse responses.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorIR
#' 
#' @seealso \code{\link{compute_impulse_responses}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute impulse responses
#' fitted         = compute_impulse_responses(posterior, horizon = 4)
#' plot(fitted)                                          # plot
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_impulse_responses(horizon = 4) |>
#'   plot()
#' 
#' @export
plot.PosteriorIR = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.1, 0, 1.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  if ( missing(main) ) main = "Impulse Responses"
  if ( missing(xlab) ) xlab = "horizon"
  
  N = dim(x)[1]
  
  oldpar <- graphics::par( 
    mfrow = c(N, N),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
    for (i in 1:N) {
      
      if (n == 1) {
        main_s = paste("shock ", i)
      } else {
        main_s = ""
      }
      
      if (i == 1) {
        ylab_v = paste("variable ", n)
      } else {
        ylab_v = ""
      }
      
      plot_ribbon(
        x[n,i,,],
        probability = probability,
        col         = col,
        main = "",
        ylab = ylab_v,
        xlab = "",
        start_at    = 0,
        bty = "n",
        axes = FALSE,
        ...
      )
      
      graphics::axis(1, labels = if (n == N) TRUE else FALSE)
      graphics::axis(2)
      
      graphics::abline(h = 0)
      
      if (n == 1) {
        graphics::mtext(
          paste("shock", i),
          side = 3,
          line = 0,
          outer = FALSE,
          cex = 0.8
        )
      }
      
    } # END i loop
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.PosteriorIR




#' @title Plots estimated regime probabilities
#'
#' @description Plots of estimated regime probabilities of Markov-switching 
#' heteroskedasticity or allocations of normal-mixture components including their 
#' median and percentiles.
#' 
#' @param x an object of class PosteriorRegimePr obtained using the
#' \code{compute_regime_probabilities()} function containing posterior draws of 
#' regime probabilities.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorRegimePr
#' 
#' @seealso \code{\link{compute_regime_probabilities}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw)# specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute regime probabilities
#' rp             = compute_regime_probabilities(posterior)
#' plot(rp)                                              # plot
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_regime_probabilities() |>
#'   plot()
#' 
#' @export
plot.PosteriorRegimePr = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  if ( missing(main) ) main = "Regime Probabilities"
  if ( missing(xlab) ) xlab = "time"
  
  M = dim(x)[1]
  
  oldpar <- graphics::par( 
    mfrow = c(M, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (m in 1:M) {
    
    plot_ribbon(
      x[m,,],
      probability = probability,
      col         = col,
      main = "",
      ylim = c(0, 1),
      ylab = paste("regime ", m),
      xlab = "",
      start_at    = 1,
      bty = "n",
      axes = FALSE,
      ...
    )
    
    graphics::axis(1, labels = if (m == M) TRUE else FALSE)
    graphics::axis(2, c(0, 1), c(0, 1))
    
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.PosteriorRegimePr







#' @title Plots structural shocks
#'
#' @description Plots of structural shocks including their median and percentiles.
#' 
#' @param x an object of class PosteriorShocks obtained using the
#' \code{compute_structural_shocks()} function containing posterior draws of 
#' structural shocks.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorShocks
#' 
#' @seealso \code{\link{compute_structural_shocks}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute structural shocks
#' shocks         = compute_structural_shocks(posterior)
#' plot(shocks)                                          # plot
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_structural_shocks() |>
#'   plot()
#' 
#' @export
plot.PosteriorShocks = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  if ( missing(main) ) main = "Structural Shocks"
  if ( missing(xlab) ) xlab = "time"
  
  N = dim(x)[1]
  
  oldpar <- graphics::par( 
    mfrow = c(N, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
  
    plot_ribbon(
      x[n,,],
      probability = probability,
      col         = col,
      main = "",
      ylab = paste("shock", n),
      xlab = "",
      start_at    = 1,
      bty = "n",
      axes = FALSE,
      ...
    )
    
    graphics::axis(1, labels = if (n == N) TRUE else FALSE)
    graphics::axis(2)
    
    graphics::abline(h = 0)
    
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.PosteriorShocks







#' @title Plots fitted values of dependent variables
#'
#' @description Plots of fitted values of dependent variables including their 
#' median and percentiles.
#' 
#' @param x an object of class Forecasts obtained using the
#' \code{forecast()} function containing posterior draws of 
#' fitted values of dependent variables.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param data_in_plot a fraction value in the range (0, 1) determining how many
#' of the last observations in the data should be plotted with the forecasts.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot Forecasts
#' 
#' @seealso \code{\link{forecast}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute forecasts
#' fore            = forecast(posterior, horizon = 4)
#' plot(fore)                                            # plot forecasts
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   forecast(horizon = 4) |>
#'   plot()
#' 
#' @export
plot.Forecasts = function(
    x,
    probability = 0.9,
    data_in_plot = 1,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  stopifnot("Argument data_in_plot must be a value within range (0,1]" = is.numeric(data_in_plot) & data_in_plot > 0 & data_in_plot <= 1)
  
  if ( missing(main) ) main = "Forecasting"
  if ( missing(xlab) ) xlab = "time"
  
  # create col_ribbon
  col_ribbon_rgb  = grDevices::col2rgb(col)
  col_ribbon      = grDevices::rgb(col_ribbon_rgb[1], col_ribbon_rgb[2], col_ribbon_rgb[3], 100, maxColorValue = 255)
  
  fore          = x$forecasts
  Y             = x$Y
  
  N             = dim(fore)[1]
  H             = dim(fore)[2]
  T             = dim(Y)[2]
  
  T_in_plot     = floor(data_in_plot * T)
  if (T_in_plot < 1) T_in_plot = 1
  obs_in_plot   = (T - T_in_plot + 1):T
  seq_in_plot   = 1:T_in_plot
  for_in_plot   = (T_in_plot + 1):(T_in_plot + H)
  
  oldpar <- graphics::par( 
    mfrow = c(N, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
    
    # compute forecasts characteristics
    fore_median   = apply(fore[n,,], 1, stats::median)
    fore_lb       = apply(fore[n,,], 1, stats::quantile, probs = 0.5 * (1 - probability)) # K x N  
    fore_ub       = apply(fore[n,,], 1, stats::quantile, probs = 1 - 0.5 * (1 - probability)) # K x N
    fore_range    = range(fore_lb, fore_ub, Y[n, obs_in_plot])
    
    base::plot(
      x      = c(seq_in_plot, for_in_plot),
      y      = c(Y[n, seq_in_plot], fore_median),
      type   = "n",
      ylim   = fore_range,
      main   = "",
      ylab   = paste("variable", n),
      xlab   = "",
      bty = "n",
      axes = FALSE,
      ...
    )

    graphics::polygon(
      x      = c(c(T_in_plot, for_in_plot), rev(c(T_in_plot, for_in_plot))),
      y      = c(c(Y[n, T], fore_lb), rev(c(Y[n, T], fore_ub))),
      col    = col_ribbon,
      border = col_ribbon
    )
    
    graphics::lines(
      x      = c(seq_in_plot, for_in_plot),
      y      = c(Y[n, obs_in_plot], fore_median),
      type   = "l",
      lwd    = 2,
      col    = col
    )
    
    graphics::axis(1, labels = if (n == N) TRUE else FALSE)
    graphics::axis(2)
    
    graphics::abline(h = 0)
    
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.Forecasts








#' @title Plots forecast error variance decompositions
#'
#' @description Plots of the posterior means of the forecast error variance 
#' decompositions.
#' 
#' @param x an object of class PosteriorFEVD obtained using the
#' \code{compute_variance_decompositions()} function containing posterior draws of 
#' forecast error variance decompositions.
#' @param cols an \code{N}-vector with colours of the plot
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorFEVD
#' 
#' @seealso \code{\link{compute_variance_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute forecast error variance decompositions
#' fevd           = compute_variance_decompositions(posterior, horizon = 4)
#' plot(fevd)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_variance_decompositions(horizon = 4) |>
#'   plot()
#' 
#' @export
plot.PosteriorFEVD = function(
    x,
    cols,
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 4.6),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  if ( missing(main) ) main = "Forecast Error Variance Decompositions"
  if ( missing(xlab) ) xlab = "horizon"
  
  N         = dim(x)[1]
  H         = dim(x)[3] - 1
  
  if ( missing(cols) ) {
    fc          = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))
    cols        = fc(N)
  }
  
  fevd      = apply(x, 1:3, mean)
  FEVD      = list()
  FEVD_mid  = list()
  for (n in 1:N) {
    FEVD[[n]] = rbind(rep(0, H + 1), apply(fevd[n,,], 2, cumsum))
    FEVD_mid[[n]] = (FEVD[[n]][1:N, H + 1] + FEVD[[n]][2:(N + 1), H + 1]) / 2
  }
  
  oldpar <- graphics::par( 
    mfrow = c(N, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
    
    graphics::plot(
      x = 0:H,
      y = FEVD[[n]][1,],
      type = "n",
      ylim = c(0, 100),
      ylab = paste("variable", n),
      main = "",
      xlab = "",
      bty = "n",
      axes = FALSE,
      ...
    )
    
    for (i in 1:N) {
      graphics::polygon(
        c(0:H, H:0), 
        c(FEVD[[n]][i,], rev(FEVD[[n]][i + 1,])), 
        col = cols[i],
        border = cols[i]
      )
    }
    
    graphics::axis(1, labels = if (n == N) TRUE else FALSE)
    graphics::axis(2, c(0, 50, 100), c(0, 50, 100))
    
    pos_right = seq(from = 20, to = 80, length.out = N)
    for (i in 1:N) {
      graphics::axis(
        4, 
        # FEVD_mid[[n]][i], 
        pos_right[i],
        i,
        col.ticks = cols[i],
        lwd.ticks = 4,
        col = "white"
      )
    }
    
    graphics::mtext( # RHS "shocks"
      "shocks",
      side = 4,
      line = 3,
      outer = FALSE,
      cex = 0.6
    )
    
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.PosteriorFEVD






#' @title Plots historical decompositions
#'
#' @description Plots of the posterior means of the historical decompositions.
#' 
#' @param x an object of class PosteriorHD obtained using the
#' \code{compute_historical_decompositions()} function containing posterior draws of 
#' historical decompositions.
#' @param cols an \code{N}-vector with colours of the plot
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi the default \code{mar} argument setting in \code{graphics::par}. Modify with care!
#' @param oma.multi the default \code{oma} argument setting in \code{graphics::par}. Modify with care!
#' @param ... additional arguments affecting the summary produced.
#' 
#' @method plot PosteriorHD
#' 
#' @seealso \code{\link{compute_historical_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' data(us_fiscal_lsuw)                                  # upload data
#' set.seed(123)                                         # set seed
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 10)          # run the burn-in
#' posterior      = estimate(burn_in, 20, thin = 1)      # estimate the model
#' 
#' # compute historical decompositions
#' fevd           = compute_historical_decompositions(posterior)
#' plot(fevd)                                            
#' 
#' # workflow with the pipe |>
#' ############################################################
#' set.seed(123)
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 10) |> 
#'   estimate(S = 20, thin = 1) |> 
#'   compute_historical_decompositions() |>
#'   plot()
#' 
#' @export
plot.PosteriorHD = function(
    x,
    cols,
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 4.6),
    oma.multi = c(6, 0, 5, 0),
    ...
) {
  
  if ( missing(main) ) main = "Historical Decompositions"
  if ( missing(xlab) ) xlab = "time"
  
  hd_mean   = apply(x, 1:3, mean)
  N         = dim(hd_mean)[1]
  T         = dim(hd_mean)[3]
  
  if ( missing(cols) ) {
    fc          = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))
    cols        = fc(N)
  }
  
  y_hat     = apply(hd_mean, c(1, 3), sum)
  ul        = list()
  for (n in 1:N) {
    ul[[n]]           = list()
    for (i in 1:N) {
      cum_mean        = apply(matrix(hd_mean[n,i:N,], ncol = T), 2, sum)
      ul[[n]][[i]]    = matrix(0, 2, T)
      which_negative  = cum_mean < 0
      ul[[n]][[i]][1, which_negative]   = cum_mean[which_negative]
      ul[[n]][[i]][2, !which_negative]  = cum_mean[!which_negative]
    }
  }
  
  oldpar <- graphics::par( 
    mfrow = c(N, 1),
    mar = mar.multi,
    oma = oma.multi
  )
  on.exit(graphics::par(oldpar))
  
  for (n in 1:N) {
    
    range_n   = range(ul[[n]])
    
    graphics::plot(
      x = 1:T,
      y = y_hat[n,1:T],
      type = "n",
      ylim = range_n,
      ylab = paste("variable", n),
      main = "",
      xlab = "",
      bty = "n",
      axes = FALSE,
      ...
    )
    for (i in 1:N) {
      graphics::polygon(
        x = c(1:T, rev(1:T)),
        y = c(ul[[n]][[i]][1,1:T], rev(ul[[n]][[i]][2,1:T])),
        col = cols[i],
        border = cols[i]
      )
    }
    
    graphics::abline(h = 0)
    
    graphics::axis(1, labels = if (n == N) TRUE else FALSE)
    graphics::axis(2, c(range_n[1], 0, range_n[2]), c(NA, 0, NA))
    
    range_m     = mean(range_n)
    range_l     = abs(range_n[2] - range_n[1]) / 2
    pos_right = seq(from = range_m - 0.6 * range_l, to = range_m + 0.6 * range_l, length.out = N)
    
    for (i in 1:N) {
      graphics::axis(
        4, 
        pos_right[i], 
        i,
        col.ticks = cols[i],
        lwd.ticks = 4,
        col = "white"
      )
    }
    
    graphics::mtext( # RHS "shocks"
      "shocks",
      side = 4,
      line = 3,
      outer = FALSE,
      cex = 0.6
    )
  } # END n loop
  
  graphics::mtext( # main title
    main,
    side = 3,
    line = 2,
    outer = TRUE
  )
  
  graphics::mtext( # x-axis label
    xlab,
    side = 1,
    line = 3,
    outer = TRUE
  )
  
  invisible(x)
} # END plot.PosteriorHD


