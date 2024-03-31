
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
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...
) {

  if ( missing(main) ) main = "Strictural shocks' conditional standard deviations"
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
  
  if ( missing(main) ) main = "Fitted values of dependent variables"
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
  
  if ( missing(main) ) main = "Impulse responses"
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
  
  if ( missing(main) ) main = "Regime probabilities"
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
  
  if ( missing(main) ) main = "Strictural shocks"
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
