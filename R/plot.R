
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
#' specification  = specify_bsvar$new(us_fiscal_lsuw)                 # specify model
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

.plot_summary = function(draws, probability, hpd = FALSE) {
  stopifnot("Argument probability must be a number from interval (0,1)." =
    is.numeric(probability) & length(probability) == 1 & probability > 0 & probability < 1)
  draws = matrix(draws, ncol = dim(draws)[length(dim(draws))])
  median = apply(draws, 1, stats::median)
  if (hpd && ncol(draws) == 1L) {
    # coda requires at least two draws; a single draw has no interval width.
    bounds = cbind(median, median)
  } else if (hpd) {
    bounds = coda::HPDinterval(coda::mcmc(t(draws)), prob = probability)
  } else {
    tail = (1 - probability) / 2
    bounds = t(apply(draws, 1, stats::quantile, probs = c(tail, 1 - tail), names = FALSE))
  }
  data.frame(median = median, lower = bounds[, 1], upper = bounds[, 2])
}

.plot_labels = function(x, margin, prefix) {
  labels = dimnames(x)[[margin]]
  if (is.null(labels)) labels = paste(prefix, seq_len(dim(x)[margin]))
  stats::setNames(labels, seq_along(labels))
}

.plot_time = function(x, margin) {
  time = dimnames(x)[[margin]]
  time = suppressWarnings(as.numeric(time))
  if (!length(time) || any(!is.finite(time))) seq_len(dim(x)[margin]) else time
}

.plot_ribbon_gg = function(data, col, main, xlab, ..., panels, reference = NULL) {
  p = ggplot2::ggplot(data, ggplot2::aes(x = .data$position, y = .data$median)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                         fill = col, alpha = 100 / 255, na.rm = TRUE) +
    ggplot2::geom_line(colour = col, ...) +
    ggplot2::labs(title = main, x = xlab, y = NULL)
  if (!is.null(reference)) {
    # Base abline() clipped the reference without changing panel limits.
    lines = lapply(split(data, data[panels], drop = TRUE), function(panel) {
      limits = range(panel$median, panel$lower, panel$upper, na.rm = TRUE)
      if (reference >= limits[1] && reference <= limits[2]) panel[1, panels, drop = FALSE]
    })
    lines = do.call(rbind, lines)
    if (!is.null(lines)) {
      lines$reference = reference
      p = p + ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = .data$reference))
    }
  }
  p
}

.plot_select = function(selection, n) {
  stopifnot("Select at least one valid integer position." =
    is.numeric(selection) && length(selection) > 0 && all(selection %in% seq_len(n)))
  unique(as.integer(selection))
}

.plot_series = function(x, probability, labels, position, col, main, xlab, ...,
                        reference, selection) {
  data = expand.grid(series = factor(selection, levels = selection), position = position)
  x = x[selection, , , drop = FALSE]
  data = cbind(data, .plot_summary(x, probability))
  .plot_ribbon_gg(data, col, main, xlab, ..., panels = "series", reference = reference) +
    ggplot2::facet_wrap(~ series, ncol = 1, scales = "free_y", strip.position = "left",
      labeller = ggplot2::as_labeller(labels))
}

.plot_decomposition = function(x, shock_names, cols, main, xlab, ..., fevd,
                               which_var, which_shock) {
  labels = .plot_labels(x, 1, "variable")
  x = x[which_var, which_shock, , , drop = FALSE]
  means = apply(x, 1:3, mean)
  N = dim(means)[1]
  M = dim(means)[2]
  K = dim(means)[3]
  position = if (fevd) seq_len(K) - 1 else .plot_time(x, 3)
  cumulative = if (fevd) cumsum else function(z) rev(cumsum(rev(z)))
  bands = apply(means, c(1, 3), cumulative)
  bands = aperm(array(bands, c(M, N, K)), c(2, 1, 3))
  if (fevd) {
    upper = bands
    lower = bands
    lower[, 1, ] = 0
    lower[, -1, ] = upper[, -M, , drop = FALSE]
  } else {
    lower = pmin(bands, 0)
    upper = pmax(bands, 0)
  }

  data = expand.grid(variable = factor(which_var, levels = which_var),
                     shock = factor(which_shock, levels = which_shock), position = position)
  data$lower = as.vector(lower)
  data$upper = as.vector(upper)
  p = ggplot2::ggplot(data, ggplot2::aes(x = .data$position, fill = .data$shock,
                                      ymin = .data$lower, ymax = .data$upper)) +
    ggplot2::geom_ribbon(...) +
    ggplot2::scale_fill_manual(values = unname(cols[which_shock]),
      labels = shock_names[which_shock], name = "shocks") +
    ggplot2::labs(title = main, x = xlab, y = if (fevd) "percent" else NULL) +
    ggplot2::facet_wrap(~ variable, ncol = 1, strip.position = "left",
      scales = if (fevd) "fixed" else "free_y",
      labeller = ggplot2::as_labeller(labels))
  if (fevd) {
    p = p + ggplot2::scale_y_continuous(breaks = c(0, 50, 100)) +
      ggplot2::coord_cartesian(ylim = c(0, 100))
  } else {
    p = p + ggplot2::geom_hline(yintercept = 0)
  }
  p
}

.plot_theme = function(p) {
  p + ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.placement = "outside",
                   plot.title = ggplot2::element_text(hjust = 0.5))
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
#' @param shock_names a vector of length \code{N} containing names of the structural shocks.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_shock Positive integer positions of structural shocks to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' 
#' @method plot PosteriorSigma
#' 
#' @seealso \code{\link{compute_conditional_sd}}
#' 
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
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
    shock_names,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...,
    which_shock = seq_len(dim(x)[1])
) {
  if (missing(main)) main = "Structural Shocks' Conditional Standard Deviations"
  if (missing(xlab)) xlab = "time"
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dim(x)[1]))
  labels = stats::setNames(as.character(shock_names), seq_len(dim(x)[1]))
  which_shock = .plot_select(which_shock, dim(x)[1])
  p = .plot_series(x, probability, labels, .plot_time(x, 2),
                   col, main, xlab, ..., reference = 1, selection = which_shock)
  .plot_theme(p)
}




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
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_var Positive integer positions of variables to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' 
#' @method plot PosteriorFitted
#' 
#' @seealso \code{\link{compute_fitted_values}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 5)          # run the burn-in
#' posterior      = estimate(burn_in, 5)                # estimate the model
#' 
#' # compute fitted values
#' fitted         = compute_fitted_values(posterior)
#' plot(fitted)                                          # plot fitted values
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
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
    ...,
    which_var = seq_len(dim(x)[1])
) {
  if (missing(main)) main = "Fitted Values of Dependent Variables"
  if (missing(xlab)) xlab = "time"
  which_var = .plot_select(which_var, dim(x)[1])
  p = .plot_series(x, probability, .plot_labels(x, 1, "variable"), .plot_time(x, 2),
                   col, main, xlab, ..., reference = 0, selection = which_var)
  .plot_theme(p)
}













#' @title Plots impulse responses
#'
#' @description Plots impulse responses to structural shocks with their posterior
#' medians and pointwise highest posterior density intervals.
#' 
#' @param x an object of class PosteriorIR obtained using the
#' \code{compute_impulse_responses()} function containing posterior draws of 
#' impulse responses.
#' @param probability Probability content of the pointwise highest posterior
#'   density (HPD) intervals computed from the draws with
#'   \code{coda::HPDinterval()}. The achieved probability depends on the
#'   number of draws. With one draw, both bounds equal the draw.
#' @param shock_names a vector of length \code{N} containing names of the structural shocks.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_var Positive integer positions of response variables to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @param which_shock Positive integer positions of structural shocks to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' 
#' @method plot PosteriorIR
#' 
#' @seealso \code{\link{compute_impulse_responses}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)   # specify model
#' burn_in        = estimate(specification, 5)          # run the burn-in
#' posterior      = estimate(burn_in, 5)                # estimate the model
#' 
#' # compute impulse responses``
#' fitted         = compute_impulse_responses(posterior, horizon = 4)
#' plot(fitted)                                          # plot
#' p = plot(fitted, which_var = 1, which_shock = 2)
#' p + ggplot2::labs(title = "Selected impulse response")
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_impulse_responses(horizon = 4) |>
#'   plot()
#' 
#' @export
plot.PosteriorIR = function(
    x,
    probability = 0.9,
    shock_names,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.1, 0, 1.1),
    oma.multi = c(6, 0, 5, 0),
    ...,
    which_var = seq_len(dim(x)[1]),
    which_shock = seq_len(dim(x)[2])
) {
  if (missing(main)) main = "Impulse Responses"
  if (missing(xlab)) xlab = "horizon"
  dims = dim(x)
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dims[2]))
  which_var = .plot_select(which_var, dims[1])
  which_shock = .plot_select(which_shock, dims[2])
  data = expand.grid(variable = factor(which_var, levels = which_var),
                     shock = factor(which_shock, levels = which_shock),
                     position = seq_len(dims[3]) - 1)
  draws = x[which_var, which_shock, , , drop = FALSE]
  data = cbind(data, .plot_summary(draws, probability, hpd = TRUE))
  p = .plot_ribbon_gg(data, col, main, xlab, ...,
                      panels = c("variable", "shock"), reference = 0) +
    ggplot2::facet_wrap(~ variable + shock, ncol = length(which_shock), scales = "free_y",
      labeller = ggplot2::labeller(
        variable = .plot_labels(x, 1, "variable"),
        shock = stats::setNames(as.character(shock_names), seq_len(dims[2]))))
  .plot_theme(p)
}




#' @title Plots estimated regime probabilities
#'
#' @description Plots of estimated regime probabilities of Markov-switching 
#' heteroskedasticity or allocations of normal-mixture components including their 
#' median and percentiles. Compact zero-based realized regime indices are displayed
#' using the equivalent regime indicators.
#' 
#' @param x an object of class PosteriorRegimePr obtained using the
#' \code{compute_regime_probabilities()} function containing posterior draws of 
#' compact realized regime indices or regime probabilities.
#' @param probability a parameter determining the interval to be plotted. The 
#' interval stretches from the \code{0.5 * (1 - probability)} to 
#' \code{1 - 0.5 * (1 - probability)} percentile of the posterior distribution.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}. Use ggplot2
#'   layers and scales for further customisation.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' 
#' @method plot PosteriorRegimePr
#' 
#' @seealso \code{\link{compute_regime_probabilities}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar_msh$new(us_fiscal_lsuw)# specify model
#' burn_in        = estimate(specification, 5)           # run the burn-in
#' posterior      = estimate(burn_in, 5)                 # estimate the model
#' 
#' # compute regime probabilities
#' rp             = compute_regime_probabilities(posterior)
#' plot(rp)                                              # plot
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar_msh$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
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
  if (missing(main)) main = "Regime Probabilities"
  if (missing(xlab)) xlab = "time"
  dims = dim(x)
  T = dims[2]
  N = if (length(dims) == 4) dims[3] else 1
  S = dims[length(dims)]
  compact = dims[1] == 1
  M = if (compact) max(x) + 1 else dims[1]
  draws = array(x, c(dims[1], T, N, S))
  if (compact) {
    indicators = array(0, c(M, T, N, S))
    for (m in seq_len(M)) indicators[m,,,] = draws[1,,,] == m - 1
    draws = indicators
  }
  data = expand.grid(regime = seq_len(M), position = .plot_time(x, 2),
                     process = seq_len(N))
  data = cbind(data, .plot_summary(draws, probability))
  p = .plot_ribbon_gg(data, col, main, xlab, ..., panels = c("regime", "process")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
    ggplot2::facet_grid(regime ~ process, switch = "y",
      labeller = ggplot2::labeller(
        regime = stats::setNames(paste("regime", seq_len(M)), seq_len(M)),
        process = stats::setNames(paste("process", seq_len(N)), seq_len(N))))
  .plot_theme(p)
}







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
#' @param shock_names a vector of length \code{N} containing names of the structural shocks.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_shock Positive integer positions of structural shocks to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' 
#' @method plot PosteriorShocks
#' 
#' @seealso \code{\link{compute_structural_shocks}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 5)           # run the burn-in
#' posterior      = estimate(burn_in, 5)                 # estimate the model
#' 
#' # compute structural shocks
#' shocks         = compute_structural_shocks(posterior)
#' plot(shocks)                                          # plot
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_structural_shocks() |>
#'   plot()
#' 
#' @export
plot.PosteriorShocks = function(
    x,
    probability = 0.9,
    shock_names,
    col = "#ff69b4",
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 2.1),
    oma.multi = c(6, 0, 5, 0),
    ...,
    which_shock = seq_len(dim(x)[1])
) {
  if (missing(main)) main = "Structural Shocks"
  if (missing(xlab)) xlab = "time"
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dim(x)[1]))
  labels = stats::setNames(as.character(shock_names), seq_len(dim(x)[1]))
  which_shock = .plot_select(which_shock, dim(x)[1])
  p = .plot_series(x, probability, labels, seq_len(dim(x)[2]),
                   col, main, xlab, ..., reference = 0, selection = which_shock)
  .plot_theme(p)
}







#' @title Plots forecasts with recent observations
#'
#' @description Plots recent observations with forecast medians and pointwise
#'   highest posterior density intervals.
#'
#' @param x an object of class Forecasts obtained using the
#' \code{forecast()} function containing posterior forecast draws.
#' @param probability Probability content of the pointwise highest posterior
#'   density (HPD) intervals computed from the draws with
#'   \code{coda::HPDinterval()}. The achieved probability depends on the
#'   number of draws. With one draw, both bounds equal the draw.
#' @param data_in_plot a fraction value in the range (0, 1] determining how many
#' of the last observations in the data should be plotted with the forecasts.
#' @param col a colour of the plot line and the ribbon
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_line()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_var Positive integer positions of variables to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' 
#' @method plot Forecasts
#' 
#' @seealso \code{\link{forecast}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 5)           # run the burn-in
#' posterior      = estimate(burn_in, 5)                # estimate the model
#' 
#' # compute forecasts
#' fore            = forecast(posterior, horizon = 4)
#' plot(fore)                                            # plot forecasts
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
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
    ...,
    which_var = seq_len(nrow(x$Y))
) {
  stopifnot("Argument data_in_plot must be a value within range (0,1]" =
    is.numeric(data_in_plot) && length(data_in_plot) == 1L && data_in_plot > 0 && data_in_plot <= 1)
  if (missing(main)) main = "Forecasting"
  if (missing(xlab)) xlab = "time"

  Y = x$Y
  which_var = .plot_select(which_var, nrow(Y))
  series = factor(which_var, levels = which_var)
  T = ncol(Y)
  K = max(1, floor(data_in_plot * T))
  H = dim(x$forecasts)[2]
  future = expand.grid(series = series, position = K + seq_len(H))
  draws = x$forecasts[which_var, , , drop = FALSE]
  future = cbind(future, .plot_summary(draws, probability, hpd = TRUE))
  observed = expand.grid(series = series, position = seq_len(K))
  observed$median = as.vector(Y[which_var, seq.int(T - K + 1, T)])
  observed$lower = observed$upper = NA_real_
  # Join the forecast ribbon to the last observed value.
  last = observed$position == K
  observed$lower[last] = observed$upper[last] = observed$median[last]
  data = rbind(observed, future)
  p = .plot_ribbon_gg(data, col, main, xlab, ..., panels = "series", reference = 0) +
    ggplot2::facet_wrap(~ series, ncol = 1, scales = "free_y", strip.position = "left",
      labeller = ggplot2::as_labeller(.plot_labels(Y, 1, "variable")))
  .plot_theme(p)
}








#' @title Plots forecast error variance decompositions
#'
#' @description Plots of the posterior means of the forecast error variance 
#' decompositions.
#' 
#' @param x an object of class PosteriorFEVD obtained using the
#' \code{compute_variance_decompositions()} function containing posterior draws of 
#' forecast error variance decompositions.
#' @param shock_names a vector of length \code{N} containing names of the structural shocks.
#' @param cols an \code{N}-vector with colours of the plot
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_ribbon()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_var Positive integer positions of variables to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @param which_shock Positive integer positions of structural shocks to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' @details Selected shocks retain their original variance shares on the 0--100
#'   percent scale.
#' 
#' @method plot PosteriorFEVD
#' 
#' @seealso \code{\link{compute_variance_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 5)           # run the burn-in
#' posterior      = estimate(burn_in, 5)                 # estimate the model
#' 
#' # compute forecast error variance decompositions
#' fevd           = compute_variance_decompositions(posterior, horizon = 4)
#' plot(fevd)
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_variance_decompositions(horizon = 4) |>
#'   plot()
#' 
#' @export
plot.PosteriorFEVD = function(
    x,
    shock_names,
    cols,
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 4.6),
    oma.multi = c(6, 0, 5, 0),
    ...,
    which_var = seq_len(dim(x)[1]),
    which_shock = seq_len(dim(x)[2])
) {
  if (missing(main)) main = "Forecast Error Variance Decompositions"
  if (missing(xlab)) xlab = "horizon"
  M = dim(x)[2]
  if (missing(shock_names)) shock_names = seq_len(M)
  if (missing(cols)) cols = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))(M)
  which_var = .plot_select(which_var, dim(x)[1])
  which_shock = .plot_select(which_shock, M)
  p = .plot_decomposition(x, shock_names, cols, main, xlab, ..., fevd = TRUE,
                          which_var = which_var, which_shock = which_shock)
  .plot_theme(p)
}






#' @title Plots historical decompositions
#'
#' @description Plots of the posterior means of the historical decompositions.
#' 
#' @param x an object of class PosteriorHD obtained using the
#' \code{compute_historical_decompositions()} function containing posterior draws of 
#' historical decompositions.
#' @param shock_names a vector of length \code{N} containing names of the structural shocks.
#' @param cols an \code{N}-vector with colours of the plot
#' @param main an alternative main title for the plot
#' @param xlab an alternative x-axis label for the plot
#' @param mar.multi,oma.multi Retained for compatibility with existing calls.
#'   Use \code{ggplot2::theme()} to adjust margins.
#' @param ... Additional arguments passed to \code{ggplot2::geom_ribbon()}. Use ggplot2
#'   layers and scales for further customisation.
#' @param which_var Positive integer positions of variables to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @param which_shock Positive integer positions of structural shocks to plot.
#'   All are plotted by default. Selected positions appear in the supplied order.
#' @return An editable ggplot object. The method does not draw it; use
#'   \code{print()} to draw it inside a function or loop.
#' @details Each shaded band uses the cumulative contribution of the selected
#'   shocks starting at that shock, in selection order.
#' 
#' @method plot PosteriorHD
#' 
#' @seealso \code{\link{compute_historical_decompositions}}
#'
#' @author Tomasz Woźniak \email{wozniak.tom@pm.me}
#' 
#' @examples
#' specification  = specify_bsvar$new(us_fiscal_lsuw)    # specify model
#' burn_in        = estimate(specification, 5)           # run the burn-in
#' posterior      = estimate(burn_in, 5)                 # estimate the model
#' 
#' # compute historical decompositions
#' fevd           = compute_historical_decompositions(posterior)
#' plot(fevd)                                            
#' 
#' # workflow with the pipe |>
#' ############################################################
#' us_fiscal_lsuw |>
#'   specify_bsvar$new() |>
#'   estimate(S = 5) |> 
#'   estimate(S = 5) |> 
#'   compute_historical_decompositions() |>
#'   plot()
#' 
#' @export
plot.PosteriorHD = function(
    x,
    shock_names,
    cols,
    main,
    xlab,
    mar.multi = c(1, 4.6, 0, 4.6),
    oma.multi = c(6, 0, 5, 0),
    ...,
    which_var = seq_len(dim(x)[1]),
    which_shock = seq_len(dim(x)[2])
) {
  if (missing(main)) main = "Historical Decompositions"
  if (missing(xlab)) xlab = "time"
  M = dim(x)[2]
  if (missing(shock_names)) shock_names = seq_len(M)
  if (missing(cols)) cols = grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))(M)
  which_var = .plot_select(which_var, dim(x)[1])
  which_shock = .plot_select(which_shock, M)
  p = .plot_decomposition(x, shock_names, cols, main, xlab, ..., fevd = FALSE,
                          which_var = which_var, which_shock = which_shock)
  .plot_theme(p)
}
