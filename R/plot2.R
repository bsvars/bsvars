#' Editable ggplot2 plots for bsvars results
#'
#' Draw an editable ggplot2 plot from posterior draws or forecasts. The existing
#' base \code{plot()} methods are unchanged and do not call \code{plot2()}.
#'
#' @param x A PosteriorIR, PosteriorSigma, PosteriorFitted, PosteriorShocks,
#'   PosteriorRegimePr, PosteriorFEVD, PosteriorHD, or Forecasts object.
#' @param probability Probability of the equal-tailed interval.
#' @param shock_names Names of the structural shocks.
#' @param col Colour of the median line and interval ribbon.
#' @param cols Colours of the shock contributions.
#' @param main Plot title.
#' @param xlab Horizontal axis label.
#' @param data_in_plot Fraction of historical observations shown with forecasts.
#' @param ... Passed to \code{ggplot2::geom_line()}, or to
#'   \code{ggplot2::geom_area()} for FEVD and HD.
#' @return A ggplot object, returned invisibly after drawing.
#' @details Median lines and equal-tailed intervals summarize the input draws;
#'   plots use a white classic theme and white facet-label backgrounds by default.
#'   FEVD and HD use posterior means. Each IR panel has its own y scale.
#'   HD contributions are connected linearly as areas stacked above and below zero.
#'   These layouts differ from the base plots. Use a ggplot2 theme to adjust
#'   layout; base parameters such as \code{mar.multi} and \code{oma.multi}
#'   apply only to \code{plot()}.
#' @seealso \code{\link{plot.PosteriorIR}}
#' @examples
#' draws = array(seq_len(96) / 100, c(2, 2, 3, 8))
#' class(draws) = "PosteriorIR"
#' p = plot2(draws)
#' p + ggplot2::theme_bw()
#' @export
plot2 = function(x, ...) {
  UseMethod("plot2")
}

#' @rdname plot2
#' @export
plot2.PosteriorSigma = function(
    x,
    probability = 0.9,
    shock_names,
    col = "#ff69b4",
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Structural Shocks' Conditional Standard Deviations"
  if (missing(xlab)) xlab = "time"
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dim(x)[1]))
  p = .ggplot_series(x, probability, shock_names, col, main, xlab,
                   reference = 1, ...)
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.PosteriorFitted = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Fitted Values of Dependent Variables"
  if (missing(xlab)) xlab = "time"
  p = .ggplot_series(x, probability, .ggplot_labels(x, 1, "variable"),
                   col, main, xlab, ...)
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.PosteriorIR = function(
    x,
    probability = 0.9,
    shock_names,
    col = "#ff69b4",
    main,
    xlab,
    ...
) {
  if (missing(main)) main = if (isTRUE(attr(x, "cumulative"))) "Cumulative Impulse Responses" else "Impulse Responses"
  if (missing(xlab)) xlab = "horizon"
  N = dim(x)[1]
  if (missing(shock_names)) shock_names = paste("shock", seq_len(N))
  data = expand.grid(variable = seq_len(N), shock = seq_len(N),
                     position = seq_len(dim(x)[3]) - 1)
  data = cbind(data, .ggplot_summary(x, probability))
  p = .ggplot_ribbon(data, col, main, xlab, ...) +
    ggh4x::facet_grid2(variable ~ shock, scales = "free_y", independent = "y",
      switch = "y",
      labeller = ggplot2::labeller(
        variable = .ggplot_labels(x, 1, "variable"),
        shock = stats::setNames(shock_names, seq_len(N))))
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.PosteriorRegimePr = function(
    x,
    probability = 0.9,
    col = "#ff69b4",
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Regime Probabilities"
  if (missing(xlab)) xlab = "time"
  dims = dim(x)
  T = dims[2]
  N = if (length(dims) == 4) dims[3] else 1
  S = dims[length(dims)]
  compact = identical(attr(x, "type"), "realized")
  M = if (compact) max(x) + 1 else dims[1]
  draws = array(x, c(dims[1], T, N, S))
  if (compact) {
    indicators = array(0, c(M, T, N, S))
    for (m in seq_len(M)) indicators[m,,,] = draws[1,,,] == m - 1
    draws = indicators
  }
  data = expand.grid(regime = seq_len(M), position = .ggplot_time(x, 2),
                     process = seq_len(N))
  data = cbind(data, .ggplot_summary(draws, probability))
  p = .ggplot_ribbon(data, col, main, xlab, ..., reference = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::facet_grid(regime ~ process, switch = "y", labeller = ggplot2::labeller(
      regime = stats::setNames(paste("regime", seq_len(M)), seq_len(M)),
      process = stats::setNames(paste("process", seq_len(N)), seq_len(N))))
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.PosteriorShocks = function(
    x,
    probability = 0.9,
    shock_names,
    col = "#ff69b4",
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Structural Shocks"
  if (missing(xlab)) xlab = "time"
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dim(x)[1]))
  p = .ggplot_series(x, probability, shock_names, col, main, xlab, ...)
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.Forecasts = function(
    x,
    probability = 0.9,
    data_in_plot = 1,
    col = "#ff69b4",
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Forecasting"
  if (missing(xlab)) xlab = "time"
  stopifnot("Argument data_in_plot must be a value within range (0,1]" = is.numeric(data_in_plot) & data_in_plot > 0 & data_in_plot <= 1)
  Y = x$Y
  N = nrow(Y)
  T = ncol(Y)
  K = max(1, floor(data_in_plot * T))
  H = dim(x$forecasts)[2]
  future = expand.grid(series = seq_len(N), position = K + seq_len(H))
  future = cbind(future, .ggplot_summary(x$forecasts, probability))
  observed = expand.grid(series = seq_len(N), position = seq_len(K))
  observed$median = as.vector(Y[, seq.int(T - K + 1, T)])
  observed$lower = observed$upper = NA_real_
  # Join the forecast ribbon to the final observed value, as in plot.Forecasts.
  last = observed$position == K
  observed$lower[last] = observed$upper[last] = observed$median[last]
  data = rbind(observed, future)
  p = .ggplot_ribbon(data, col, main, xlab, ...) +
    ggplot2::facet_wrap(~ series, ncol = 1, scales = "free_y", strip.position = "left",
      labeller = ggplot2::as_labeller(.ggplot_labels(Y, 1, "variable")))
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.PosteriorFEVD = function(
    x,
    shock_names,
    cols,
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Forecast Error Variance Decompositions"
  if (missing(xlab)) xlab = "horizon"
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dim(x)[2]))
  if (missing(cols)) cols = .ggplot_colours(dim(x)[2])
  p = .ggplot_decomposition(x, shock_names, cols, main, xlab, ...,
                          horizon = TRUE)
  .ggplot_draw(p)
}

#' @rdname plot2
#' @export
plot2.PosteriorHD = function(
    x,
    shock_names,
    cols,
    main,
    xlab,
    ...
) {
  if (missing(main)) main = "Historical Decompositions"
  if (missing(xlab)) xlab = "time"
  if (missing(shock_names)) shock_names = paste("shock", seq_len(dim(x)[2]))
  if (missing(cols)) cols = .ggplot_colours(dim(x)[2])
  p = .ggplot_decomposition(x, shock_names, cols, main, xlab, ...)
  .ggplot_draw(p)
}

