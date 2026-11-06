.ggplot_summary = function(draws, probability) {
  stopifnot(is.numeric(probability), length(probability) == 1,
            probability > 0, probability < 1)
  margins = seq_len(length(dim(draws)) - 1)
  data.frame(
    median = as.vector(apply(draws, margins, stats::median)),
    lower = as.vector(apply(draws, margins, stats::quantile,
                            probs = (1 - probability) / 2)),
    upper = as.vector(apply(draws, margins, stats::quantile,
                            probs = 1 - (1 - probability) / 2)))
}

.ggplot_ribbon = function(data, col, main, xlab, ..., reference = 0) {
  p = ggplot2::ggplot(data, ggplot2::aes(x = position, y = median)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                          fill = col, alpha = 100 / 255, na.rm = TRUE) +
    ggplot2::geom_line(colour = col, ...) +
    ggplot2::labs(title = main, x = xlab, y = NULL)
  if (!is.null(reference)) {
    # Keep out-of-range references from expanding a panel's y scale.
    panels = intersect(c("variable", "shock", "series"), names(data))
    lines = lapply(split(data, data[panels]), function(panel) {
      limits = range(panel$median, panel$lower, panel$upper, na.rm = TRUE)
      if (reference >= limits[1] && reference <= limits[2]) panel[1, panels, drop = FALSE]
    })
    lines = do.call(rbind, lines)
    if (!is.null(lines)) {
      lines$reference = reference
      p = p + ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = reference))
    }
  }
  p
}

.ggplot_series = function(object, probability, labels, col, main, xlab,
                            ..., reference = 0) {
  N = dim(object)[1]
  data = expand.grid(series = seq_len(N), position = .ggplot_time(object, 2))
  data = cbind(data, .ggplot_summary(object, probability))
  .ggplot_ribbon(data, col, main, xlab, ..., reference = reference) +
    ggplot2::facet_wrap(~ series, ncol = 1, scales = "free_y", strip.position = "left",
      labeller = ggplot2::as_labeller(stats::setNames(labels, seq_len(N))))
}

.ggplot_decomposition = function(object, shock_names, cols, main, xlab,
                                    ..., horizon = FALSE) {
  N = dim(object)[1]
  M = dim(object)[2]
  K = dim(object)[3]
  position = if (horizon) seq_len(K) - 1 else
    .ggplot_time(object, 3)
  data = expand.grid(variable = seq_len(N), shock = seq_len(M),
                     position = position)
  data$shock = factor(data$shock)
  data$value = as.vector(apply(object, 1:3, mean))
  # Keep each shock's time series together so stat_align inserts zero crossings.
  if (!horizon) data = data[order(data$variable, data$shock, data$position), ]
  stack = ggplot2::position_stack(reverse = TRUE)
  layer = ggplot2::geom_area(stat = if (horizon) "identity" else "align",
                             position = stack, ...)
  ggplot2::ggplot(data, ggplot2::aes(x = position, y = value, fill = shock)) +
    layer +
    ggplot2::scale_fill_manual(values = unname(cols), labels = shock_names) +
    ggplot2::labs(title = main, x = xlab, y = if (horizon) "percent" else NULL,
                   fill = "shock") +
    ggplot2::facet_wrap(~ variable, ncol = 1, scales = "free_y", strip.position = "left",
      labeller = ggplot2::as_labeller(.ggplot_labels(object, 1, "variable")))
}

.ggplot_labels = function(object, margin, prefix) {
  n = dim(object)[margin]
  labels = dimnames(object)[[margin]]
  if (is.null(labels)) labels = paste(prefix, seq_len(n))
  stats::setNames(labels, seq_len(n))
}

.ggplot_time = function(object, margin) {
  labels = dimnames(object)[[margin]]
  if (is.null(labels)) seq_len(dim(object)[margin]) else as.numeric(labels)
}

.ggplot_colours = function(n) {
  grDevices::colorRampPalette(c("#ff69b4", "#ffd700"))(n)
}

utils::globalVariables(c("position", "median", "lower", "upper", "shock", "value", "reference"))

.ggplot_draw = function(p) {
  p = p + ggplot2::theme_classic() +
    ggplot2::theme(strip.background = ggplot2::element_rect(colour = NA),
                   strip.placement = "outside",
                   plot.title = ggplot2::element_text(hjust = 0.5))
  print(p)
  invisible(p)
}

