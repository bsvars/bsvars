grDevices::pdf(NULL)
# Fixed draws make labels, dimensions, and quantiles independent of an MCMC run.
ir = array(seq_len(96)^2 / 100, c(2, 2, 3, 8),
           dimnames = list(c("output", "price"), NULL, 0:2, NULL))
class(ir) = "PosteriorIR"
original = ir
p = plot2(ir, shock_names = c("Demand", "Supply"))
expect_true(inherits(p, "ggplot"))
expect_identical(ir, original)
expect_equal(p$data$position, rep(0:2, each = 4))
b = ggplot2::ggplot_build(p)
s = summary(ir)
for (i in 1:2) {
  for (j in 1:2) {
    rows = p$data$variable == i & p$data$shock == j
    expect_equal(p$data$median[rows],
                   vapply(1:3, function(h) median(ir[i,j,h,]), 0.0))
    expect_equal(p$data$lower[rows], unname(s[[j]][[i]][, "5% quantile"]))
    expect_equal(p$data$upper[rows], unname(s[[j]][[i]][, "95% quantile"]))
  }
}
# Check one off-diagonal panel against the corresponding summary.
panel = b$layout$layout$PANEL[b$layout$layout$variable == 1 & b$layout$layout$shock == 2]
ribbon = b$data[[1]][b$data[[1]]$PANEL == panel, ]
expect_equal(ribbon$ymin, unname(s[[2]][[1]][, "5% quantile"]))
expect_equal(ribbon$ymax, unname(s[[2]][[1]][, "95% quantile"]))
# IR panels have independent y scales and only draw a reference in range.
expect_equal(length(unique(b$layout$layout$SCALE_Y)), 4)
zr = ir
zr[1,2,,] = zr[1,2,,] - 50
expect_equal(sort(as.integer(as.character(unique(
  ggplot2::layer_data(plot2(zr), 3)$PANEL)))), 2L)
labels = unlist(p$facet$params$labeller(data.frame(variable = 1:2, shock = 1:2)))
expect_true(all(c("output", "price", "Demand", "Supply") %in% labels))
edited = p + ggplot2::geom_hline(yintercept = 1, colour = "blue") +
  ggplot2::labs(title = "Edited") + ggplot2::theme_bw()
expect_identical(edited$labels$title, "Edited")
expect_equal(length(ggplot2::ggplot_build(edited)$data), length(b$data) + 1)
expect_equal(nrow(ggplot2::ggplot_build(plot2(ir,
  shock_names = c("same", "same")))$layout$layout), 4)
expect_error(plot2(ir, probability = 1))
expect_error(plot2(ir, probability = NA_real_))
expect_error(plot2(ir, shock_names = "one"))
p68 = plot2(ir, probability = 0.68, linewidth = 1)
expect_equal(p68$data$lower[1], unname(quantile(ir[1,1,1,], 0.16)))
expect_equal(p68$data$upper[1], unname(quantile(ir[1,1,1,], 0.84)))

series = array(seq_len(64) / 10, c(2, 4, 8),
               dimnames = list(c("output", "price"),
                               c("2000", "2000.25", "2000.5", "2000.75"), NULL))
class(series) = "PosteriorFitted"
q = plot2(series)
expect_equal(q$data$median[1], median(series[1,1,]))
expect_equal(q$data$lower[1], unname(quantile(series[1,1,], 0.05)))
expect_equal(q$data$upper[1], unname(quantile(series[1,1,], 0.95)))
expect_equal(sort(unique(q$data$position)), c(2000, 2000.25, 2000.5, 2000.75))
expect_equal(nrow(ggplot2::ggplot_build(q)$layout$layout), 2)
one = series[1,,, drop = FALSE]
class(one) = "PosteriorFitted"
expect_equal(nrow(ggplot2::ggplot_build(plot2(one))$layout$layout), 1)
expect_identical(unname(unlist(q$facet$params$labeller(data.frame(series = 1:2)))),
                 c("output", "price"))
class(series) = "PosteriorSigma"
sigma = plot2(series, shock_names = c("Demand", "Supply"))
expect_equal(unique(ggplot2::layer_data(sigma, 3)$yintercept), 1)
expect_identical(unname(unlist(sigma$facet$params$labeller(data.frame(series = 1:2)))),
                 c("Demand", "Supply"))
class(series) = "PosteriorShocks"
shocks = plot2(series)
expect_identical(unname(unlist(shocks$facet$params$labeller(data.frame(series = 1:2)))),
                 c("shock 1", "shock 2"))
single_draw = series[,,1, drop = FALSE]
class(single_draw) = "PosteriorFitted"
expect_equal(plot2(single_draw)$data$lower,
               plot2(single_draw)$data$upper)

fore = structure(list(Y = matrix(1:10, 2, dimnames = list(c("a", "b"), NULL)),
                       forecasts = array(seq_len(16)^2, c(2, 1, 8))),
                  class = "Forecasts")
fp = plot2(fore, data_in_plot = 0.01)
expect_equal(nrow(fp$data), 4)
expect_equal(fp$data$median[fp$data$position == 1], c(9, 10))
expect_equal(fp$data$lower[fp$data$position == 1], c(9, 10))
expect_equal(fp$data$upper[fp$data$position == 1], c(9, 10))
expect_equal(unique(fp$data$position[fp$data$position > 1]), 2)
expect_equal(fp$data$median[fp$data$position > 1],
               c(median(fore$forecasts[1,1,]), median(fore$forecasts[2,1,])))
expect_equal(nrow(ggplot2::ggplot_build(fp)$layout$layout), 2)
expect_equal(nrow(plot2(fore, data_in_plot = 0.6)$data), 8)
negative_fore = fore
negative_fore$Y = -negative_fore$Y
negative_fore$forecasts = -negative_fore$forecasts
negative_plot = ggplot2::ggplot_build(plot2(negative_fore))
expect_true(all(vapply(negative_plot$layout$panel_params,
  function(panel) max(panel$y$limits) < 0, TRUE)))
expect_error(plot2(fore, data_in_plot = 0))
expect_error(plot2(fore, data_in_plot = NA_real_))
fore2 = fore
fore2$forecasts = array(seq_len(32)^2, c(2, 2, 8))
fp2 = plot2(fore2)
fs2 = summary(fore2)
for (n in 1:2) {
  rows = fp2$data$position > ncol(fore2$Y) & fp2$data$series == n
  expect_equal(fp2$data$lower[rows], unname(fs2[[n]][, "5% quantile"]))
  expect_equal(fp2$data$upper[rows], unname(fs2[[n]][, "95% quantile"]))
}

compact = array(c(0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1), c(1, 3, 4))
class(compact) = "PosteriorRegimePr"
attr(compact, "type") = "realized"
cp = plot2(compact)
for (m in 1:2) for (t in 1:3) {
  rows = cp$data$regime == m & cp$data$position == t
  indicators = as.numeric(compact[1,t,] == m - 1)
  expect_equal(cp$data$median[rows], median(indicators))
  expect_equal(cp$data$lower[rows], unname(quantile(indicators, 0.05)))
  expect_equal(cp$data$upper[rows], unname(quantile(indicators, 0.95)))
}
hmsh = array(rep(compact, 2), c(1, 3, 2, 4))
class(hmsh) = "PosteriorRegimePr"
attr(hmsh, "type") = "realized"
hp = plot2(hmsh)
expect_equal(nrow(ggplot2::ggplot_build(hp)$layout$layout), 4)
for (m in 1:2) for (t in 1:3) for (n in 1:2) {
  rows = hp$data$regime == m & hp$data$position == t & hp$data$process == n
  expect_equal(hp$data$median[rows], median(hmsh[1,t,n,] == m - 1))
}
dense = array(rep(c(0.2, 0.8, 0.4, 0.6, 0.1, 0.9), 4), c(2, 3, 4))
class(dense) = "PosteriorRegimePr"
attr(dense, "type") = "filtered"
dp = plot2(dense)
expect_equal(dp$data$median, c(0.2, 0.8, 0.4, 0.6, 0.1, 0.9))
dense4 = array(rep(dense, 2), c(2, 3, 2, 4))
class(dense4) = "PosteriorRegimePr"
attr(dense4, "type") = "filtered"
dp4 = plot2(dense4)
for (m in 1:2) for (t in 1:3) for (n in 1:2) {
  rows = dp4$data$regime == m & dp4$data$position == t & dp4$data$process == n
  expect_equal(dp4$data$median[rows], median(dense4[m,t,n,]))
}
fevd = array(rep(c(20, 35, 80, 65), 3 * 4), c(2, 2, 3, 4),
             dimnames = list(c("a", "b"), NULL, 0:2, NULL))
class(fevd) = "PosteriorFEVD"
vp = plot2(fevd, shock_names = c("Demand", "Supply"))
vs = summary(fevd)
for (n in 1:2) for (m in 1:2) {
  rows = vp$data$variable == n & vp$data$shock == m
  expect_equal(vp$data$value[rows], unname(vs[[n]][,m]))
}
vb = ggplot2::ggplot_build(vp)$data[[1]]
expect_equal(range(vb$ymin, vb$ymax), c(0, 100))
expect_identical(vp$scales$scales[[1]]$labels, c("Demand", "Supply"))
hd = array(rep(c(2, 4, -3, -1), 3 * 4), c(2, 2, 3, 4),
           dimnames = list(c("a", "b"), NULL, c("2000", "2001", "2002"), NULL))
hd[1,1,2,] = -2
class(hd) = "PosteriorHD"
hdp = plot2(hd)
hds = summary(hd)
for (n in 1:2) for (m in 1:2) {
  rows = hdp$data$variable == n & hdp$data$shock == m
  expect_equal(hdp$data$value[rows], unname(hds[[n]][,m]))
}
hdb = ggplot2::ggplot_build(hdp)$data[[1]]
first = hdb[hdb$PANEL == 1 & hdb$x == 2000, ]
expect_equal(sort(pmin(first$ymin, first$ymax)), c(-3, 0))
expect_equal(sort(pmax(first$ymin, first$ymax)), c(0, 2))
# A sign-changing contribution keeps its stack and tapers to zero between periods.
second = hdb[hdb$PANEL == 1 & hdb$x == 2001, ]
expect_equal(sort(pmin(second$ymin, second$ymax)), c(-5, -2))
expect_equal(sort(pmax(second$ymin, second$ymax)), c(-2, 0))
crossing = hdb[hdb$PANEL == 1 & hdb$group == 1 &
                hdb$x %in% c(2000.5, 2001.5), ]
expect_equal(crossing$x, c(2000.5, 2001.5))
expect_equal(crossing$ymin, crossing$ymax)
quarterly_hd = hd
dimnames(quarterly_hd)[[3]] = as.character(2000 + (0:2) / 4)
quarterly_area = ggplot2::ggplot_build(plot2(quarterly_hd))$data[[1]]
crossing = quarterly_area[quarterly_area$PANEL == 1 & quarterly_area$group == 1 &
                           quarterly_area$x %in% c(2000.125, 2000.375), ]
expect_equal(crossing$x, c(2000.125, 2000.375))
expect_equal(crossing$ymin, crossing$ymax)

# Singleton variable/shock dimensions and a single draw remain arrays.
one_ir = ir[1,1,,, drop = FALSE]
class(one_ir) = "PosteriorIR"
expect_equal(nrow(ggplot2::ggplot_build(plot2(one_ir))$layout$layout), 1)
one_draw_ir = ir[,,,1, drop = FALSE]
class(one_draw_ir) = "PosteriorIR"
expect_equal(plot2(one_draw_ir)$data$lower,
               plot2(one_draw_ir)$data$upper)
for (cl in c("PosteriorFEVD", "PosteriorHD")) {
  one = fevd[1,1,,, drop = FALSE]
  class(one) = cl
  expect_true(inherits(ggplot2::ggplot_build(plot2(one)), "ggplot_built"))
}

# The two entry points use different graphics systems and return types.
counter = new.env()
counter$n = 0L
trace("grid.newpage", where = asNamespace("grid"),
      tracer = function() counter$n = counter$n + 1L, print = FALSE)
base_returned = withVisible(plot(ir))
expect_identical(counter$n, 0L)
expect_false(base_returned$visible)
expect_identical(base_returned$value, ir)
returned = withVisible(plot2(ir))
untrace("grid.newpage", where = asNamespace("grid"))
expect_identical(counter$n, 1L)
expect_false(returned$visible)
expect_true(inherits(returned$value, "ggplot"))
inside_function = function(x) plot2(x)
expect_true(inherits(inside_function(ir), "ggplot"))
custom = plot2(ir, main = "Custom title", xlab = "Periods", col = "blue",
                probability = 0.68, shock_names = c("Demand", "Supply"))
expect_identical(custom$labels$title, "Custom title")
expect_identical(custom$labels$x, "Periods")
expect_equal(unique(ggplot2::ggplot_build(custom)$data[[1]]$fill), "blue")

# Interleaving calls preserves base layout, the ggplot theme and input draws.
layout = graphics::par(c("mfrow", "mar", "oma"))
theme = ggplot2::theme_get()
before = plot2(ir)$data
plot(ir, mar.multi = c(2, 3, 1, 1), oma.multi = c(3, 0, 2, 0))
expect_identical(graphics::par(c("mfrow", "mar", "oma")), layout)
expect_identical(plot2(ir)$data, before)
expect_identical(graphics::par(c("mfrow", "mar", "oma")), layout)
expect_identical(ggplot2::theme_get(), theme)
expect_identical(ir, original)
grDevices::dev.off()
