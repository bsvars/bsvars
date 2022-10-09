

library("ggplot2")
library("hexSticker")
library("svglite")
library("magrittr")
library("tidyr")
library("ggpubr")


set.seed(123)
post = us_fiscal_lsuw |>
  specify_bsvar$new(p = 1, stationary = rep(TRUE, 3)) |>
  estimate(S = 1000) |> 
  estimate(S = 5000) 

ir = compute_impulse_responses(post, horizon = 50)

ir_mean = apply(ir[1,2,,], 1, mean)
ir_hdi  = apply(ir[1,2,,], 1, HDInterval::hdi, credMass = .68)
ir_hdi  = apply(ir[1,2,,], 1, HDInterval::hdi, credMass = .68)
plot(1:51, ir_mean, ylim = range(ir_hdi), type = "l")
lines(1:51, ir_hdi[1,])
lines(1:51, ir_hdi[2,])








library("ggplot2")
library("hexSticker")
library("svglite")
library("tidyr")
library("magrittr")
library("ggpubr")

s <- -(75 - 1:150)^2 / 5000 + 2
m <- s^2
set.seed(7)
series <- rnorm(length(s), m + 3.5, s)
p <- data.frame(time = seq_along(series), a = series, b = -series) %>%
  pivot_longer(c(a, b)) %>%
  ggplot(aes(time, value, group = name)) +
  geom_line(color = rgb(54L, 77L, 146L, 255L, maxColorValue = 255L),
            size = 0.55) +
  ggpubr::theme_transparent() +
  theme(legend.position = "none",
        strip.text.y = element_blank())
plot(p)

sticker(p, package = "stochvol", p_size = 10,
        s_x = 1, s_y = 1, s_width = 1.9, s_height = 2.0,
        p_x = 1, p_y = 1.05,
        #p_family = "mono",
        url = "https://cran.r-project.org/package=stochvol",
        u_color = "grey89", u_size = 1,
        h_fill = rgb(0xe1, 0xad, 0x01, 255, maxColorValue = 255L),
        h_color = "grey85", p_color = rgb(0x28, 0x1f, 0x00, 255L, maxColorValue = 255L),
        filename = "logo_stochvol.svg")
