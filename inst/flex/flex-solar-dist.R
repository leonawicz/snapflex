library(dplyr)
library(fitdistrplus)
rsds1 <- readRDS(url("https://s3.amazonaws.com/leonawicz/apps/snapflex/rsds1/rsds1.rds"))
loc <- "Vancouver"
loc2 <- paste(loc, snaplocs::get_state(loc), sep = ", ")
d <- rsds1[[loc2]]
x <- filter(d, Year %in% 2050:2050)$value

dist_names <- c("Weibull", "lognormal", "gamma", "normal")
dists <- c("weibull", "lnorm", "gamma", "norm")
fitted <- purrr::map(dists, ~fitdist(x, .x))
aic <- purrr::map_dbl(fitted, "aic")

par(mfrow = c(2, 2))
plot.legend <- dist_names
denscomp(fitted, legendtext = plot.legend)
qqcomp(fitted, legendtext = plot.legend)
cdfcomp(fitted, legendtext = plot.legend)
ppcomp(fitted, legendtext = plot.legend)

data_frame(Distribution = dist_names, AIC = aic) %>% arrange(desc(AIC))

library(ggplot2)
pars <- fitted[[4]]$estimate
dseq <- seq(pars[1] - 2, pars[1] + 2, length.out = 200)
dn <- dnorm(dseq, mean = pars[1], sd = pars[2])
ggplot() + geom_rug(aes(x = x), sides = "b") + geom_line(data = data_frame(dseq = dseq, dn = dn), aes(dseq, dn))
