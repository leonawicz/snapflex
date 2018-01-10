# @knitr setup
library(leaflet)
library(dplyr)
library(ggplot2)
library(showtext)
font_add_google("Lato", "lato")
showtext_auto()

library(snapplot)
library(snapstat)
library(snapclim)
reg <- snaplocs::get_province(loc)

clrs1 <- snapplot::snapalettes(1)[c(1, 2, 4, 8)]
gg_ts <- function(x, variable, title, subtitle, col, base_size = 20, base_family = "lato"){
  ggplot(filter(x, Var == variable), aes(Year, Mean, colour = Season, fill = Season)) +
    geom_line(size = 1, alpha = 0.5) + geom_point(size = 2) +
    geom_point(colour = "white", pch = 21, size = 2) + geom_smooth(method = "lm", se = FALSE) +
    snapplot::theme_snap(base_size = base_size, base_family = base_family) +
    scale_color_manual(labels = levels(x$Season), values = col) +
    scale_fill_manual(labels = levels(x$Season), values = col) +
    labs(title = title, subtitle = subtitle)
}
gg_bars <- function(x, variable, title, subtitle, col, base_size = 28, base_family = "lato"){
  ggplot(filter(x, Var == variable), aes(Season, Mean, colour = Season, fill = Season)) +
    geom_col(size = 1) +
    snapplot::theme_snap(base_size = base_size, base_family = base_family) +
    scale_colour_manual(labels = levels(x$Season), values = col) +
    scale_fill_manual(labels = levels(x$Season), values = col) +
    labs(title = title, subtitle = subtitle)
}

x <- climdata("ar5stats", loc, time_scale = "seasonal") %>%
  filter(Var %in% c("pr", "tas")) %>%
  deltas() %>% filter(Year >= 2020 & Year < 2100) %>% group_by(Var, Season, Year) %>%
  summarise(Mean = mean(Mean))
x2 <- filter(x, Year >= 2070) %>% summarise(Mean = mean(Mean)) %>%
  mutate(Mean = ifelse(Var == "pr", Mean - 1, Mean))
subtitle <- "From 1960 - 1989 seasonal climatologies"

ndec <- (mean(c(2070, 2099)) - mean(c(1960, 1989))) / 10
total_change <- summarise(x2, Mean = mean(Mean))$Mean
pct_per_dec <- round(100 * (total_change[1]^(1 / ndec)), 1)
dif_per_dec <- round(total_change[2] / ndec, 1)

# nolint start

# @knitr sb_title
area <- paste0(loc, ", ", reg)
cat("###", area, "\n>Projected seasonal changes in temperature and precipitation\n\n")

cat("<p align='justify'>These climate graphs show projected changes in seasonal temperature and precipitation for ", paste0(area, "."),
    " (Top) Annual time series of projected delta change for 2020 - 2099 based on five climate models and three greenhouse gas emissions scenarios. (Bottom) Average seasonal change for the 2070 - 2099 period compared to the 1960 - 1989 historical baseline.</p>\n")

cat("<p align='justify'>The top splits temperature and precipitation projections onto two tabs. All graphs shows projections for each three-month seasonal period. Seasons are defined with winter beginning from prior year December through current year February (DJF) and so on (MAM, JJA, SON). Historical baseline climatologies are also seasonal for making delta change comparisons between seasons.</p>\n")

cat("<p align='justify'>Results shown are based on SNAP's 2-km downscaled gridded AR5/CMIP5 climate model outputs and historical CRU data, extracted in the vicinity of", paste0(area, "."), "Estimations and graphs are made with the R programming language and presented using <code>flexdashboard</code>.</p>\n")

#valueBox(dif_per_dec, icon = "ion-android-sunny", color = "#FF3030")
#valueBox(pct_per_dec, icon = "ion-ios-rainy", color = "#1E90FF")

# nolint end

# @knitr sb_map
snapplot::leafloc(loc)

# @knitr plot1a
gg_ts(x, "tas", "2020 - 2099 seasonal temperature deltas", subtitle, clrs1) +
  ylab(expression(Temperature~deltas~(degree~C)))

# @knitr plot1b
gg_ts(x, "pr", "2020 - 2099 seasonal precipitation deltas", subtitle, clrs1) +
  ylab(expression(Precipitation~deltas~(ratio)))

# @knitr plot2a
gg_bars(x2, "tas", "2070 - 2099 mean temperature change", subtitle, clrs1) +
  ylab(expression(Mean~temperature~change~(degree~C))) + theme(legend.position = "none")

# @knitr plot2b
gg_bars(x2, "pr", "2070 - 2099 mean precipitation change", subtitle, clrs1) +
  ylab(expression(Mean~precipitation~change~(symbol("\045")))) + theme(legend.position = "none")
