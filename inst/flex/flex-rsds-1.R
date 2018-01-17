# @knitr setup
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

library(snapplot)
plot_theme <- get(params$snaptheme)

library(showtext)
font_add_google(params$gfont, "gfont", regular.wt = params$regular, bold.wt = params$bold)
showtext_auto()

stat_compare_means <- function(mapping = NULL, data = NULL, method = NULL, paired = FALSE, # override issues in ggpubr
                               method.args = list(), ref.group = NULL, comparisons = NULL,
                               hide.ns = FALSE, label.sep = ", ", label = NULL, label.x.npc = "left",
                               label.y.npc = "top", label.x = NULL, label.y = NULL, tip.length = 0.03,
                               symnum.args = list(), geom = "text", position = "identity",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)
{
  if (!is.null(comparisons)) {
    method.info <- ggpubr:::.method_info(method)
    method <- method.info$method
    method.args <- ggpubr:::.add_item(method.args, paired = paired)
    if (method == "wilcox.test")
      method.args$exact <- FALSE
    pms <- list(...)
    size <- ifelse(is.null(pms$size), 0.3, pms$size)
    textsize <- ifelse(is.null(pms$size), 10, pms$size)
    color <- ifelse(is.null(pms$color), "black", pms$color)
    map_signif_level <- FALSE
    if (is.null(label))
      label <- "p.format"
    if (ggpubr:::.is_p.signif_in_mapping(mapping) | (label %in% "p.signif")) {
      map_signif_level <- c(`****` = 1e-04, `***` = 0.001,
                            `**` = 0.01, `*` = 0.05, ns = 1)
      if (hide.ns)
        names(map_signif_level)[5] <- " "
    }
    step_increase <- ifelse(is.null(label.y), 0.12, 0)
    ggsignif::geom_signif(comparisons = comparisons, y_position = label.y,
                          test = method, test.args = method.args, step_increase = step_increase,
                          size = size, textsize = textsize, color = color, map_signif_level = map_signif_level,
                          tip_length = tip.length, data = data)
  }
  else {
    mapping <- ggpubr:::.update_mapping(mapping, label)
    layer(stat = ggpubr:::StatCompareMeans, data = data, mapping = mapping,
          geom = geom, position = position, show.legend = show.legend,
          inherit.aes = inherit.aes, params = list(label.x.npc = label.x.npc,
                                                   label.y.npc = label.y.npc, label.x = label.x,
                                                   label.y = label.y, label.sep = label.sep, method = method,
                                                   method.args = method.args, paired = paired, ref.group = ref.group,
                                                   symnum.args = symnum.args, hide.ns = hide.ns,
                                                   na.rm = na.rm, ...))
  }
}

d$value <- (24 * d$value) / (1000 * 0.0864) # MJ/m^2/day to kWh/m^2/day

clrs <- c("gray50", "#00AFBB", "#E7B800", snapplot::snapalettes()[c(4, 7, 8)])
clrs2 <- clrs[2:3]
clrs3 <- c("#00AFBB", "#E7B800", snapplot::snapalettes()[4])
contrast <- ifelse(params$snaptheme %in% c("theme_snapdark"), "white", "black")

dsub <- filter(d, Model != "CRU 4.0")
dsum <- dsub %>%
  mutate(Window = ifelse(Year %in% 2010:2039, "2010 - 2039", ifelse(Year %in% 2040:2069, "2040 - 2069", "2070 - 2099"))) %>%
  mutate(Window = factor(Window, levels = unique(Window))) %>%
  group_by(Window, Model) %>% summarise(Mean = mean(value)) %>%
  mutate(Model_Window = paste(Window, Model)) %>% arrange(Window, Mean)

bhats <- signif(lm(value ~ Year, data = dsub)$coefficients, 3)
lm_eqn <- function(df){
  m <- lm(value ~ Year, df)
  eq <- substitute(~~italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = signif(coef(m)[1], 3),
                        b = signif(coef(m)[2], 3),
                        r2 = round(summary(m)$r.squared, 3)))
  as.character(as.expression(eq))
}
n_proj <- length(unique(dsub$Year))
rsds1 <- bhats[1] + bhats[2] * min(dsub$Year)
rsds2 <- bhats[1] + bhats[2] * max(dsub$Year)
ratio <- rsds2 / rsds1
total_pct_change <- signif(100 * (ratio - 1), 2)
change_per_decade <- round(100 * (abs(ratio)^(10 / n_proj) - sign(ratio)), 1)
totpct <- paste0("~~Total~projected~change:~", total_pct_change, '*symbol("\045")')
decpct <- paste0("~~", change_per_decade, '*symbol("\045")/decade')
yrange <- diff(range(d$value))
totpos <- max(d$value) - 0.075 * yrange
decpos <- max(d$value) - 0.15 * yrange
prime_lab <- expression(Solar~Irradiance~(kWh/m^2/day))
prime_lab2 <- "solar irradiance"
pct_change_statement <- paste0("The estimated projected percent change in solar irradiance over the period 2006 - 2100 using the five climate models is ", total_pct_change,
                               "%. This is approximately ", change_per_decade, "% change per decade during the period. These mean estimates are based on the linear regression in figure 1.")
p1size1 <- ifelse(simplify, 1, 0.5)

p1 <- ggplot(d, aes(Year, value)) + geom_smooth(data = filter(d, !(Model == "CRU 4.0" & Year > 2005)), aes(colour = Model), se = FALSE, linetype = "longdash", size = p1size1)
if(!simplify) p1 <- p1 + geom_point(aes(colour = Model), alpha = 0.2)
p1 <- p1 + scale_colour_manual(values = clrs) +
  geom_smooth(data = dsub, colour = contrast, method = "lm", size = 1) +
  plot_theme(base_family = "gfont", base_size = 20) + theme(text = element_text(size=40), plot.margin = unit(c(5, 10, 5, 5), "mm"), axis.text = element_text(size = 40), legend.text = element_text(size = 40)) + guides(colour = guide_legend(override.aes = list(size=5, alpha = 0.5))) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = paste("Projected trend in", prime_lab2, "in", loc2),
       subtitle = "By model and average", x = "Year", y = prime_lab) +
if(!simplify) p1 <- p1 + annotate("text", -Inf, Inf, label = lm_eqn(d), parse = TRUE, size = 12, colour = contrast, hjust = 0, vjust = 1) +
  annotate("text", -Inf, totpos, label = totpct, parse = TRUE, size = 14, colour = contrast, hjust = 0, vjust = 1) +
  annotate("text", -Inf, decpos, label = decpct, parse = TRUE, size = 14, colour = contrast, hjust = 0, vjust = 1)

p2 <- ggdensity(d, x = "value", add = "mean", rug = TRUE, color = "Period", fill = "Period",
                palette = clrs2, size = 1, ggtheme = plot_theme(base_family = "gfont", base_size = 20)) +
  theme(text = element_text(size=40), plot.margin = unit(c(5, 10, 5, 5), "mm"), axis.text = element_text(size = 40), legend.text = element_text(size = 40)) + guides(colour = guide_legend(override.aes = list(size=5))) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = paste("Distributions of", prime_lab2, "in", loc2, "over time"),
       subtitle = "1950 - 2013 CRU 4.0 and 2006 - 2100 GCM outputs", x = prime_lab, y = "Density")

d2 <- d
d2$Model <- reorder(d$Model, d$value, FUN=median)
idx <- match(levels(reorder(d$Model, d$value, FUN=median)), levels(d$Model))
comps <- purrr::map(2:6, ~c(levels(d$Model)[1], levels(d$Model)[.x]))
p3 <- ggboxplot(d2, x = "Model", y = "value",
                color = contrast, fill = "Model", palette = clrs[idx],
                add = "jitter", shape = 21, ggtheme = plot_theme(base_family = "gfont", base_size = 20)) +
  stat_compare_means(comparisons = comps, color = contrast, textsize = 20) +
  stat_compare_means(colour = contrast, size = 12) +
  theme(text = element_text(size=40), plot.margin = unit(c(5, 10, 5, 5), "mm"), legend.key.size = unit(1,"line"), axis.text = element_text(size = 40), legend.text = element_text(size = 40), legend.position = "none") +
  scale_x_discrete(expand = c(0, 0.4)) +
  labs(title = paste("Distributions of", prime_lab2, "in", loc2, "by model"),
       subtitle = "1950 - 2013 CRU 4.0 and 2006 - 2100 GCM outputs. Global and select pairwise tests for difference in means.", x = "Model", y = prime_lab)

dsum <- filter(d, Model != "CRU 4.0" & Year >= 2010 & Year < 2100) %>%
  mutate(Window = ifelse(Year %in% 2010:2039, "2010 - 2039", ifelse(Year %in% 2040:2069, "2040 - 2069", "2070 - 2099"))) %>%
  mutate(Window = factor(Window, levels = unique(Window))) %>%
  group_by(Window, Model) %>% summarise(Mean = mean(value)) %>%
  mutate(Model_Window = paste(Window, Model)) %>% arrange(Window, Mean)

p4 <- ggplot(dsum, aes(factor(Model_Window, levels = unique(Model_Window)), Mean, colour = Window)) + scale_colour_manual(values = clrs3) +
  coord_flip() + plot_theme(base_family = "gfont", base_size = 20) +
  theme(text = element_text(size=40), plot.margin = unit(c(5, 10, 5, 5), "mm"), axis.text = element_text(size = 40), legend.text = element_text(size = 40)) + guides(colour = guide_legend(title = "Period"), override.aes=list(alpha=1)) +
  scale_y_continuous(expand = c(0.025, 0)) + scale_x_discrete(expand = c(0, 1)) +
  geom_segment(aes(y = min(Mean), xend = Model_Window, yend = Mean, colour = Window), size = 1) +
  geom_point(aes(colour = Window), shape = 19, size = 3) +
  geom_text(aes(label = round(Mean, 1)), colour = contrast, size = 10, vjust = 1.7) +
  labs(title = paste("Projected mean", prime_lab2, "by model and time period"),
       subtitle = loc2, x = NULL, y = prime_lab)

set_axis_label_colors <- function(g, data, label, axis){
  gb <- ggplot2::ggplot_build(g)
  cols <- unlist(gb$data[[1]]["colour"])
  names(cols) <- as.vector(data[[label]])
  if(axis == "x") return(g + theme(axis.text.x = element_text(colour = cols)))
  g + theme(axis.text.y = element_text(colour = cols))
}

p4 <- set_axis_label_colors(p4, dsum, "Model_Window", "y")
