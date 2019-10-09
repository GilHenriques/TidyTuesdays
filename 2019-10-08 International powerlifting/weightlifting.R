# International powerlifting
# plot for TidyTuesday 2019-10-08: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-08
# Gil J. B. Henriques

library(tidyverse)

# Fonts -------------------------------------------------------------------
library(showtext) # for google fonts
font_add_google("Roboto", "roboto")
showtext_auto()

# Read data ---------------------------------------------------------------
ipf_lifts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

# Function for split violin plots. Author: jan-glx at StackOverflow -------
# https://stackoverflow.com/a/45614547
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})
geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

# Tidy data and plot ------------------------------------------------------
windows()

ipf_lifts %>% 
  rename(Bench = best3bench_kg, Deadlift = best3deadlift_kg, Squat = best3squat_kg, Sex = sex) %>% 
  pivot_longer(cols = c(Bench, Deadlift, Squat), names_to = "Weightlifting type", values_to = "Maximum weight lifted (kg)") %>% 
  select(Sex, equipment, age_class, age, `Weightlifting type`, `Maximum weight lifted (kg)`) %>% 
  filter(equipment %in% c("Raw", "Single-ply")) %>% # Data doesn't contain any male values for Wraps
  drop_na() %>% 
  ggplot(aes(x = `Weightlifting type`, y = `Maximum weight lifted (kg)`, fill = Sex, color = Sex)) + 
  geom_split_violin(trim = T, alpha = .6) + 
  geom_boxplot(fill = "white", alpha = .8, width = .1, position = position_dodge(width = .2), outlier.shape = NA, coef = 0) +
  facet_wrap(.~ equipment)  +
  scale_color_manual(values = alpha(c("#a80225","#4581ff"),0.4), labels = c("Female", "Male")) +
  scale_fill_manual(values = c("#a80225","#4581ff"), labels = c("Female", "Male")) +
  ylim(0, 500) +
  ggdark::dark_theme_gray(base_family = "roboto", base_size = 14) + 
  theme(plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "top") +
  labs(title = "International powerlifting", subtitle = "Maximum weight lifted with different equipments and weightlifting types", caption = "Plot by @_Gil_Henriques for #TidyTuesday. Data from Open Powerlifting.")

ggsave("powerlifting_plot.pdf", width = 8, height = 5)
