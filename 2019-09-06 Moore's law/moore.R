# 2019, September 06 - Gil J. B. Henriques
# TidyTuesday: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-09-03

library(tidyverse)

# Adding font -------------------------------------------------------------
library(showtext) # for google fonts
font_add_google("Fira Mono", "firamono")
showtext_auto()


# Load data ---------------------------------------------------------------
cpu <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-03/cpu.csv")



# Fit model that follows Moore's law to data ------------------------------
expFunc <- function(t, n0, td){ 
  # number of transistors as a function of time, initial number of transistors, and doubling time
  n0*2^(t/td)
} 

count <- cpu$transistor_count
date <- cpu$date_of_introduction
fit <- nls(count ~ expFunc(date, n0, 2), start = list(n0 = 2000))
year_seq <- seq(min(date), max(date), by = 0.1)
prediction <- expFunc(year_seq, summary(fit)$parameters[1], 2)


# Plot deviation from prediction and geom_smooth -----------------------------
df <- cpu %>% mutate(prediction = expFunc(date_of_introduction, summary(fit)$parameters[1], 2), 
                     deviation = transistor_count/prediction) %>% drop_na

df %>% ggplot(aes(x = date_of_introduction, y = deviation)) + geom_point() + scale_y_log10() +
  geom_smooth() -> plot


# Find the points within/outside the bands --------------------------------
bounds <- ggplot_build(plot)$data[[3]]
color_df <- tibble(date = bounds$x, above = bounds$ymax > 0, below = bounds$ymin < 0)
color_df <- color_df %>% mutate(color = paste(above, below))
color_vec <- as.numeric(factor(color_df$color))
rectangles <- tibble(xstart = color_df$date[c(1,1+which(diff(color_vec)!=0))], 
       xend =  c(color_df$date[c(1+which(diff(color_vec)!=0))], max(color_df$date)),
       col = color_vec[color_vec != lag(color_vec, default = !color_vec[1])]
)


# Make plot ---------------------------------------------------------------
labels <- tibble(
  x1 = c(1972, 1983.5, 2008.5, 2017.5),
  y1 = c(1, 1, 2.5, 0.4),
  x2 = c(1973.5, 1984, 2005, 2014),
  y2 = c(6.5e-2, 0.85e01, 5e01, 4e-2),
  text = c("The black line indicates Moore's prediction.\nPoints above (below) it indicate CPUs with\nmore (fewer) transistors than predicted.",
           "When the gray bands intersect\nthe prediction, Moore's law holds.",
           "Blue bands indicate time periods   \nwhen technology is ahead of Moore's law.   ",
           "In recent years (red band),\ntechnology has moved slower\nthan Moore's law.")
)


windows()
ggplot(data = df, aes(x = date_of_introduction, y = deviation)) + 
  theme_minimal() +
  geom_rect(data = rectangles, aes(x = NULL, y = NULL, ymin = 0.005, ymax = 100, xmin = xstart, xmax = xend, fill = factor(col)), alpha = 0.5) +
  geom_point(alpha = 0.3) + 
  scale_y_log10(limits = c(0.005,100), labels = scales::math_format()) +
  geom_hline(yintercept = 1, size = 2, alpha = 0.5) +
  geom_smooth(color = "black", lty = "dotted") +
  ylab("Number of transistors / Prediction") + xlab("Year") +
  scale_fill_manual(values = c("salmon", "lightblue", "white")) +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "firamono")) +
  geom_curve(data = labels[c(1,3),], aes(x = x1, y = y1, xend = x2, yend = y2), curvature = 0.25) +
  geom_curve(data = labels[c(2,4),], aes(x = x1, y = y1, xend = x2, yend = y2), curvature = -0.25) +
  geom_text(data = labels, aes(x = x2, y = y2, label = text), size = 3.3, hjust = c(0, 0.5, 1, 1), vjust = c(1, 0, 0.5, 1)) +
  scale_x_continuous(breaks = seq(1970, 2018, by = 8)) +
  labs(title = "Moore's law holds up pretty well", 
       subtitle = "Moore's law predicts that the number of transistors in an\nintegrated circuit doubles every two years.",
       caption = "Plot by @_Gil_Henriques for #TidyTuesday")
  
ggsave("moore_law.pdf", width = 6.5, height = 5.5)