# 2019, August 27 - Gil J. B. Henriques
# TidyTuesday: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-27

library(tidyverse)
library(showtext) # for google fonts

font_add_google("Gaegu", "gaegu")
showtext_auto()


# Read and prepare data ---------------------------------------------------
df <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")
df2 <- df %>% mutate(number = strsplit(as.character(number), "; ")) %>% 
  unnest() %>% # Splits rows that contain multiple episodes into multiple rows
  group_by(season) %>% summarize(guests_per_episode = n_distinct(guest_star)/n_distinct(number))


# Prepare for plot --------------------------------------------------------
# I want the axes and the line connecting the points to look hand-drawn, so I will use geom_line + jitter to draw them
spline_int <- as.data.frame(spline(as.numeric(df2$season), df2$guests_per_episode, method = "natural"))
x.ax <- data.frame(x = seq(from = 0, to = 30, by = 1), y = rep(0, 31))
y.ax <- data.frame(x = rep(0, 21), y = seq(from = 0, to = 4, by = 0.2))


# Plotting ----------------------------------------------------------------
windows()
plot <- df2 %>% 
  ggplot(aes(x = as.numeric(season), y = guests_per_episode)) + 
  geom_point(size = 4, color = "white") + 
  theme_void() + 
  theme(text = element_text(family = "gaegu"),
        axis.text = element_text(size = 15, color = "white"),
        axis.title = element_text(size = 20, color = "white"),
        plot.title = element_text(size = 25,face = "bold", color = "white", hjust = 0.5),
        plot.caption = element_text(color = "white"),
        plot.margin = margin(t = 1, r = 3, b = 1, l = 1, "cm"),
        axis.title.y = element_text(angle = 90)) + 
  ylab("Guests per episode") + xlab("Season") + 
  labs(title = "The number of guest stars in\nthe Simpsons is increasing", caption = "@_Gil_Henriques for #TidyTuesday") +
  ylim(-0.1,4.3) + xlim(-0.1, 30.5) + 
  geom_line(data = spline_int, aes(x = x, y = y), color = "white", position = position_jitter(w = 0.05, h = 0.05), size = 0.8) +
  geom_line(data = x.ax, aes(x = x, y = y), color = "white", position = position_jitter(w = 0.0, h = 0.05), size = 0.8) +
  geom_line(data = y.ax, aes(x = x, y = y), color = "white", position = position_jitter(w = 0.05, h = 0.0), size = 0.8)

ggimage::ggbackground(plot, "chalkboard_simpsons.gif", by = "height")
