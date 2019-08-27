# devtools::install_github("yixuan/showtext")
library(tidyverse) 
library(gganimate)
library(showtext) # for google fonts


# Create world map --------------------------------------------------------
world <- map_data("world")
map <- ggplot() + theme_void() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) + coord_fixed(1.3)


# Read data ---------------------------------------------------------------
meteorites <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
recent <- meteorites %>% 
  filter(long != max(long, na.rm = T) & year < 2020 & !is.na(mass)) %>%  # Remove bad data points
  filter(year > 1900) %>%  # focus on 20th and 21st ctry
  mutate(massKg = mass/1000) %>%
  mutate(massKg = ifelse(massKg > 5000, 5000, massKg)) # change very high masses to 5000+ kg (6 data points affected)


# Import fonts ------------------------------------------------------------
font_add_google("Viga", "viga")
showtext_auto()

# Plot meteorite falls ----------------------------------------------------
windows() # Open plotting device (for Windows OS)

map + 
  geom_point(data = recent, aes(x = long, y = lat, size = massKg), color = "yellow", alpha = 0.5) +
  labs(size = "Mass (kg)", caption = "Data: Meteoritical Society, via NASA: tinyurl.com/yxkrhykk", 
       title = "Meteorites found on Earth", subtitle = "Year found: {frame_time}") +
  theme(legend.position = c(0.15, 0.36), text = element_text(family = "viga")) +
  scale_size(labels=c("0","1000","2000","3000","4000", "5000+"),guide="legend") +
  transition_events(start = year, end = year + 5L, enter_length = 6L, exit_length = 4L) + # from gganimate
  enter_grow() + exit_fade() # from gganimate

anim_save("tidytuesdayMeteorites.gif", path = "R/") # Save gif
