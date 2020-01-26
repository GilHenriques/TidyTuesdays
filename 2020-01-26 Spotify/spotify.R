# TidyTuesday: Spotify music / January 26, 2020 / Gil J.B. Henriques
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-21/readme.md

library(tidyverse)
library(showtext)

font_add_google(name = "Roboto", family = "roboto") # Add Roboto font

df <- tidytuesdayR::tt_load('2020-01-21') # Load data
df <- df$spotify_songs

coord_radar <- function (theta = "x", start = 0, direction = 1){ # Function by Hadley Wickham, for radar plot coordinates
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction), is_linear = function(coord) TRUE)
}

showtext_auto() # turn on showtext
windows() 
df %>% 
  select(track_id, track_album_release_date, track_popularity, playlist_genre, 
         danceability, energy, tempo, duration_ms, valence, loudness) %>%
  mutate(track_album_release_date = lubridate::ymd(track_album_release_date),
         track_album_release_date = lubridate::year(track_album_release_date),
         playlist_genre = fct_recode(playlist_genre, 
                                     EDM = "edm", Latin = "latin", Pop = "pop",
                                     `R&B` = "r&b", Rap = "rap", Rock = "rock")) %>% 
  rename(Year = track_album_release_date) %>% 
  pivot_longer(-(1:4), names_to = "Feature", values_to = "value") %>% 
  group_by(Feature) %>% 
  mutate(value = scales::rescale(value, to = c(0, 1))) %>% 
  ungroup() %>% 
  group_by(playlist_genre, Year, Feature) %>% 
  summarize(mean_value = mean(value)) %>% 
  ggplot(aes(x = Feature, y = mean_value, color = Year, group = Year)) + 
  geom_point() + 
  geom_polygon(fill = NA) +
  facet_wrap(. ~ playlist_genre) +
  coord_radar() +
  scale_x_discrete(labels = c("Danceability", "Length", "Energy", 
                              "Loudness", "Tempo", "Valence")) +
  theme(text = element_text(family = "roboto"),
        axis.text.x = element_text(angle = 360/(2*pi)*rev(seq(pi/6, 2*pi-pi/6, len = 6)), 
                                   color = "gray95", size = 11),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 20, face = "bold", color = "#36524d", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#36524d", hjust = 0.5),
        panel.background = element_rect(fill = "#36524d"), #"gray30"),
        strip.background = element_rect(fill = "white", colour = "white"),
        strip.text.x = element_text(colour = "#36524d", face = "bold", size = 12),
        legend.position = "bottom",
        panel.grid = element_line(colour = "darkgray")) +
  labs(title = "Music, then and now", 
       subtitle = "The evolution of musical features during the last 60 years",
       caption = "Features normalized between 0 and 1. Data from Spotify, via the spotifyr package. Plot by @_Gil_Henriques for #TidyTuesday") +
  xlab(NULL) + ylab(NULL) +
  scale_color_viridis_c()

ggsave("spotify_plot.pdf", width = 9, height = 8)




