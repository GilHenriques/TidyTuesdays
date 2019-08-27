# TidyTuesday: Nuclear explosions! / August 20, 2019 / Gil J.B. Henriques
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-20

library(tidyverse)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# Data on countries' latitude and longitude, from https://developers.google.com/public-data/docs/canonical/countries_csv
countries <- read_delim("countries.txt", "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(country, source.lat = latitude, source.long = longitude)

# Get data for plotting world map.
# Rename countries so they have the same code as in the countries dataframe; also match all former USSR countries under the same code
world.data <- left_join(map_data("world"), data_frame(region = c("USA", "Russia", "UK", "France", "India", "Pakistan", "China", "Georgia", "Ukraine", "Moldova", "Belarus", "Armenia", "Azerbaijan",  "Kazakhstan", "Uzbekistan", "Turkmenistan", "Kyrgyzstan", "Tajikistan"), 
                                                      country = c("US", "RU",     "GB", "FR",     "IN",   "PK",         "CN",   "RU",      "RU",      "RU",      "RU",      "RU",      "RU",          "RU",         "RU",         "RU",           "RU",         "RU")))

# Recode countries from the TidyTuesday data so they have the same code as in the countries dataframe and merge the two dataframes
df2 <- df %>% mutate(country = recode(country, 
                                "USA" = "US", 
                                "USSR" = "RU", 
                                "UK" = "GB", 
                                "FRANCE" = "FR", 
                                "INDIA" = "IN", 
                                "PAKIST" = "PK",
                                "CHINA" = "CN")) %>% 
  left_join(countries, by = "country")

# For labelling
text.df <- df2 %>% select(source.long, source.lat, country) %>% unique
text.df$label <- c("USA", "USSR", "UK", "France", "China", "India", "Pakistan")

# Plot
df2 %>% ggplot() + 
  geom_polygon(data = world.data, aes(x = long, y = lat, group = group), fill = "lightgray") +
  geom_polygon(data = world.data, aes(x = long, y = lat, group = group, fill = country), alpha = 0.4) + 
  coord_equal() + theme_void() + 
  theme(panel.background = element_rect(fill = "grey26", color = "grey26"), 
        plot.background = element_rect(fill = "grey26"),
        legend.position = "none",
        plot.title = element_text(color = "lightgray", hjust = 0.5),
        plot.caption = element_text(color = "lightgray", size = 8),
        plot.subtitle = element_text(color = "lightgray", size = 9, hjust = 0.5)) +
  geom_point(aes(x = source.long, y = source.lat), color = "white", size = 5.3) +
  geom_point(aes(x = source.long, y = source.lat, color = country), size = 4) +
  geom_point(aes(x = longitude, y = latitude, color = country, size = (yield_upper + yield_lower)/2)) +
  geom_curve(aes(x = source.long, y = source.lat, xend = longitude, yend = latitude, 
                 color = country, size = (yield_upper + yield_lower)/2), 
             alpha = 0.3) +
  scale_fill_manual(values = c("darkred", "mediumaquamarine", "navy", "orange", "springgreen3", "brown2", "slateblue", "lightgray")) +
  scale_color_manual(values = c("darkred", "mediumaquamarine", "navy", "orange", "springgreen3", "brown2", "slateblue", "lightgray")) +
  geom_label(data = text.df, aes(x = source.long, y = source.lat, label = label, fill = country), color = "white", hjust = 0, nudge_x = 4, size = 3) +
  labs(title = "Nuclear explosions (1945-2007)", 
       subtitle = "Source country and target location of nuclear explosions.\nLine thickness indicates yield.",
       caption = "Data: SIPRI. Plot: Gil Henriques")

# Print plot
ggsave("Nuclear_explosions.png", width = 8, height = 4.5)  
