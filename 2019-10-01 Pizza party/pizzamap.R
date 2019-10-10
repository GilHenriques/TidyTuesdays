# TidyTuesday 2019-10-01: Pizza party!
# Gil J. B. Henriques

library(tidyverse)

# Fonts -------------------------------------------------------------------
library(showtext) # for google fonts
font_add_google("Roboto", "roboto")
showtext_auto()

# Read and filter data ----------------------------------------------------
pizza_barstool <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")
NYpizza <- pizza_barstool %>% filter(city == "New York", longitude > -74.1) 


# Get census tracts from latitude and longitude ---------------------------
# method by Danny Farnand at StackOverflow: https://stackoverflow.com/questions/51499410/retrieve-census-tract-from-coordinates
NYpizza$census_code <- apply(NYpizza, 1, function(row) tigris::call_geolocator_latlon(row['latitude'], row['longitude'])) # this takes a while
NYpizza$id <- substr(NYpizza$census_code, 1, 11)


# Get census tract map data for NYC and join it with pizza data -----------
tigris::lookup_code("New York", "New York")
nyc_tracts <- tigris::tracts(state = '36', county = c('061'))
nyc_map <- fortify(nyc_tracts, region = "GEOID")

nyc_full <- left_join(NYpizza, nyc_map, by = "id") %>% drop_na
nyc_short <- nyc_full %>% select(name, id, review_stats_all_average_score, long, lat, order, hole, piece, group) %>% 
  group_by(name) %>% mutate(count = n(), score = mean(review_stats_all_average_score)) %>% distinct(name, .keep_all = T)


# Create bivariate (score and count) categories ---------------------------
# method by Timo Grossenbacher at https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

# create 3 buckets for score
quantiles_score <- nyc_short %>%
  pull(score) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for count
quantiles_count <- nyc_short %>%
  pull(count) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for score and blue for count
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", 
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", 
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", 
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", 
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" 
) %>%
  gather("group", "fill")


nyc_short %<>%
  mutate(
    score_quantiles = cut(
      score,
      breaks = quantiles_score,
      include.lowest = TRUE
    ),
    count_quantiles = cut(
      count,
      breaks = quantiles_count,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(
      as.numeric(score_quantiles), "-",
      as.numeric(count_quantiles)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")



# Plot map with bivariate colors ------------------------------------------
map <- ggplot() + geom_map(data = nyc_map, map = nyc_map, aes(x = long, y = lat, map_id = id), color = "lightgrey", fill = "darkgrey") +
  geom_map(data = nyc_short, map = nyc_map, aes(x = long, y = lat, map_id = id, fill = group)) + theme(legend.position = "none") +
  #geom_map(data = nyc_short, map = nyc_map, aes(x = long, y = lat, map_id = id, fill = score), alpha = 0.5) + theme(legend.position = "none") +
  #geom_point(data = NYpizza, aes(x = longitude, y = latitude), alpha = 0.5) +
  coord_map() +
  theme_void(base_family = "roboto") + theme(legend.position = "none") +
  scale_fill_manual(values = bivariate_color_scale$fill) +
  annotate("text", x = -74.01, y = 40.85, label = "Where to buy\npizza in NYC?", size = 7) +
  labs(caption = "Plot by @_Gil_Henriques for #TidyTuesday.\nData from: Barstool Sports.")



# Create legend for bivariate scale ---------------------------------------
# Again, method by Timo Grossenbacher from https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/

# separate the groups
bivariate_color_scale %<>%
  separate(group, into = c("score", "count"), sep = " - ") %>%
  mutate(score = as.integer(score),
         count = as.integer(count))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = score,
      y = count,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher quality pizza >>",
       y = "More pizza places >>") +
  ggthemes::theme_map(base_family = "roboto") +
  # make font small enough
  theme(
    axis.title = element_text(size = 9)
  ) +
  # quadratic tiles
  coord_fixed()



# Combine map with legend and save ----------------------------------------
library(cowplot)

windows()
ggdraw() +
  draw_plot(map) +
  draw_plot(legend, 0.6, 0.03, 0.25, 0.25)


ggsave("pizzaplot.pdf", height = 8, width = 6)
