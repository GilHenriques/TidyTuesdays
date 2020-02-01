# TidyTuesday: San Francisco trees / February 1, 2020 / Gil J.B. Henriques

library(tidyverse) # general functions
library(maptools) # mapping
library(cartogram) # for cartogram making
library(cowplot) # for combining and aligning plots

# Import shapefile for San Francisco. Downloaded from Data SF at: https://data.sfgov.org/Geographic-Locations-and-Boundaries/SF-Find-Neighborhoods/pty2-tcw4
san_francisco_shapes <- sf::st_read("SF Find Neighborhoods/geo_export_c2fe8148-63b9-4183-bcc4-3c82da4684b7.shp")
san_francisco_map <- san_francisco_shapes[2] 

# Import tree dataset from TidyTuesday github
sf_trees <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# Make four plots: one for each of the 3 most common species and one for all trees
plot_list <- vector(mode = "list", length = 4L)
for(i in 1:4){
  most_common <- sf_trees %>% 
    group_by(species) %>% 
    summarize(n = n()) %>% 
    top_n(4, n) %>%
    slice(i) %>% 
    pull(species) # for i in 1:3 this gives us the three most common species
  
  # Get coordinates of trees in map format
  if(i < 4) trees <- sf_trees %>% filter(species == most_common)
  else trees <- sf_trees
  
  trees <- trees %>% 
    filter(
      !is.na(longitude), 
      longitude >= -125,
      latitude > 37.6
    ) %>% 
    select(x = longitude, y = latitude) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(san_francisco_map))
  
  
  # Get number of trees per section
  trees_per_section <- trees %>%
    sf::st_intersection(san_francisco_map) %>% 
    dplyr::add_count(name) %>% 
    group_by(name) %>% summarize(n = first(n))
  
  # Join map and tree number
  san_francisco_map_with_trees <- left_join(san_francisco_map, trees_per_section, by = "name")
  san_francisco_map_with_trees <- sf::st_transform(san_francisco_map_with_trees[2], 3857)
  
  # Construct a cartogram 
  san_francisco_cartogram <- cartogram(san_francisco_map_with_trees, "n", itermax = 5)
  
  # Plot cartogram
  plot_list[[i]] <- san_francisco_cartogram %>% fortify() %>% ggplot() + 
    geom_sf(aes(fill = n/100, color = n/100), size = 2) + 
    scale_fill_viridis_c(option = "plasma") +
    scale_color_viridis_c(option = "plasma") +
    labs(fill = "Hundreds of trees", 
         color = "Hundreds of trees", 
         subtitle = if_else(i < 4, gsub(":.*", "", most_common), "All trees in San Francisco")) +
    theme(
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      legend.position = "bottom", #c(0.1,0.85),
      legend.background = element_rect(fill = "transparent"),
      axis.ticks = element_blank()
    )
  
  print(i)
}

# Arrange panels and title and save plot
title <- ggdraw() + draw_label("Most common trees in San Francisco", fontface = "bold", x = 0.5, hjust = 0.5)
label <- ggdraw() + draw_label("Data from SFGov | Plot by @_Gil_Henriques for #TidyTuesday", x = 1, hjust = 1, size = 8)
plot_grid(
  title, 
  plot_grid(plot_list[[4]], plot_list[[3]], plot_list[[2]], plot_list[[1]], nrow = 2, align = "vh"),
  label,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.05, 1, 0.03)
)

ggsave("trees_SF.pdf", width = 5.75, height = 6.45)
