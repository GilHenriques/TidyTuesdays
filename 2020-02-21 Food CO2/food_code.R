# TidyTuesday: Co2 and food / February 21, 2020 / Gil J.B. Henriques
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-02-18/readme.md

library(tidyverse)
library(ggtext)


# Set font ----------------------------------------------------------------
sysfonts::font_add_google("Roboto Condensed", "rc")
showtext::showtext_auto()


# Read data --------------------------------------------------------------
food_consumption <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')


# Map co2 per country -----------------------------------------------------
world_map <- map_data("world") %>% 
  mutate(country = region)

windows()
(map <- food_consumption %>% 
  group_by(country) %>% 
  summarize(co2 = sum(co2_emmission)) %>% 
  right_join(world_map, by = "country") %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = co2)) +
  geom_polygon(color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgray") +
  coord_quickmap() +
  cowplot::theme_map() +
  theme(text = element_text(family = "rc"), legend.position = c(0.1,0.35), legend.title = element_text(size = 10)) +
  labs(fill = expression(paste("CO"[2], " kg/person/yr")),
       subtitle = "Food-related carbon emissions per capita vary considerably between countries. Why is this the case?"))


# Ternary plot of diet per country ----------------------------------------
diet_per_country <- food_consumption %>% 
  mutate(continent = countrycode::countrycode(country, 
                                              origin = "country.name", 
                                              destination = "continent"),
         food_type = case_when(food_category %in% c("Wheat and Wheat Products", "Soybeans", "Rice", "Nuts inc. Peanut Butter") ~ "Plants",
                               food_category %in% c("Milk - inc. cheese", "Eggs") ~ "Eggs and dairy",
                               TRUE ~ "Meat and fish")) %>% 
  group_by(continent, country, food_type) %>% summarize(co2 = sum(co2_emmission)) %>% 
  ungroup()

diet_per_continent <- diet_per_country %>% 
  group_by(continent, food_type) %>%
  summarize(co2 = sum(co2)) %>% 
  mutate(country = NA)

diet <- bind_rows(diet_per_country, diet_per_continent) %>% 
  pivot_wider(names_from = food_type, values_from = co2)

colors <- c("gold", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

windows()
(diet_ternplot <- diet %>% 
    ggtern::ggtern(aes(x = `Eggs and dairy`, y = `Meat and fish`, z = Plants, color = continent,
                       label = if_else(is.na(country), paste0("   ", continent), ""))) +
    geom_point(show.legend = FALSE, 
               alpha = if_else(is.na(diet$country), 0, 0.5), 
               size = if_else(is.na(diet$country), 0, 1)) +
    geom_point(show.legend = FALSE, 
               alpha = if_else(is.na(diet$country), 1, 0), 
               size = if_else(is.na(diet$country), 3, 0),
               color = "black") +
    geom_point(show.legend = FALSE, 
               alpha = if_else(is.na(diet$country), 1, 0), 
               size = if_else(is.na(diet$country), 2, 0)) +
    geom_text(show.legend = FALSE, hjust = 0, size = 3.4, fontface = "bold", color = "black", family = "rc") +
    ggtern::theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(family = "rc")) +
    #xlab("Eggs &\nDairy") + ylab("Meat & fish") + ggtern::zlab("Plants") +
    xlab("")+ylab("")+ggtern::zlab("") +
    geom_text(data = data.frame(x = c(0, 0, 1), y = c(0, 1, 0), z = c(1, 0, 0), text = c("Plants", "Meat & Fish", "Egg &\nDairy")), 
              aes(x=x, y=y, z=z, label = text), color = "black", family = "rc") +
    labs(subtitle = "One reason is that different countries have very different diets...") +
    scale_color_manual(values = colors) 
)


# Bar plot of co2 per diet ------------------------------------------------
food_colors <- c("black", "palegreen", "khaki1", "palegreen3", "lightpink", "indianred1", "palegreen4",
                 "indianred2", "indianred3", "khaki3", "red3")

windows()
(diet_barplot <- food_consumption %>% 
  group_by(food_category) %>% 
  summarize(co2 = sum(co2_emmission)) %>% 
  ungroup() %>% 
  mutate(food_type = case_when(food_category %in% c("Wheat and Wheat Products", "Soybeans", "Rice", "Nuts inc. Peanut Butter") ~ "Plants",
                               food_category %in% c("Milk - inc. cheese", "Eggs") ~ "Eggs and dairy",
                               TRUE ~ "Meat and fish"),
         food_category = case_when(food_category == "Wheat and Wheat Products" ~ "Wheat",
                                   food_category == "Nuts inc. Peanut Butter" ~ "Nuts",
                                   food_category == "Milk - inc. cheese" ~ "Dairy",
                                   food_category == "Soybeans" ~"", # hide soybeans category -- too small
                                   TRUE ~ food_category)) %>% 
  ggplot(aes(reorder(food_type, -co2, sum), co2/1000, fill = reorder(food_category, co2), label = reorder(food_category, co2))) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  geom_text(position = position_stack(vjust = .5), family = "rc") +
  cowplot::theme_minimal_hgrid() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  scale_fill_manual(values = food_colors) +
  xlab("") + ylab(expression(paste("CO"[2], " metric tonnes/person/yr"))) +
  theme(text = element_text(family = "rc")) +
  labs(subtitle = "... and animal-based diets are more carbon intensive than plant-based ones"))



# Plots relating consumption, co2, and gdp --------------------------------
# Data comes from world bank
gdp_wb <- wbstats::wb(indicator = "NY.GDP.PCAP.CD", startdate = 2018, enddate = 2018) %>%
  mutate(gdp_per_capita = value) %>% 
  select(gdp_per_capita, country) %>% 
  mutate(country = case_when(country == "United States" ~ "USA",
                             TRUE ~ country))

gdp_cons <- food_consumption %>% group_by(country) %>% summarize(consumption = sum(consumption)) %>% 
  left_join(gdp_wb) %>% 
  drop_na() %>% 
  mutate(continent = countrycode::countrycode(country, 
                                              origin = "country.name", 
                                              destination = "continent"))

colors <- c("gold", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

windows()  
(gdp_plot <- gdp_cons %>% 
  ggplot(aes(gdp_per_capita, consumption, color = continent,
             label = if_else((consumption == max(consumption) | gdp_per_capita == max(gdp_per_capita) | 
                              gdp_per_capita == min(gdp_per_capita) | country == "USA"), 
                              country, ""))) +
  geom_point() +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_manual(values = colors) +
  ggrepel::geom_text_repel(show.legend = FALSE, family = "rc") +
  cowplot::theme_minimal_grid() +
  theme(legend.position = c(0.025, 0.75), legend.title = element_blank(), text = element_text(family = "rc")) +
  ylab("Consumption (kg/person/yr)") + xlab("GDP/person (in 2018 USD)") +
  labs(subtitle = "Another reason is that people from richer countries consume more food..."))
  
windows()
(cons_plot <- food_consumption %>% group_by(country) %>% summarize(consumption = sum(consumption), co2 = sum(co2_emmission)) %>% 
  mutate(continent = countrycode::countrycode(country, 
                                              origin = "country.name", 
                                              destination = "continent")) %>% 
  ggplot(aes(consumption, co2, color = continent,
             label = if_else((consumption == max(consumption) | co2 == max(co2) | 
                                co2 == min(co2) | country == "USA"),
                             country, "")
             )) +
  geom_point(show.legend = FALSE) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_manual(values = colors) +
  ggrepel::geom_text_repel(show.legend = FALSE, family = "rc") +
  cowplot::theme_minimal_grid() +
  theme(text = element_text(family = "rc")) +
  xlab("Consumption (kg/person/yr)") + ylab(expression(paste("CO"[2], " metric tonnes/person/yr"))) +
  labs(subtitle = "... which results in higher carbon emissions.") )


# Assemble everything together --------------------------------------------
legend <- cowplot::get_legend(
  gdp_plot + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

windows()
(consumption_plots <- cowplot::plot_grid( cowplot::plot_grid(gdp_plot + theme(legend.position = "none"),
                                       cons_plot, nrow = 1), legend, ncol = 1, rel_heights = c(1, .1)  )
)

# Final arrangement
windows()
many_plots <- cowplot::plot_grid(
  map,
  cowplot::plot_grid(ggplotGrob(diet_ternplot), diet_barplot, nrow = 1),
  consumption_plots,
  ncol = 1
)

title <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "How does the carbon footprint of food vary across the world?",
    fontface = "bold",
    fontfamily = "rc",
    size = 19,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7),
    text = element_text(family = "rc")
  )

end_note <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Data from nu3 and World Bank. Plot by @_Gil_Henriques for #TidyTuesday.",
    fontfamily = "rc",
    size = 8,
    x = 1,
    hjust = 1
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7),
    text = element_text(family = "rc")
  )

windows()
cowplot::plot_grid(
  title, many_plots, end_note,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.025, 1, 0.01)
)


ggsave("co2food.pdf", width = 12, height = 16)
