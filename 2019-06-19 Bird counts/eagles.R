library(tidyverse) # Will also need: timetk, forecast, sweep, scales, cowplot

# Read and clean data
bird_counts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-18/bird_counts.csv")
eagles <- bird_counts %>% filter(species=="Bald Eagle", year > 1950) %>% 
  mutate(year = as.Date(ISOdate(year, 12, 24)))

# Create time series object
eagles_ts <- eagles %>% select(year, how_many_counted_by_hour) %>% 
  timetk::tk_ts(start = 1950, frequency = 1, silent = TRUE)

# Fit an auto.arima model
fit_arima <- eagles_ts %>% forecast::auto.arima(D = 1)

# Forecast 10 years into the future
fcast_arima <- fit_arima %>% forecast::forecast(h = 10)

# Plot forecast
forecast <- sweep::sw_sweep(fcast_arima, fitted = FALSE)  %>% 
  ggplot(aes(x = index, y = how_many_counted_by_hour, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#fcdede", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#fcb8b8", color = NA, size = 0, alpha = 0.8) +
  geom_path(aes(group = 1), size = 1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2", labels = c("Observed", "Forecast")) +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.text = element_text(size = 11, 
                                   margin = margin(r = 30, l = 5, unit = "pt"))) +
  labs(title = "How frequently will Bald Eagles be counted in the future?",
       subtitle = "Forecasted (ARIMA) Bald Eagle counts per hour in Hamilton, Ontario",
       caption = "Data from Bird Studies Canada (@BirdsCanada), cleaned by Sharleen W. (@_sharleen_w)") +
  xlab("Year") + ylab("Bald Eagles counted per hour") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# Create map of Canada (for inset)
canada <- map_data("world", region = "Canada")
hamilton <- ggplot() + geom_polygon(data = canada, aes(x = long, y = lat, group = group)) +
  theme_void() + geom_point(aes(x = -79, y = 43), color = "#d95f02", size = 4) +
  annotate("text", x = -79-33, y = 43, color = "#d95f02", size = 4, label = "Hamilton, ON")

# Final plot
final <- cowplot::ggdraw(forecast) + cowplot::draw_plot(hamilton, x = 0.1, y = 0.47, width = .28, height = .28)

cowplot::ggsave(filename = "eagles_tidy_tuesday.png", plot = final, width = 16.5, height = 12, units = "cm")
