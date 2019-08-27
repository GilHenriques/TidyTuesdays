library(tidyverse)
library(ggalluvial)  


# Read data ---------------------------------------------------------------
emperors <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")


# Part 1: chronology ------------------------------------------------------
df <- emperors %>% 
  mutate(notes = if_else(is.na(notes), "no notes", notes)) %>%
  # Fix BCE dates
  mutate(birth = if_else(str_detect(notes, "birth is BCE|birth, reign.start are BCE"), -lubridate::year(birth), lubridate::year(birth))) %>%
  mutate(reign_start = if_else(str_detect(notes, "reign.start are BCE"), -lubridate::year(reign_start), lubridate::year(reign_start))) %>% 
  mutate(death = lubridate::year(death)) %>% 
  mutate(reign_end = lubridate::year(reign_end)) %>% 
  select(index, name, birth, death, reign_start, reign_end, dynasty, era) %>% 
  # Turn into long format
  gather(key = "event", value = "date", -c(index, name, dynasty, era))

year_era <- df %>% filter(event == "reign_start", era == "Dominate") %>% summarize(yearmin = min(date, na.rm = T)) %>% pull
df$dynasty <- factor(df$dynasty, levels = reorder(df$dynasty, df$index))

chrono <- df %>% ggplot(aes(x = date, y = reorder(name, index), color = dynasty)) +
  geom_blank() +
  geom_rect(aes(xmin = min(date, na.rm = T) - 15, xmax = max(date, na.rm = T) + 90, ymin = index-0.5, ymax = index+0.5, fill = dynasty), alpha = 0.05, colour = NA) +
  geom_vline(xintercept = year_era, color = "white", size = 2) +
  geom_line(alpha = 0.5, size = 2.6, lineend = "round") +
  geom_line(data = filter(df, event %in% c("reign_start", "reign_end")), aes(x = date, y = reorder(name, index), color = dynasty), size = 2.6, lineend = "round", alpha = 0.9) +
  geom_text(data = filter(df, event == "death"), aes(x = date + 15, y = reorder(name, index), label = name), hjust = 0, size = 3, show.legend = F) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(), 
        legend.position = "bottom") +
  scale_x_continuous(limits = c(min(df$date, na.rm = T) - 15, max(df$date, na.rm = T) + 90), name = "Year", breaks = seq(0, 400, 100)) +
  scale_color_brewer(name = "Dynasty", palette = "Set2") + scale_fill_brewer(name = "Dynasty", palette = "Set2") +
  annotate("text", x = year_era - 10, y = 2, label = "Principate era", size = 3, hjust = 1) +
  annotate("text", x = year_era + 10, y = 2, label = "Dominate era", size = 3, hjust = 0) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
  labs(title = "A chronology of Roman Emperors", subtitle = "Life (light colors) and reign (dark colors) of Roman Emperors")



# Part 2: Rise and fall ---------------------------------------------------
df2 <- emperors %>% select(index, rise, cause)

df2$rise <- case_when(
  str_detect(df2$rise, "Appointment") ~ "Appointment",
  TRUE ~ as.character(df2$rise)
)

alluvial <- df2 %>% ggplot(aes(axis1 = rise, axis2 = cause)) + 
  geom_alluvium(aes(fill = rise), width = 0.2) +
  geom_stratum(alpha = 0.3, fill = c(rep("grey", 5), rep(c("white", "black"), 3), "white"), color = "white", size = 1, width = 0.2) +
  theme_minimal() +
  theme(plot.title = element_text(vjust = -3), legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_text(stat = "stratum", label.strata = TRUE, size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "The rise and fall of Roman Emperors", caption = "Data: Wikipedia. Plot: Gil Henriques") +
  annotate("text", x = 1, y = 71, label = "Rise", size = 4.5) +
  annotate("text", x = 2, y = 71, label = "Fall", size = 4.5)


# Arrange and make pdf ----------------------------------------------------
ggpubr::ggarrange(chrono, alluvial, ncol = 2, widths = c(2, 1.3))
ggsave("emperors.pdf", width = 14, height = 8)
  
