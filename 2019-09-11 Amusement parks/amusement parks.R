# TidyTuesday for 2019-9-10 -----------------------------------------------
# Amusement Park injuries 
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-09-10
# Gil Henriques

library(tidyverse)
library(gganimate)

# Add fonts ---------------------------------------------------------------
library(showtext)
font_add_google("Noto Sans", "noto")
showtext_auto()

# Read and clean data -----------------------------------------------------
safer_parks <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")
spdf <- safer_parks %>% select(acc_date, gender, age_youngest) %>% 
  mutate(gender = case_when(
    gender %in% c("M", "m") ~ "M", 
    gender %in% c("F", "f") ~"F")
  ) %>%  
  drop_na()
spdf$year <- spdf$acc_date %>% lubridate::mdy() %>% lubridate::year()

# Bin ages ----------------------------------------------------------------
break_points <- c(-Inf, seq(from = spdf$age_youngest %>% min, to = spdf$age_youngest %>% max, by = 5), Inf)
label_names <- round(seq(from = spdf$age_youngest %>% min, by = 5, length = length(break_points)-1),1)
spdf$age.cut <- cut(spdf$age_youngest, breaks = break_points, labels = label_names)# %>% as.character %>% as.numeric

# Make animated plot ------------------------------------------------------
windows()
spdf %>% filter(year > 2010) %>% group_by(year, gender, age.cut) %>% summarize(count = n()) %>% ungroup %>% 
  mutate(count = if_else(gender == "F", count, -count)) %>% 
  ggplot(aes(x = age.cut, y = count,  fill = gender)) + geom_col(position = "stack") + 
  coord_flip() +
  scale_y_continuous(breaks = seq(from = -200, to = 200, by = 50), 
                     labels = c(seq(from = 200, to = 0, by = -50), seq(from = 50, to = 200, by = 50))) +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "noto"),
        plot.subtitle = element_text(color = "gray40"),
        plot.caption = element_text(color = "gray40"),
        axis.text = element_text(color = "gray40")) +
  geom_text(x = 15, y = -100, label = "\u2642", hjust = 0.5, size = 65, color = "steelblue", family = "noto", fontface = "bold") +
  geom_text(x = 15, y = 100, label = "\u2640", hjust = 0.5, size = 65, color = "salmon", family = "noto", fontface = "bold") +
  xlab("Age class of youngest\n ") + ylab("Number of injuries") +
  scale_fill_manual(values = c("salmon", "steelblue")) +
  labs(title = "Amusement Park injuries in {round(frame_time)}", subtitle = "Age and gender distribution for injuries in the USA",
       caption = "Plot by @_Gil_Henriques for #TidyTuesday / Data from data.world") +
  transition_time(year) # this is the gganimate part 

# Save gif ----------------------------------------------------------------
anim_save("amusementparks.gif")
