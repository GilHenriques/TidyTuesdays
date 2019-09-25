# TidyTuesday September 25, 2019
# Gil J. B. Henriques

library(tidyverse)
library(geofacet)
library(showtext) # for google fonts


# Read data ---------------------------------------------------------------
school_diversity <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")


# Wrangle -----------------------------------------------------------------
school_diversity_summary <- school_diversity %>% group_by(SCHOOL_YEAR, ST, LEAID) %>% 
  summarize(meanAIAN = (mean(AIAN/100*Total,na.rm=T))/sum(Total),
            meanAsian = (mean(Asian/100*Total,na.rm=T))/sum(Total),
            meanBlack = (mean(Black/100*Total,na.rm=T))/sum(Total),
            meanHispanic = (mean(Hispanic/100*Total,na.rm=T))/sum(Total),
            meanWhite = (mean(White/100*Total,na.rm=T))/sum(Total)) %>% 
  ungroup %>% group_by(SCHOOL_YEAR, ST) %>% 
  summarise(meanAIAN = mean(meanAIAN), 
            meanAsian = mean(meanAsian),
            meanBlack = mean(meanBlack),
            meanHispanic = mean(meanHispanic),
            meanWhite = mean(meanWhite))

SD_old <- school_diversity_summary %>% filter(SCHOOL_YEAR == "1994-1995")
SD_new <- school_diversity_summary %>% filter(SCHOOL_YEAR == "2016-2017")

colnames(SD_old) <- c(colnames(SD_old)[1:2], paste0(colnames(SD_old)[-(1:2)], "94-95"))
colnames(SD_new) <- c(colnames(SD_new)[1:2], paste0(colnames(SD_new)[-(1:2)], "16-17"))

SD <- left_join(SD_old[,-1], SD_new[,-1], by = "ST")


# Plot --------------------------------------------------------------------
font_add_google("Roboto", "roboto")
showtext_auto()
windows()
SD %>% mutate(Native = `meanAIAN16-17` - `meanAIAN94-95`,
              Asian = `meanAsian16-17` - `meanAsian94-95`,
              Black = `meanBlack16-17` - `meanBlack94-95`,
              Hispanic = `meanHispanic16-17` - `meanHispanic94-95`,
              White = `meanWhite16-17` - `meanWhite94-95`
) %>%
  select(ST, Native, Asian, Black, Hispanic, White) %>% 
  #filter(ST == "WA") %>% 
  pivot_longer(-ST, names_to = "Ethnicity", values_to = "Change") %>% 
  ggplot(aes(x = Ethnicity, y = Change, fill = Ethnicity)) + 
  geom_hline(yintercept = 0.1, color = "white") + geom_hline(yintercept = -0.1, color = "white") +
  geom_col() +
  theme_void() + 
  theme(axis.text.y = element_text(size = 8, family = "gaegu"), 
        plot.title = element_text(hjust = 0.3, margin = margin(t = 20, b = -35), family = "gaegu"),
        plot.subtitle = element_text(hjust = 0.3, margin = margin(t = 45, b = -35), family = "gaegu"),
        legend.position = c(0.35, 0.90), legend.title = element_blank(),
        legend.direction = "horizontal", 
        panel.background = element_rect(fill = "gray90",
                                        colour = "gray90",
                                        size = 0.5, linetype = "solid")) +
  labs(title = "Increasing racial diversity in schools", 
       subtitle = "Change in the percentage of students\nfrom different racial groups, 1994-2017.",
       caption = "Plot by @_Gil_Henriques for #TidyTuesday") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), breaks = c(-0.1, 0, 0.1)) +
  geom_hline(yintercept = 0) +
  facet_geo(~ ST, grid = "us_state_grid2") +
  scale_fill_brewer(palette = "Set1")


# Save --------------------------------------------------------------------
ggsave("racialdiversity.pdf")
