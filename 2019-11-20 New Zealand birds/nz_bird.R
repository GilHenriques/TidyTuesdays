# 2019-11-19 TidyTuesday
# Gil J.B. Henriques

library(tidyverse)


# Read data ---------------------------------------------------------------
nz_bird <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv") %>% 
  groupdata2::group(n = 5, method = "greedy") %>%  # add voter id
  select(vote_rank, bird_breed) %>% 
  separate(vote_rank, c(NA, "rank"))

# Simulate voting process and save each round's results -------------------
election <- tibble(bird_breed = NA, percent = NA, round = NA)
majority <- FALSE; round <- 1;
while(!majority){
  round_df <- nz_bird %>% 
    group_by(.groups) %>% 
    filter(rank == min(rank)) %>% # count each voter's highest preference 
    ungroup %>% 
    group_by(bird_breed) %>% 
    summarize(sum = n()) %>% 
    ungroup %>% 
    mutate(percent = sum/sum(sum), round = round) %>% 
    select(bird_breed, percent, round)
  
  election <- bind_rows(election, round_df)
  
  majority <- (round_df %>% pull(percent) %>% max) >= 0.5
  
  if(!majority) { # if no majority, remove lowest voted bird
    bird_to_remove <- round_df %>% filter(percent == min(percent)) %>% pull(bird_breed)
    
    nz_bird <- nz_bird %>% filter(!(bird_breed %in% bird_to_remove))
  }
  
  round <- round + 1
  print(round) # to keep track of progress
}


# Plot --------------------------------------------------------------------

# Since I can't use nudge_x together with vjust in geom_text, I need to "fake" nudging by
# adding blank spaces at the end of the labels
election <- election %>% 
  mutate(bird_space = paste0(bird_breed, "  ")) 

# I will highlight top candidates
top_birds <- election %>% filter(round > 75) %>% pull(bird_breed) %>%  unique()

election %>% 
  ggplot(aes(x = round, y = percent, fill = bird_breed)) +
  # Most species will appear in grey
  geom_col(position = "stack", color = "white", fill = "gray95", size = 0.1) + 
  # Top species will be colorful
  geom_col(data = filter(election, bird_breed %in% top_birds), position = "stack", color = "white", size = 0.1) +
  geom_segment(x = 0, xend = 82, y = 0.5, yend = 0.5, lty = 2, inherit.aes = F) + # Add a line at 50%
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") + # Color is needed for text labels
  theme_minimal() +
  theme(legend.position = "none", legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(margin = margin(r = 0))) +
  scale_y_continuous(labels = scales::percent, position = "right",
                     expand = expand_scale(add = c(0.001, 0.05))) +
  xlab("Instant runoff round") + ylab("Percent of vote") +
  geom_text(data = filter(election, bird_breed %in% top_birds & round == 1), aes(label = bird_space, color = bird_breed), position = position_stack(vjust = 0.5), hjust = 1, size = 3.5) +
  annotate(geom="text",x = 1, y = 0.65, label = "Other species  ", hjust = 1, size = 3.5, color = "grey") +
  labs(title = "The Yellow-eyed penguin is New Zealand's 2019 Bird of the Year", 
       subtitle = "Voting was based on the instant runoff system: the first preferences of all the votes cast are tallied in a\nfirst round of counting. If no bird has more than half of the votes, new rounds of counting are held until one\nbird has a majority. Columns in the figure show species vote shares per round.", 
       caption = "Visualization: @_Gil_Henriques for #TidyTuesday. Data: New Zealand Forest & Bird.") +
  scale_x_discrete(expand = expand_scale(add = c(20,0)), breaks = seq(0,80,20), labels = seq(0,80,20), limits = seq(0,80,20))

# Save plot
ggsave("nz_birds.pdf", width = 7.5, height = 5)  
