library(tidyverse)
library(ggraph)
library(tidygraph)
library(ggtext)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()


ttdata <- tidytuesdayR::tt_load(2020, week = 27)
df <- ttdata$characters

df[df == "Storm x2* Cyclops*"] <- "Storm, Cyclops"

df_short <- df %>% 
  mutate(on_a_date_with_which_character = ifelse(on_a_date_with_which_character == "1", NA, on_a_date_with_which_character)) %>% 
  select(character, kiss_with_which_character, explicitly_states_i_love_you_to_whom, on_a_date_with_which_character) %>% 
  separate_rows(kiss_with_which_character, sep = ", ") %>% 
  separate_rows(explicitly_states_i_love_you_to_whom, sep = ", ") %>% 
  mutate(character = str_remove_all(character, " =[^.]*$"),
         kiss_with_which_character = str_remove_all(kiss_with_which_character, "[[:punct:]]"), 
         kiss_with_which_character = str_remove_all(kiss_with_which_character, " x[2,3]"),
         on_a_date_with_which_character = str_remove_all(on_a_date_with_which_character, "[[:punct:]]"),
         explicitly_states_i_love_you_to_whom = str_remove_all(explicitly_states_i_love_you_to_whom, "[[:punct:]]"),
         explicitly_states_i_love_you_to_whom = str_remove_all(explicitly_states_i_love_you_to_whom, " x[2,3,4]")
         ) %>% 
  filter(is.na(kiss_with_which_character) + is.na(on_a_date_with_which_character) + is.na(explicitly_states_i_love_you_to_whom) < 3)

# Manually fix input errors
df_short[df_short == "Marvel Girl/Phoenix"] <- "Phoenix"
df_short[df_short == "Phoenix(2)"] <- "Phoenix"
df_short[df_short == "Jean Grey"] <- "Phoenix"

df_short[df_short == "Lorna Dane"] <- "Polaris"
df_short[df_short == "Lorna DanePolaris"] <- "Polaris"

df_short[df_short == "Moira MacTaggert (scientist helper)"] <- "Moira MacTaggert"
df_short[df_short == "Moira MacTaggart"] <- "Moira MacTaggert"
df_short[df_short == "Moira Mactaggart"] <- "Moira MacTaggert"
df_short[df_short == "Moira McTaggart"] <- "Moira MacTaggert"
df_short[df_short == "Moia MacTaggart"] <- "Moira MacTaggert"
df_short[df_short == "Joseph MacTaggart"] <- "Joseph MacTaggert"

df_short[df_short == "Ariel/Sprite/Shadowcat"] <- "Kitty Pryde"
df_short[df_short == "Kitty Pryde Future Self"] <- "Kitty Pryde"
df_short[df_short == "Kitty Pyde"] <- "Kitty Pryde"

df_short[df_short == "Madelyn Pryor"] <- "Madelyne Pryor"
df_short[df_short == "Madelyne  Pryor"] <- "Madelyne Pryor"

df_short[df_short == "Princess Lilandra"] <- "Lilandra"

df_short[df_short == "Cylops"] <- "Cyclops"
df_short[df_short == "With local women in the savage land"] <- "Savage land women"

characters <- unlist(df_short, use.names = FALSE)[!is.na(unlist(df_short, use.names = FALSE))] %>% unique()
unique(characters)

`%nin%` <- Negate(`%in%`) # not in operator

unnamed_characters <- c("Savage land women", "Unnamed Alien",
                        "Unnamed Alien Medic", "Unnamed human figure")

colnames(df_short) <- c("character", "Kisses", "Says 'I love you'", "Goes on a date")

# Problem: A:B and B:A should be the same
# Pastes together two columns, such that A:B and B:A both end up in same order
paste_cols = function(x, y, sep = ":"){
  stopifnot(length(x) == length(y))
  return(lapply(1:length(x), function(i) {paste0(sort(c(x[i], y[i])), collapse = ":")}) %>% unlist())
}

df_short[df_short == "Naźe"] <- "Naze"

set.seed(2); df_short %>% 
  pivot_longer(-character, names_to = "type", values_to = "to") %>% 
  filter(character %nin% unnamed_characters) %>%  # remove unnamed characters
  filter(to %nin% unnamed_characters) %>%
  rename(from = "character") %>% 
  distinct() %>% 
  drop_na() %>% 
  mutate(col_pairs = paste_cols(from, to, sep = ":")) %>% 
  distinct(col_pairs, type, .keep_all = TRUE) %>% 
  as_tbl_graph(directed = FALSE) %>% 
  ggraph(layout = "fr") +
  geom_edge_fan(aes(color = type), width = 0.8, strength = 1.5, alpha = 0.8, show.legend = FALSE) +
  geom_node_text(aes(label = name), repel = TRUE, force = 5, segment.color = "white") +
  geom_node_point() +
  labs(subtitle = "Arrows connect characters who went on a <span style = 'color:#A95AA1;'>**date**</span> together, <span style = 'color:#85CBF9;'>**kissed**</span>, or when at least one of them explicitly<br>said <span style = 'color:#F5793A;'>**'I love you'**</span>, during Chris Claremont's 16-year run on *Uncanny X-Men* #97-278",
       title = "**Expressions on love in Chris Claremont's** ***Uncanny X-Men***",
       caption = "**Data:** Claremont Run Project / **Visualization:** @_Gil_Henriques for #TidyTuesday") +
  theme(panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        plot.subtitle = element_markdown(family = "roboto"),
        plot.title = element_markdown(family = "roboto"),
        plot.caption = element_markdown(family = "roboto")) +
  scale_edge_colour_manual(values = c("#A95AA1", "#85CBF9", "#F5793A")) 

ggsave("2020-07-01 X-Men/xmen_relationships.pdf", width = 7.5, height = 6)  
ggsave("2020-07-01 X-Men/xmen_relationships.png", width = 7.5, height = 6)  

