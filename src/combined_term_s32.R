# co-occurences with compounded people terms
# this inlcudes whitemen, whiteboy, negromen, negroboy, blackman, blackboy, etc.

# co-occurrences with combined terms ------------------------------------
library("stringi")
library(data.table)
library(tidyverse)
library(tidytext)
library(textstem)
library(SnowballC)
library(readxl)
library(rvest)
library(tm)
library(topicmodels)
library(tidyr)
library(textdata)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(widyr)
library(stringr)
library(networkD3)

collapse <- fread("~/git/dspg2020amsoldier/data/dictionary/collapse_words.csv", sep = ",")
collapse <- mutate(collapse, original = paste("\\b", original,"\\b", sep = "")) #so that stringr doesn't pick up on instances where it is part of another word
#replace with collapsed words
source(here::here("src", "load_data.R"))

data$long <- stri_replace_all_regex(data$long, collapse$original, collapse$collapse, vectorize_all = FALSE)
data$outfits_comment <- stri_replace_all_regex(data$outfits_comment, collapse$original, collapse$collapse, vectorize_all = FALSE)

S32N <- filter(data, racial_group == "black")
S32W <- filter(data, racial_group == "white")

text77_df <- tibble(row = 1:nrow(S32W), text = S32W$outfits_comment, outfits = S32W$outfits) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$long) #Written response on overall thoughts on the survey
textn_df <- tibble(row = 1:nrow(S32N), text = S32N$long)

useless_responses = c("none","None","0", "12","none.","[none]","noone","[blank]","gujfujuj", "None.", "I", NA)

# tidy_77 <- text77_df %>%
#   filter(!text %in% useless_responses) %>% #filtering out useless 1 word responses
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words) %>%
#   mutate(word = wordStem(word)) %>%
#   group_by(row) %>%
#   count(word, sort = T) %>%
#   mutate(response = "short", race = "white")
#
# tidy_78 <- text78_df %>%
#   filter(!text %in% useless_responses) %>% #filtering out useless 1 word responses
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words) %>%
#   mutate(word = wordStem(word)) %>%
#   group_by(row) %>%
#   count(word, sort = T) %>%
#   mutate(response = "long", race = "white")
#
# tidy_n <- textn_df %>%
#   filter(!text %in% useless_responses) %>% #filtering out useless 1 word responses
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words) %>%
#   mutate(word = wordStem(word)) %>%
#   group_by(row) %>%
#   count(word, sort = T) %>%
#   mutate(response = "long", race = "black")

# co-occurences
row_n_words <- textn_df %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word= textstem::lemmatize_words(word)) %>%
  mutate(word= wordStem(word))

# count words co-occuring within sections
word_pairs_n <- row_n_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors_n <- row_n_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(correlation > .1)
# write.csv(word_cors_n, "clean_black_long_edge_occur.csv")
#visualizes correlation network
# word_cors_n %>%
#   simpleNetwork(fontSize = 12, zoom =T)

# colorman, whiteman, coloredsoldi, negrosoldi, whitesoldi
word_cors_n %>%
  filter(item1 %in% c("coloredsoldi", "whiteman")) %>%
  group_by(item1) %>%
  filter(item2 != "negro") %>%
  filter(item2 != "white") %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#E57200") +
  xlab("Co-Occurring Word") +
  facet_wrap(~ item1, scales = "free") +
  ggtitle("Co-Occurences with 'coloredsoldi' and 'whiteman' from Black Soldiers' Long Responses") +
  coord_flip() +
  theme_minimal()

set.seed(2016)
word_cors_n %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "#E57200", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Co-Occurences of Words from Black Soldiers' Long Responses at the 15 percent Threshold") +
  theme_void()

# white long response
row_78_words <- text78_df %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word= textstem::lemmatize_words(word)) %>%
  mutate(word= wordStem(word))

word_pairs_78 <- row_78_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors_78 <- row_78_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(correlation > .1)
# write.csv(word_cors_78, "clean_white_long_edge_occur.csv")
#visualizes correlation network
# word_cors_78 %>%
#   simpleNetwork(fontSize = 12, zoom =T)

# whiteman,

word_cors_78 %>%
  filter(item1 %in% c("whiteman", "blacksoldi")) %>%
  group_by(item1) %>%
  filter(item2 != "negro") %>%
  filter(item2 != "white") %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#2C4F6B") +
  xlab("Co-Occurring Word") +
  facet_wrap(~ item1, scales = "free") +
  ggtitle("Co-Occurences with 'Negro' and 'White' from White Soldiers' Long Responses") +
  coord_flip() +
  theme_minimal()

set.seed(2016)
word_cors_78 %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "#E57200", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Co-Occurences of Words from White Soldiers' Long Responses at the 15 percent Threshold") +
  theme_void()

# for v against -------------------------------------------
# white for seg (w4)

row_w4_words <- text77_df %>%
  filter(outfits == "['They should be in separate outfits']") %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word= textstem::lemmatize_words(word)) %>%
  mutate(word= wordStem(word))

word_pairs_w4 <- row_w4_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors_w4 <- row_w4_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)  %>%
  filter(correlation > 0)
#visualizes correlation network
# word_cors_w4 %>%
#   simpleNetwork(fontSize = 12, zoom =T)

# whiteman, negrosoldi
word_cors_w4 %>%
  filter(item1 %in% c("whiteman", "negrosoldi")) %>%
  group_by(item1) %>%
  filter(item2 != "white") %>%
  filter(item2 != "negro") %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#0E879C") +
  xlab("Co-Occurring Word") +
  facet_wrap(~ item1, scales = "free") +
  ggtitle("Co-Occurences with 'Negro' and 'White' from Pro-segregation White Soldier's Short Comments") +
  coord_flip()  +
  theme_minimal()

set.seed(2016)
word_cors_w4 %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "#2C4F6B", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Co-Occurences of Words from White Soldiers' Pro-Segregation \nShort Responses at the 15 percent Threshold") +
  theme_void()

# white against segregation (wag)
row_wag_words <- text77_df %>%
  filter(outfits == "['They should be together in the same outfits']") %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word= textstem::lemmatize_words(word)) %>%
  mutate(word= wordStem(word))

word_pairs_wag <- row_wag_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors_wag <- row_wag_words %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(correlation > 0)
#visualizes correlation network
# word_cors_wag %>%
#   simpleNetwork(fontSize = 12, zoom =T)

# there arent any combos

# word_cors_wag %>%
#   filter(item1 %in% c("whiteman", "negrosoldi")) %>%
#   group_by(item1) %>%
#   filter(item2 != "white") %>%
#   filter(item2 != "negro") %>%
#   top_n(6) %>%
#   ungroup() %>%
#   mutate(item2 = reorder(item2, correlation)) %>%
#   mutate(item1 = reorder(item1, correlation)) %>%
#   ggplot(aes(item2, correlation)) +
#   geom_bar(stat = "identity", fill = "#E6CE3A") +
#   xlab("Co-Occurring Word") +
#   facet_wrap(~ item1, scales = "free") +
#   ggtitle("Co-Occurences with 'Negro' and 'White' from Anti-segregation White Soldier's Short Comments") +
#   coord_flip()  +
#   theme_minimal()

set.seed(2016)
word_cors_wag %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "#E57200", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Co-Occurences of Words from White Soldiers' Long Responses at the 15 percent Threshold") +
  theme_void()


# white short response

row_77_words <- text77_df %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word= textstem::lemmatize_words(word)) %>%
  mutate(word= wordStem(word))

word_pairs_77 <- row_77_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors_77 <- row_77_words %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort = TRUE)
# write.csv(word_cors_77, "clean_white_short_occur.csv")
#visualizes correlation network
# word_cors_77 %>%
#   simpleNetwork(fontSize = 12, zoom =T)

# whiteman, negrosoldi, whitesoldi, colorman
word_cors_77 %>%
  filter(item1 %in% c("whiteman", "negrosoldi")) %>%
  group_by(item1) %>%
  filter(item2 != "white") %>%
  filter(item2 != "negro") %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#E6CE3A") +
  xlab("Co-Occurring Word") +
  facet_wrap(~ item1, scales = "free") +
  ggtitle("Co-Occurences with 'Negro' and 'White' from White Soldier's Short Comments") +
  coord_flip()  +
  theme_minimal()

set.seed(2016)
word_cors_77 %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "#E57200", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Co-Occurences of Words from White Soldiers' Short Responses at the 15 percent Threshold") +
  theme_minimal()





