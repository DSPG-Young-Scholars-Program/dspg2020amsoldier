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

# data ------------------------------------------------------------------

collapse <- fread("~/git/dspg2020amsoldier/data/dictionary/collapse_words.csv", sep = ",")
collapse <- mutate(collapse, original = paste("\\b", original,"\\b", sep = "")) #so that stringr doesn't pick up on instances where it is part of another word
#replace with collapsed words
source(here::here("src", "load_data.R"))

# collapse_union, collapse_union_1, collapse_union_2, collapse_union_3
data0 <- data
data1 <- data
data2 <- data
data3 <- data
# collapse_union, blackmal v blackfemal v white...
data0$long <- stri_replace_all_regex(data$long, collapse$original, collapse$collapse_union, vectorize_all = FALSE)
data0$outfits_comment <- stri_replace_all_regex(data$outfits_comment, collapse$original, collapse$collapse_union, vectorize_all = FALSE)
S32N <- filter(data0, racial_group == "black")
S32W <- filter(data0, racial_group == "white")
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$outfits_comment, outfits = S32W$outfits) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$long) #Written response on overall thoughts on the survey
textn_df <- tibble(row = 1:nrow(S32N), text = S32N$long)

# collapse_union_1, blackperson v whiteperson
data1$long <- stri_replace_all_regex(data$long, collapse$original, collapse$collapse_union_1, vectorize_all = FALSE)
data1$outfits_comment <- stri_replace_all_regex(data$outfits_comment, collapse$original, collapse$collapse_union_1, vectorize_all = FALSE)
S32N_1 <- filter(data1, racial_group == "black")
S32W_1 <- filter(data1, racial_group == "white")
text77_df_1 <- tibble(row = 1:nrow(S32W_1), text = S32W_1$outfits_comment, outfits = S32W_1$outfits) #Written response to "should soldiers be in separate outfits?"
text78_df_1 <- tibble(row = 1:nrow(S32W_1), text = S32W_1$long) #Written response on overall thoughts on the survey
textn_df_1 <- tibble(row = 1:nrow(S32N_1), text = S32N_1$long)

# collapse_union_2, black mal v blacksoldi, white...
data2$long <- stri_replace_all_regex(data$long, collapse$original, collapse$collapse_union_2, vectorize_all = FALSE)
data2$outfits_comment <- stri_replace_all_regex(data$outfits_comment, collapse$original, collapse$collapse_union_2, vectorize_all = FALSE)
S32N_2 <- filter(data2, racial_group == "black")
S32W_2 <- filter(data2, racial_group == "white")
text77_df_2 <- tibble(row = 1:nrow(S32W_2), text = S32W_2$outfits_comment, outfits = S32W_2$outfits) #Written response to "should soldiers be in separate outfits?"
text78_df_2 <- tibble(row = 1:nrow(S32W_2), text = S32W_2$long) #Written response on overall thoughts on the survey
textn_df_2 <- tibble(row = 1:nrow(S32N_2), text = S32N_2$long)

# collapse_union_3, negroperson v colorperson v blackperson v whiteperson
data3$long <- stri_replace_all_regex(data$long, collapse$original, collapse$collapse_union_3, vectorize_all = FALSE)
data3$outfits_comment <- stri_replace_all_regex(data$outfits_comment, collapse$original, collapse$collapse_union_3, vectorize_all = FALSE)
S32N_3 <- filter(data3, racial_group == "black")
S32W_3 <- filter(data3, racial_group == "white")
text77_df_3 <- tibble(row = 1:nrow(S32W_3), text = S32W_3$outfits_comment, outfits = S32W_3$outfits) #Written response to "should soldiers be in separate outfits?"
text78_df_3 <- tibble(row = 1:nrow(S32W_3), text = S32W_3$long) #Written response on overall thoughts on the survey
textn_df_3 <- tibble(row = 1:nrow(S32N_3), text = S32N_3$long)

# combined co-occurrences ---------------------------------------------

union_graphs_black <- function(data1, term1, term2) {
  row_n_words <- data1 %>%
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

  graph <- word_cors_n %>%
    filter(item1 %in% c(term1, term2)) %>%
    group_by(item1) %>%
    filter(item2 != term1) %>%
    filter(item2 != term2) %>%
    filter(item2 != "negro") %>%
    filter(item2 != "white") %>%
    filter(item2 != "color") %>%
    filter(item2 != "south") %>%
    filter(item2 != "black") %>%
    filter(item2 != "southern") %>%
    filter(item2 != "north") %>%
    filter(item2 != "northern") %>%
    filter(item2 != "question") %>%
    filter(item2 != "answer") %>%
    top_n(6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    mutate(item1 = reorder(item1, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity", fill = "#E57200") +
    xlab("Co-Occurring Word") +
    facet_wrap(~ item1, scales = "free") +
    ggtitle(paste0("Co-Occurences with Terms for ", term1, " and ", term2," \nfrom Black Soldiers' Long Responses")) +
    coord_flip() +
    theme_minimal()

  graph
}

union_graphs_white <- function(data2, term1, term2){
  # white long response
  row_78_words <- data2 %>%
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

  graph <- word_cors_78 %>%
    filter(item1 %in% c(term1, term2)) %>%
    group_by(item1) %>%
    filter(item2 != term1) %>%
    filter(item2 != term2) %>%
    filter(item2 != "negro") %>%
    filter(item2 != "white") %>%
    filter(item2 != "black") %>%
    filter(item2 != "color") %>%
    filter(item2 != "south") %>%
    filter(item2 != "southern") %>%
    filter(item2 != "north") %>%
    filter(item2 != "northern") %>%
    filter(item2 != "question") %>%
    filter(item2 != "answer") %>%
    top_n(6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    mutate(item1 = reorder(item1, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity", fill = "#2C4F6B") +
    xlab("Co-Occurring Word") +
    facet_wrap(~ item1, scales = "free") +
    ggtitle(paste0("Co-Occurences with Terms for ", term1, " and ", term2," \nfrom White Soldiers' Long Responses")) +
    coord_flip() +
    theme_minimal()

  graph
}

# no underscore: blackmal, blackfemal, whitemal, whitefemal
# _1: blackperson (not really for white), whiteperson
# _2: blackperson, blacksoldi, whiteperson (doesn't work for white), whitesoldi (doesn't work)
# _3: person terms across color (not white), black (not for black or white), negro (not white), white

union_graphs_black(textn_df_1, "blackperson", "whiteperson")
union_graphs_white(text78_df_1, "blackperson", "whiteperson")

# set.seed(2016)
# word_cors_n %>%
#   filter(correlation > .2) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
#   geom_node_point(color = "#E57200", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   ggtitle("Co-Occurences of Words from Black Soldiers' Long Responses at the 15 percent Threshold") +
#   theme_void()

# set.seed(2016)
# word_cors_78 %>%
#   filter(correlation > .2) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
#   geom_node_point(color = "#E57200", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   ggtitle("Co-Occurences of Words from White Soldiers' Long Responses at the 15 percent Threshold") +
#   theme_void()

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

word_cors_w4 %>%
  filter(item1 %in% c("whitemal", "blackmal")) %>%
  group_by(item1) %>%
  filter(item2 != "blackmal") %>%
  filter(item2 != "whitemal") %>%
  filter(item2 != "negro") %>%
  filter(item2 != "white") %>%
  filter(item2 != "color") %>%
  filter(item2 != "south") %>%
  filter(item2 != "southern") %>%
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

word_cors_wag %>%
  filter(item1 %in% c("whitemal", "blackmal")) %>%
  group_by(item1) %>%
  # filter(item2 != "blackmal") %>%
  # filter(item2 != "whitemal") %>%
  # filter(item2 != "negro") %>%
  # filter(item2 != "white") %>%
  # filter(item2 != "color") %>%
  # filter(item2 != "south") %>%
  # filter(item2 != "southern") %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#E6CE3A") +
  xlab("Co-Occurring Word") +
  facet_wrap(~ item1, scales = "free") +
  ggtitle("Co-Occurences with 'Negro' and 'White' from Anti-segregation White Soldier's Short Comments") +
  coord_flip()  +
  theme_minimal()

set.seed(2016)
word_cors_wag %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
  geom_node_point(color = "#E57200", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  ggtitle("Co-Occurences of Words from Anti-Segregation White Soldiers' Short Responses at the 15 percent Threshold") +
  theme_void()

# white short response -----------------------------------------------------

union_graphs_short <- function(data, term1, term2){

row_77_words <- data %>%
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

graph <- word_cors_77 %>%
  filter(item1 %in% c(term1, term2)) %>%
  group_by(item1) %>%
  filter(item2 != term1) %>%
  filter(item2 != term2) %>%
  filter(item2 != "negro") %>%
  filter(item2 != "white") %>%
  filter(item2 != "black") %>%
  filter(item2 != "color") %>%
  filter(item2 != "south") %>%
  filter(item2 != "southern") %>%
  filter(item2 != "north") %>%
  filter(item2 != "northern") %>%
  filter(item2 != "question") %>%
  filter(item2 != "answer") %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  mutate(item1 = reorder(item1, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity", fill = "#E6CE3A") +
  xlab("Co-Occurring Word") +
  facet_wrap(~ item1, scales = "free") +
  ggtitle(paste0("Co-Occurences with ", term1, " and ", term2," \nfrom White Soldier's Short Comments")) +
  coord_flip()  +
  theme_minimal()

graph

}

union_graphs_short(text77_df_1, "blackperson", "whiteperson")

# set.seed(2016)
# word_cors_77 %>%
#   filter(correlation > .15) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE) +
#   geom_node_point(color = "#E57200", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   ggtitle("Co-Occurences of Words from White Soldiers' Short Responses at the 15 percent Threshold") +
#   theme_minimal()


# bar plots with differences in sets with unionized terms ----------------------------------------------------

data(stop_words)

word_counts <- s32 %>%
  filter(response_type == "long") %>% # we only need to look at long responses
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(racial_group, response_type, word) %>%
  count() %>%
  arrange(desc(n))


black_words <- word_counts %>% filter(racial_group == "black")
white_words <- word_counts %>% filter(racial_group == "white")
unique_black_words <- anti_join(black_words, white_words, by = "word")
unique_white_words <- anti_join(white_words, black_words, by = "word")

words_joined <- full_join(black_words, white_words, by = "word") %>%
  replace_na(replace = list(black_words = 0, white_words = 0))

words_diff <- words_joined %>%
  transmute(word, n.x, n.y) %>%
  rename(black = n.x, white = n.y) %>%
  mutate(diff = black - white) %>%
  drop_na()

top_diff <- words_diff %>%
  arrange(-diff) %>%
  group_by(word) %>%
  mutate(word = factor(word, levels = word))  %>%
  top_n(n = 10, wt = diff)

words_diff %>%
  as.data.frame(.) %>%
  arrange(desc(diff)) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(10, diff) %>%
  ggplot(., aes(x = word, y = diff)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frequency of Words in Soldiers' Long Responses",
       x = "Word",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_flip()

words_diff %>%
  as.data.frame(.) %>%
  arrange(desc(diff)) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(-10, diff) %>%
  ggplot(., aes(x = word, y = diff)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frequency of Words in Soldiers' Long Responses",
       x = "Word",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_flip()
