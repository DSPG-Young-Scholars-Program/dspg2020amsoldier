library(tidyverse)
library(tidytext)

# load data into environment
source(here::here("src", "load_data.R"));

## method 1
word_counts <- s32 %>% 
  unnest_tokens(word, text) %>%
  filter(word %in% get_sentiments("nrc")$word) %>%
  group_by(racial_group, response_type, word) %>%
  count() %>%
  arrange(desc(n))

black_words <- word_counts %>% filter(racial_group == "black")
white_words <- word_counts %>% filter(racial_group == "white")

# a - b
# list of words only used by Black soldiers and their associated frequency
unique_black_words <- anti_join(black_words, white_words, by = "word")
# b - a
# list of words only used by white soldiers and their associated frequency
unique_white_words <- anti_join(white_words, black_words, by = "word")

## method 2 : removing common words between groups

word_totals <- word_counts %>% 
  group_by(racial_group) %>% 
  summarize(sum = sum(n))

word_props <- word_counts %>% 
  inner_join(word_totals) %>% 
  mutate(prop = n / sum) %>% 
  arrange(desc(prop))