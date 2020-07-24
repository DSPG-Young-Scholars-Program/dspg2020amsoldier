library(tidyverse)
library(tidytext)

# load data into environment
source(here::here("src", "load_data.R"));

## method 1: the venn diagram, look at words used uniquely by one racial group
word_counts <- s32 %>% 
  filter(response_type == "long") %>% # we only need to look at long responses 
  unnest_tokens(word, text) %>%
  filter(word %in% get_sentiments("nrc")$word) %>% # filter to words that have sentiments, take out if not doing sentiment
  group_by(racial_group, response_type, word) %>%
  count() %>%
  arrange(desc(n))

# words used by Black soldiers
black_words <- word_counts %>% filter(racial_group == "black") 

# words used by white soldiers
white_words <- word_counts %>% filter(racial_group == "white")

# A - B
# list of words only used by Black soldiers and their associated frequency
unique_black_words <- anti_join(black_words, white_words, by = "word")

# B - A
# list of words only used by white soldiers and their associated frequency
unique_white_words <- anti_join(white_words, black_words, by = "word")

## method 2 : removing common words between groups
# get total number of words used by each racial group
word_totals <- word_counts %>% 
  group_by(racial_group) %>% 
  summarize(sum = sum(n))
# compute prop column that shows the proportion a word is used
word_props <- word_counts %>% 
  inner_join(word_totals) %>% 
  mutate(prop = n / sum) %>% 
  arrange(desc(prop))

black_props <- word_props %>% 
  ungroup() %>%
  filter(racial_group == "black") %>% 
  rename(black_prop = prop) %>% 
  select(c("word", "black_prop"))

white_props <- word_props %>%
  ungroup() %>%
  filter(racial_group == "white") %>%
  rename(white_prop = prop) %>%
  select(c("word", "white_prop"))

word_props_joined <- full_join(black_props, white_props, by = "word") %>%
  replace_na(replace = list(black_props = 0, white_props = 0))
# does it make more sense to fill NA with 1 or 0. 0 is the obvious choice since that actively reflects the 
# proportion of that word in that racial group but using 1 might allow us to highlight words that are only used in 1?
# not sure what is best

# create relative proportion variable that is the absolute value of difference between black prop and white prop
word_props_joined$rel_prop <- abs(word_props_joined$black_prop - word_props_joined$white_prop)
word_props_final <- word_props_joined %>% arrange(desc(rel_prop))
View(word_props_final)