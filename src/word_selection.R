library(tidyverse)
library(tidytext)
library(dplyr)

# load data into environment
source(here::here("src", "load_data.R"));

## method 1: the venn diagram, look at words used uniquely by one racial group
get_word_counts <- function(a) {
  word_counts <- a %>%
    unnest_tokens(word, text) %>%
    filter(word %in% get_sentiments("nrc")$word) %>%
    group_by(word) %>%
    count() %>%
    arrange(desc(n))
};

get_unique_words <- function(a, b) {
  # get word counts for both groups
  a_word_counts <- get_word_counts(a);
  b_word_counts <- get_word_counts(b);
  
  # get unique words for a and b
  a_unique_words <- anti_join(a_word_counts, b_word_counts, by = "word");
  b_unique_words <- anti_join(b_word_counts, a_word_counts, by = "word");
  
  # create list object to return both sets
  unique_words <- list("a" = a_unique_words, "b" = b_unique_words)
  return(unique_words)
};

# get unique words for black and white long responses
black_long <- s32 %>% filter(racial_group == "black" & response_type == "long")
white_long <- s32 %>% filter(racial_group == "white" & response_type == "long")
long_unique_words_by_race <- get_unique_words(black_long, white_long)
unique_black_words <- long_unique_words_by_race$a
unique_white_words <- long_unique_words_by_race$b

# get unique words for integrationists/segregationists short responses
seg_short <- s32 %>% filter(racial_group == "white" &
                              response_type == "short" &
                              outfits == "['They should be in separate outfits']");
int_short <- s32 %>% filter(racial_group == "white" &
                              response_type == "short" &
                              outfits == "['They should be together in the same outfits']");
short_unique_words_by_outfit_opinion <- get_unique_words(seg_short, int_short)
unique_seg_words <- short_unique_words_by_outfit_opinion$a
unique_int_words <- short_unique_words_by_outfit_opinion$b

## method 2 : removing common words between groups
# get total number of words used by each racial group
# create widget where you change the threshold and it changes the radar chart

word_counts <- s32 %>% 
  filter(response_type == "long") %>% # we only need to look at long responses 
  unnest_tokens(word, text) %>%
  filter(word %in% get_sentiments("nrc")$word) %>% # filter to words that have sentiments, take out if not doing sentiment
  group_by(racial_group, response_type, word) %>%
  count() %>%
  arrange(desc(n))

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
  replace_na(replace = list(black_prop = 0, white_prop = 0))
# does it make more sense to fill NA with 1 or 0. 0 is the obvious choice since that actively reflects the 
# proportion of that word in that racial group but using 1 might allow us to highlight words that are only used in 1?
# not sure what is best

# create relative proportion variable that is the absolute value of difference between black prop and white prop
word_props_joined$signed_prop <- word_props_joined$black_prop - word_props_joined$white_prop
word_props_joined$diff_prop <- abs(word_props_joined$black_prop - word_props_joined$white_prop)
word_props_final <- word_props_joined %>% arrange(desc(diff_prop))
word_props_final$index <- 1:nrow(word_props_final)


## REMOVE LATER
get_average_sentiment_from_word_counts <- function(a) {
  # compute initial sentiments by joining word counts with NRC lexicon
  sentiments <- a %>%
    inner_join(get_sentiments("nrc"), by = "word") %>%
    group_by(sentiment) %>%
    summarize(count = sum(n))
  
  # normalize to the number of sentiments
  total_sentiment_count <- sum(sentiments$count)
  norm_sentiments <- sentiments %>%
    mutate(prop = count / total_sentiment_count) %>%
    select(-c("count")) %>%
    spread(sentiment, prop)
  
  # return normalized sentiments
  return(norm_sentiments)
};

# compute average sentiment for word counts for each group of interest
# in the format to create radar chart
unique_black_words_sentiments <- get_average_sentiment_from_word_counts(unique_black_words);
unique_white_words_sentiments <- get_average_sentiment_from_word_counts(unique_white_words);
unique_seg_words_sentiments <- get_average_sentiment_from_word_counts(unique_seg_words)
unique_int_words_sentiments <- get_average_sentiment_from_word_counts(unique_int_words)

