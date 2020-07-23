# sentiment analysis file
# load libraries
library(tidytext)
library(tidyr)
library(here)

# load data
source(here::here("src", "load_data.R"));
source(here::here("src", "examining_negation.R"));

# identical(s32, s32_negation_removed) # FALSE
# load sentiments
nrc_sentiments <- get_sentiments("nrc");

remove_words <- function(text, words) {
  pattern <- paste(words, collapse = "|");
  text <- str_replace_all(text, pattern, "");
  return(text);
}

# load data with negated bigrams removed for sentiment analysis
# data$index = 1:nrow(data) # need to change

# remove white, black, negro from text
words <- c("white", "black", "negro");

s32$text <- remove_words(s32$text, words);
s32_negation_removed$text <- remove_words(s32_negation_removed$text, words);


## compute sentiments
tmp <- s32 %>% 
  unnest_tokens(word, text) %>%
  inner_join(nrc_sentiments);

s32_sentiments <- tmp %>% 
  group_by(index, racial_group, response_type, outfits, sentiment) %>% 
  count() %>%
  spread(sentiment, n, fill = 0);

# normalize sentiments by the number of words contributing to that sentiment
s32_sentiments <- s32_sentiments %>% 
  mutate(word_count = anger + anticipation + disgust + fear + joy + negative + positive + sadness + surprise + trust) %>%
  filter(word_count > 0) %>%
  mutate(anger = anger / word_count,
            anticipation = anticipation / word_count,
            disgust = disgust / word_count,
            fear = fear / word_count,
            joy = joy / word_count,
            negative = negative / word_count,
            positive = positive / word_count,
            sadness = sadness / word_count,
            surprise = surprise / word_count,
            trust = trust / word_count);

## compute sentiments with negated bigrams removed
tmp <- s32_negation_removed %>% 
  unnest_tokens(word, text) %>%
  inner_join(nrc_sentiments);

s32_negation_removed_sentiments <- tmp %>% 
  group_by(index, racial_group, response_type, outfits, sentiment) %>% 
  count() %>%
  spread(sentiment, n, fill = 0);

# normalize sentiments by the number of words contributing to that sentiment
s32_negation_removed_sentiments <- s32_negation_removed_sentiments %>% 
  mutate(word_count = anger + anticipation + disgust + fear + joy + negative + positive + sadness + surprise + trust) %>%
  filter(word_count > 0) %>%
  mutate(anger = anger / word_count,
         anticipation = anticipation / word_count,
         disgust = disgust / word_count,
         fear = fear / word_count,
         joy = joy / word_count,
         negative = negative / word_count,
         positive = positive / word_count,
         sadness = sadness / word_count,
         surprise = surprise / word_count,
         trust = trust / word_count);







