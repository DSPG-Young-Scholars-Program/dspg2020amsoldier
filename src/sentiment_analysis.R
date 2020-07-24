# sentiment analysis file
# load libraries
library(tidyverse)
library(tidytext)
library(tidyr)
library(here)

# load data
source(here::here("src", "examining_negation.R"));

# identical(s32, s32_negation_removed) # FALSE
# load sentiments
nrc_sentiments <- get_sentiments("nrc");
bing_sentiments <- get_sentiments("bing");
afinn_sentiments <- get_sentiments("afinn");

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

get_nrc_sentiments <- function(data) {
  # tokenize and join with nrc sentiment lexicon
  tokens <- data %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("nrc"));

  # compute sentiments
  sentiments <- tokens %>%
    group_by(index, racial_group, response_type, outfits, sentiment) %>%
    count() %>%
    spread(sentiment, n, fill = 0);
 
   # normalize sentiments
  sentiments <- sentiments %>%
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
  return(sentiments);
};

s32_sentiments <- get_nrc_sentiments(s32);
s32_negation_removed_sentiments <- get_nrc_sentiments(s32_negation_removed);
  

# sentiment analysis of responses related to gender
gender_words <- read.csv(here::here("data", "dictionary", "gender.csv"));head(gender_words) 
gender_match <- paste(paste("\\b", gender_words$gender,"\\b", sep = ""), collapse="|") # regex friendly

# filter s32 to repsonses that discuss gender
s32_gender <- s32 %>% 
  filter(grepl(gender_match, text));

s32_gender_sentiments <- get_nrc_sentiments(s32_gender);

# look only at female
female_words <- gender_words %>% filter(category == "female");
female_match <- paste(paste("\\b", female_words$gender, "\\b", sep = ""), collapse = "|")
s32_female <- s32 %>%
  filter(grepl(female_match, text));
s32_female_sentiments <- get_nrc_sentiments(s32_female);
nrow(s32_female_sentiments) # 162

# look only at male
male_words <- gender_words %>% filter(category == "male");
male_match <- paste(paste("\\b", male_words$gender, "\\b", sep = ""), collapse = "|")
s32_male <- s32 %>%
  filter(grepl(male_match, text));
s32_male_sentiments <- get_nrc_sentiments(s32_male);
nrow(s32_male_sentiments) # 1792

# look only at identity
# identity_words <- gender_words %>% filter(category == "identity");
# identity_match <- paste(paste("\\b", identity_words$gender, "\\b", sep = ""), collapse = "|")
# s32_identity <- s32 %>%
#   filter(grepl(identity_match, text));
# s32_identity_sentiments <- get_nrc_sentiments(s32_identity);
# nrow(s32_identity_sentiments) # 0

# look only at relation
relation_words <- gender_words %>% filter(category == "relation");
relation_match <- paste(paste("\\b", relation_words$gender, "\\b", sep = ""), collapse = "|")
s32_relation <- s32 %>%
  filter(grepl(relation_match, text));
s32_relation_sentiments <- get_nrc_sentiments(s32_relation);
nrow(s32_relation_sentiments) # 175

# look only at construct
# construct_words <- gender_words %>% filter(category == "construct");
# construct_match <- paste(paste("\\b", construct_words$gender, "\\b", sep = ""), collapse = "|")
# s32_construct <- s32 %>%
#   filter(grepl(construct_match, text));
# s32_construct_sentiments <- get_nrc_sentiments(s32_construct);
# nrow(s32_construct_sentiments) # 1 

# look at female and relation
female_and_relation_words <- gender_words %>% filter(category == "relation" | category == "female");
match <- paste(paste("\\b", female_and_relation_words$gender, "\\b", sep = ""), collapse = "|")
s32_female_and_relation <- s32 %>%
  filter(grepl(match, text));
s32_female_and_relation_sentiments <- get_nrc_sentiments(s32_female_and_relation);
nrow(s32_female_and_relation_sentiments) # 294


