library(sentimentr)
library(RPostgreSQL)
library(stringr)
library(tidytext)
library(dplyr)
library(purrr)
library(tidyr)
library(syn)
library(data.table)

source(here::here("src", "load_data.R"));
data <- copy(s32);
# tracemem(data) == tracemem(s32) # FALSE
# sum(str_detect(s32$text, neg_bigram_pattern)) # 3938

# fix contractions without apostrophes, might not need to do anymore
data$text <- str_replace_all(data$text, "cant", "can not")
data$text <- str_replace_all(data$text, "dont", "do not")
data$text <- str_replace_all(data$text, "couldnt", "could not")

# bigrams
# extracts all bigrams from data
bigrams <- data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

# counts bigrams by response type
bigrams_count <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negation_words <- c("not", "without", "no", "can't", "don't", "won't", "cant", "dont", "wont", 
                    "couldn't", "wouldn't", "couldnt", "wouldnt", "never", "hasn't", "hasnt", 
                    "haven't", "havent", "ain't", "aint")

# look only at bigrams where the first word is a negation word
neg_bigrams_count <- bigrams_count %>% 
  filter(word1 %in% negation_words)

# generate unique list of negated words
neg_words <- unique(neg_bigrams_count$word2)

neg_bigrams <- neg_bigrams_count %>%
  unite(bigram, word1, word2, sep = " ")

# removing negated bigrams from text
# create regex pattern from list of negated bigrams
neg_bigram_pattern <- paste(neg_bigrams$bigram, collapse = "|")
# use line below to check the number of repsonses that contain a negated bigram
# sum(str_detect(data$text, neg_bigram_pattern)) 

data$text <- str_replace_all(data$text, neg_bigram_pattern, "")
# use the line below, should be 0 to indicate that all negated bigrams have been removed
sum(str_detect(data$text, neg_bigram_pattern)) # 0
sum(str_detect(s32$text, neg_bigram_pattern)) # 3938

# save data 
s32_negation_removed <- data;

s32_negation_removed
# write.csv(data, "./data/data_neg_bigrams_removed.csv")


# LOOK into antonyms
# # load nrc sentiments
# nrc_sentiments <- get_sentiments("nrc")
# 
# # convert character vector to tibble
# neg_words <- tibble(word = neg_words)
# 
# # find words that have sentiments
# neg_words_sents <- inner_join(neg_words, nrc_sentiments)
# 
# # get list of unique words that have sentiments
# neg_words_to_save <- unique(neg_words_sents$word)
# # write.csv(neg_words_to_save, "./data/neg_words.csv")
# 
# # loop over words and get first antonym which has nrc sentiments
# # returns NA for words that don't have antonyms or don't have 
# # nrc sentiments for those antonyms
# res <- sapply(neg_words_to_save, function(x) {
#   word <- x;
#   # get antonyms of word
#   ants <- ant(word);
#   # determine which antonyms are in the sentiments library
#   idx <- match(TRUE, ants %in% nrc_sentiments$word);
#   if (!is.na(idx)) {
#     return(ants[idx]);
#   } else {
#     return(NA);
#   }
# });
# 
# # create tibble with original word and its antonym
# antonyms <- tibble(
#   word = neg_words_to_save,
#   antonym = res,
# );
# 
# # how many NAs are there in the antonyms tibble?
# sum(is.na(antonyms$antonym)) # 266


