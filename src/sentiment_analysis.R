# sentiment analysis file
# load libraries
library(tidytext)

nrc_sentiments <- get_sentiments("nrc")

# differences in sentiments between race
nrc_sentiments %>% filter(word == "black")
nrc_sentiments %>% filter(word == "white")
nrc_sentiments %>% filter(word == "negro")

# load data with negated bigrams removed for sentiment analysis
data <- read.csv("./data/s32_neg_bigrams_removed.csv") %>% subset(select = -c(X));
data$index = 1:nrow(data)

# remove white, black, negro from text
data$text <- str_replace_all(data$text, "white", "")
data$text <- str_replace_all(data$text, "black", "")
data$text <- str_replace_all(data$text, "negro", "")


# compute sentiments for each response
tmp <- data %>% 
  unnest_tokens(word, text) %>%
  inner_join(nrc_sentiments)

sentiments <- tmp %>% 
  group_by(index, response_type, sentiment) %>% 
  count() %>%
  spread(sentiment, n, fill = 0)

# normalize sentiments by the number of words contributing to that sentiment
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
            trust = trust / word_count)





