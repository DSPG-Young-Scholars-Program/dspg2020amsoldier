library(dplyr)
library(ggplot2)
library(data.table)
library(tidytext)
library(textstem)
library(SnowballC)
library(Snowball)
library(readxl)
library(rvest)
library(tidytext)
library(dplyr)
library(tm)
library(topicmodels)
library(ggplot2)

#load in your data as data tables
S32W <- as.data.table(read_excel("~/Downloads/Survey_32N and 32W consolidated.xlsx", sheet = 1, col_names = TRUE))
S32N <- as.data.table(read_excel("~/Downloads/Survey_32N and 32W consolidated.xlsx", sheet = 2, col_names=TRUE))

# text mining - mo --------------------------------------------------------

# this will create data frames out out of text
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$T3) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$T4) #Written response on overall thoughts on the survey

# laod in stop words: words without any true meaning
data(stop_words)

# this will take in data frames, separate by tokens, remove stop words, create counts,
# and create a vector for document that will help to create document term matrix, it will
# also stem the words

tidy_77 <- text77_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  unique() %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort = T) %>%
  mutate(document = 1)

tidy_78 <- text78_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  unique() %>%
  mutate(word = wordStem(word)) %>%
  count(word, sort = T) %>%
  mutate(document = 1)

# omit NAs that have somehow gotten in
tidy_77 <- na.omit(tidy_77)
tidy_78 <- na.omit(tidy_78)

# create dtm -------------------------------------------------------------

# cast_dtm creates dtm so that we can complete lda
# a document term matrix is one row per document, one column per word, 
# each value is a count

dtm_77 <- cast_dtm(tidy_77, term = word, document = document, value = n)
dtm_78 <- cast_dtm(tidy_78, term = word, document = document, value = n)

# lda - mo ---------------------------------------------------------------
# LDA finds topics depending on the number of clusters you want
# number of clusters we want
num_clusters <- 2
lda_77 <- LDA(dtm_77, k = num_clusters, method = "VEM", control = NULL)
lda_78 <- LDA(dtm_78, k = num_clusters, method = "VEM", control = NULL)

# this will separate out topics and have a weighted probability
topics_77 <- tidy(lda_77, matrix = "beta")
topics_78 <- tidy(lda_78, matrix = "beta")

# this groups by topics and shows top 10 words and arranges by beta
topics_terms_77 <- topics_77 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# this graphs so you can visualize
topics_terms_77 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

topics_terms_78 <- topics_78 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topics_terms_78 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
