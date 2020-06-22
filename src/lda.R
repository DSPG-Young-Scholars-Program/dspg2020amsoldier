library(dplyr)
library(ggplot2)
library(data.table)
library(tidytext)
library(textstem)
library(SnowballC)
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
head(S32N)
# text mining - mo --------------------------------------------------------

# this will create data frames out out of text
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$T3) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$T4) #Written response on overall thoughts on the survey
textn_df <- tibble(row = 1:nrow(S32N), text = S32N$T5) #Written response to "should soldiers be in separate outfits?"

# laod in stop words: words without any true meaning
data(stop_words)

# this will take in data frames, separate by tokens, remove stop words, create counts,
# and create a vector for document that will help to create document term matrix, it will
# also stem the words

# I think we should include more stemming and cleaning. What I did was very basic
# compared to what mary completed

n_words <- text77_df %>%
  unnest_tokens(word, text) %>%
  count(row, sort = T)
summary(n_words$n)

tidy_77 <- text77_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(row) %>%
  count(word, sort = T)

tidy_78 <- text78_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(row) %>%
  count(word, sort = T)

tidy_n <- textn_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(row) %>%
  count(word, sort = T)

# omit NAs that have somehow gotten in
tidy_77 <- na.omit(tidy_77)
tidy_78 <- na.omit(tidy_78)
tidy_n <- na.omit(tidy_n)

# dtm - mo ---------------------------------------------------------------

# cast_dtm creates dtm so that we can complete lda
# a document term matrix is one row per document, one column per word, 
# each value is a count

dtm_77 <- cast_dtm(tidy_77, term = word, document = row, value = n)
dtm_78 <- cast_dtm(tidy_78, term = word, document = row, value = n)
dtm_n <- cast_dtm(tidy_n, term = word, document = row, value = n)

# lda - mo ---------------------------------------------------------------
# LDA finds topics depending on the number of clusters you want
# number of clusters we want
num_clusters <- 5
lda_77 <- LDA(dtm_77, k = num_clusters, method = "VEM", control = NULL)
lda_78 <- LDA(dtm_78, k = num_clusters, method = "VEM", control = NULL)
lda_n <- LDA(dtm_n, k = num_clusters, method = "VEM", control = NULL)

# this will separate out topics and have a weighted probability
topics_77 <- tidy(lda_77, matrix = "beta")
topics_78 <- tidy(lda_78, matrix = "beta")
topics_n <- tidy(lda_n, matrix = "beta")


# this groups by topics and shows top 10 words and arranges by beta
# Q77 white
topics_terms_77 <- topics_77 %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# this graphs and reorders so you can visualize
topics_terms_77 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# S32 Q78 white
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

# S32 black
topics_terms_n <- topics_n %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topics_terms_n %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# euclidean distances to compare white and black - mo -------------------------
# comapre with 78 and n
exposure_78 <- posterior(lda_78,dtm_78)
apply(exposure_78$topics,1,sum)
exposure_n <- posterior(lda_n,dtm_n)
apply(exposure_n $topics,1,sum)

euclidean_distances <- c()
max_exposure <- matrix(F,nrow(exposure_n$topics),num_clusters)
for(i in 1:nrow(exposure_n$topics)){
  euclidean_distances[i] <- sqrt(sum((exposure_n$topics[i,] - exposure_78$topics[i,])^2))
  # which text was exposed to (full v summary)
  max_exposure[i,which.max(exposure_n$topics[i,])] <- T
  max_exposure[i,which.max(exposure_78$topics[i,])] <- T
}
print(sum(apply(max_exposure,1,sum) == 1)/nrow(exposure_n$topics))
# 0.1220998 - what does this mean though? is this close or far?

# sentiment analysis - mo -------------------------

# naming categories (the hard way) - mo ------------------------------------------------------
