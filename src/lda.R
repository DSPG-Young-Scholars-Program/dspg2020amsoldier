library(dplyr)
library(ggplot2)
library(data.table)
library(tidytext)
library(textstem)
library(SnowballC)
library(readxl)
library(rvest)
library(tm)
library(topicmodels)





#load in your data as data tables
library("RPostgreSQL")
# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data
S32 <- dbGetQuery(conn, "SELECT *
                  FROM american_soldier.survey_32_combined")
# disconnect from postgresql
dbDisconnect(conn)

S32N = S32 %>% filter(racial_group == "black")
S32W = S32 %>% filter(racial_group == "white")

head(S32N)


# text mining - mo --------------------------------------------------------
#T5 = long_comment, T3 = outfits_comment, T4 = long_comment
# this will create data frames out out of text
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$outfits_comment) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$long_comment) #Written response on overall thoughts on the survey
textn_df <- tibble(row = 1:nrow(S32N), text = S32N$long_comment) #Written response to "should soldiers be in separate outfits?"

# laod in stop words: words without any true meaning
data(stop_words)

# this will take in data frames, separate by tokens, remove stop words, create counts,
# and create a vector for document that will help to create document term matrix, it will
# also stem the words

# I think we should include more stemming and cleaning. What I did was very basic
# compared to what mary completed

num_words77 <- text77_df %>%
  unnest_tokens(word, text) %>%
  count(row, sort = T)
summary(num_words77$n)
# looking at responses with only 1 word
text77_df %>%
  filter(row %in% num_words77$row[which(num_words77$n==1)]) %>%
  na.omit()%>%
  View()

num_words78 <- text78_df %>%
  unnest_tokens(word, text) %>%
  count(row, sort = T)
summary(num_words78$n)
# looking at responses with only 1 word
text78_df %>%
  filter(row %in% num_words78$row[which(num_words78$n==1)]) %>%
  na.omit()%>%
  View()

num_wordsn <- textn_df %>%
  unnest_tokens(word, text) %>%
  count(row, sort = T)
summary(num_wordsn$n)
# looking at responses with only 1 word
textn_df %>%
  filter(row %in% num_wordsn$row[which(num_wordsn$n==1)]) %>%
  na.omit()%>%
  View()

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
