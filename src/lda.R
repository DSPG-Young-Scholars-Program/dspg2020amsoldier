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
library(tidyr)
library(textdata)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(widyr)


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
# looking at responses with only 1 word
# Bunch of useless one word responses
useless_responses = c("none","None","0", "12","none.","[none]","noone","[blank]","gujfujuj", "None.", "I", NA)

# num_words77 <- text77_df %>%
#   unnest_tokens(word, text) %>%
#   count(row, sort = T)
# summary(num_words77$n)
# text77_df %>%
#   filter(row %in% num_words77$row[which(num_words77$n==1)]) %>%
#   filter(!text %in% useless_responses) %>%
#   View()

# num_words78 <- text78_df %>%
#   unnest_tokens(word, text) %>%
#   count(row, sort = T)
# summary(num_words78$n)
# text78_df %>%
#   filter(row %in% num_words78$row[which(num_words78$n==1)]) %>%
#   filter(!text %in% useless_responses) %>%
#   View()
#
# num_wordsn <- textn_df %>%
#   unnest_tokens(word, text) %>%
#   count(row, sort = T)
# summary(num_wordsn$n)
# textn_df %>%
#   filter(row %in% num_wordsn$row[which(num_wordsn$n==1)]) %>%
#   filter(!text %in% useless_responses) %>%
#   View()

tidy_77 <- text77_df %>%
  filter(!text %in% useless_responses) %>% #filtering out useless 1 word responses
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(row) %>%
  count(word, sort = T)

tidy_78 <- text78_df %>%
  filter(!text %in% useless_responses) %>% #filtering out useless 1 word responses
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(row) %>%
  count(word, sort = T)

tidy_n <- textn_df %>%
  filter(!text %in% useless_responses) %>% #filtering out useless 1 word responses
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(row) %>%
  count(word, sort = T)


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
  # which text was exposed to (black v white)
  max_exposure[i,which.max(exposure_n$topics[i,])] <- T
  max_exposure[i,which.max(exposure_78$topics[i,])] <- T
}
print(sum(apply(max_exposure,1,sum) == 1)/nrow(exposure_n$topics))
# 0.1157193 - what does this mean though: distance of topics between both groups

# naming categories (the hard way) - mo ------------------------------------------------------
# here, soon, will lie code for naming categories without us having to name them

# sentiment analysis by word - mo -------------------------

nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

nrc_n <- tidy_n %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE)

bing_n <- tidy_n %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

afinn_n <- tidy_n %>%
  inner_join(afinn) %>%
  count(word, value, sort = TRUE) %>%
  group_by(word) %>%
  summarise(sentiment = sum(value), row) %>%
  mutate(method = "AFINN")


nrc_77 <- tidy_77 %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE)

bing_77 <- tidy_77 %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

afinn_77 <- tidy_77 %>%
  inner_join(afinn) %>%
  count(word, value, sort = TRUE) %>%
  group_by(word) %>%
  summarise(sentiment = sum(value), row) %>%
  mutate(method = "AFINN")

nrc_78 <- tidy_78 %>%
  inner_join(nrc) %>%
  count(word, sentiment, sort = TRUE)

bing_78 <- tidy_78 %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

afinn_78 <- tidy_78 %>%
  inner_join(afinn) %>%
  count(word, value, sort = TRUE) %>%
  group_by(word) %>%
  summarise(sentiment = sum(value), row) %>%
  mutate(method = "AFINN")

# differences in sentiments -------------------------------------------------------

# black - long response
bing_and_nrc <- bind_rows(tidy_n %>%
                            inner_join(bing) %>%
                            mutate(method = "Bing et al."),
                          tidy_n %>%
                            inner_join(nrc) %>%
                            mutate(method = "NRC")) %>%
  count(row, method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn_n,
          bing_and_nrc) %>%
  ggplot(aes(row, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# white - short response
bing_and_nrc_77 <- bind_rows(tidy_77 %>%
                               inner_join(bing) %>%
                               mutate(method = "Bing et al."),
                             tidy_77 %>%
                               inner_join(nrc) %>%
                               mutate(method = "NRC")) %>%
  count(row, method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn_77,
          bing_and_nrc_77) %>%
  ggplot(aes(row, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# white - long response
bing_and_nrc_78 <- bind_rows(tidy_78 %>%
                               inner_join(bing) %>%
                               mutate(method = "Bing et al."),
                             tidy_78 %>%
                               inner_join(nrc) %>%
                               mutate(method = "NRC")) %>%
  count(row, method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn_78,
          bing_and_nrc_78) %>%
  ggplot(aes(row, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# sentiment words counts -------------------------------------------------------
custom_stop_words <- bind_rows(tibble(word = c("unclear", "underline"),
                                      lexicon = c("custom")),
                               stop_words)

# these graphs fully just show whatever number of words they want as oppsed to 10 because ties
bing_counts_n <- tidy_n %>%
  inner_join(bing) %>%
  anti_join(custom_stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts_n %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

bing_counts_77 <- tidy_77 %>%
  inner_join(bing) %>%
  anti_join(custom_stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts_77 %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

bing_counts_78 <- tidy_78 %>%
  inner_join(bing) %>%
  anti_join(custom_stop_words) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_counts_78 %>%
  group_by(sentiment) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# negations ----------------------------------------------------------

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated_n %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated_n %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()


# word clouds ----------------------------------------------------------------------

bing_counts_n %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

bing_counts_77 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

bing_counts_78 %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# bigrams -------------------------------------------------------------------------

tidy_n_bigrams <- textn_df %>%
  filter(!text %in% useless_responses) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

bigrams_separated_n <- tidy_n_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_n <- bigrams_separated_n %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts_n <- bigrams_filtered_n %>%
  count(word1, word2, sort = TRUE)

bigrams_united_n <- bigrams_filtered_n %>%
  unite(bigram, word1, word2, sep = " ")

tidy_n_trigrams <- textn_df %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

bigram_n_tf_idf <- bigrams_united_n %>%
  count(bigram) %>%
  bind_tf_idf(bigram, n) %>%
  arrange(desc(tf_idf))

bigram_graph_n <- bigram_counts_n %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph_n, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# negation bigrams -------------------------------------------------------------------------
negation_bigrams <- bigrams_separated_n %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  slice(seq_len(20)) %>%
  arrange(word1,desc(contribution)) %>%
  ungroup()

bigram_graph <- negation_bigrams %>%
  graph_from_data_frame() #From `igraph`

set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = .25) +
  geom_edge_density(aes(fill = value)) +
  geom_node_point(color = "purple1", size = 1) + #Purple for Prince!
  geom_node_text(aes(label = name),  repel = TRUE) +
  theme_void() + theme(legend.position = "none",
                       plot.title = element_text(hjust = 0.5)) +
  ggtitle("Negation Bigram Network")

# co-occurences -------------------------------------------------------------------

row_n_words <- textn_df %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)


# count words co-occuring within sections
word_pairs_n <- row_n_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors_n <- row_n_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors_n %>%
  filter(item1 %in% c("negro", "white")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)
word_cors_n %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

