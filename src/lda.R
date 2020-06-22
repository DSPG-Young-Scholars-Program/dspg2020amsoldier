library(dplyr)
library(ggplot2)
library(data.table)
library(tidytext)
library(textstem)
library(SnowballC)
library(readxl)
library(rvest)
library(tidytext) #Using for stop words and sentiment dictionary
library(dplyr) #Speeding up commands with the pipe opeartor
library(tm) #Cleaning text and generating word frequencies/vectors
library(topicmodels)
library(ggplot2)

num_clusters <- 5

S32W <- as.data.table(read_excel("~/Downloads/Survey_32N and 32W consolidated.xlsx", sheet = 1, col_names = TRUE))
S32N <- as.data.table(read_excel("~/Downloads/Survey_32N and 32W consolidated.xlsx", sheet = 2, col_names=TRUE))

# text mining - mo --------------------------------------------------------
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$T3) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$T4) #Written response on overall thoughts on the survey
data(stop_words)

tidy_77 <- text77_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = T)

tidy_78 <- text78_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = T)

tidy_77 <- na.omit(tidy_77)
tidy_78 <- na.omit(tidy_78)

# create dtm -------------------------------------------------------------

# lda - mo ---------------------------------------------------------------