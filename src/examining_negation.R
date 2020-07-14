library(sentimentr)
library(RPostgreSQL)
library(stringr)
library(tidytext)
library(dplyr)
library(purrr)
library(tidyr)

#load in your data as data tables
# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data
S32 <- dbGetQuery(conn, "SELECT *
                  FROM american_soldier.survey_32_clean")
# disconnect from postgresql
dbDisconnect(conn)

S32N <- S32 %>% filter(racial_group == "black")
S32W <- S32 %>% filter(racial_group == "white")

# text mining - mo --------------------------------------------------------
#T5 = long_comment, T3 = outfits_comment, T4 = long_comment
# this will create data frames out out of text
white_short <- tibble(row = 1:nrow(S32W), 
                      text = S32W$outfits_comment, 
                      response_type = rep("white_short", nrow(S32W))) # Written response to "should soldiers be in separate outfits?"
white_long <- tibble(row = 1:nrow(S32W), 
                     text = S32W$long, 
                     response_type = rep("white_long", nrow(S32W))) # Written response on overall thoughts on the survey
black_long <- tibble(row = 1:nrow(S32N),
                     text = S32N$long, 
                     response_type = rep("black_long", nrow(S32N))) # Written response to overall thoughts on survey
# combine into one
data <- bind_rows(black_long, white_long, white_short) 
data <- data %>% filter(!is.na(text))

# fix contractions without apostrophes
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
# sum(str_detect(data$text, neg_bigram_pattern)) 
# save data 
write.csv(data, "./data/s32_neg_bigrams_removed.csv")




