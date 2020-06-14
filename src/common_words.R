library(dplyr)
library(tidytext);library(textstem);library(SnowballC)
library(readxl)

S32W <- read_excel("~/git/Survey_32N and 32W consolidated.xlsx", sheet = 1, col_names = TRUE)
S32N <- read_excel("~/git/Survey_32N and 32W consolidated.xlsx", sheet = 2, col_names=TRUE)

## Using TidyText techniques whoooo

# put text into a dataframe. columns= row, text
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$T3) #Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$T4) #Written response on overall thoughts on the survey

#tokenization & word count | Removing stop words | NOT stemmed
word_count77_W <- text77_df %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

word_count78_W <- text78_df %>% unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

#do this in a function
#writing a function to do the same across all different dataset and questions (note, needs to be re-written for DB connection) 
word_count <- function(data, q){
  attach(data)
  text_df <- tibble(row = 1:nrow(data), text = q)
  word_counts <- text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
  detach(data)
  return(word_counts)
}
word_count77w <- word_count(S32W,T3)
word_count78w <- word_count(S32W,T4)
word_count78n <- word_count(S32N,T5)
head(word_count77w);head(word_count78w);head(word_count78n)




#--------STEMMING/LEMMATIZATION WORDS------------
#wordStem(word_count77_W$word)
# stemw77_W <- text77_df %>% unnest_tokens(word, text) %>% 
#   anti_join(stop_words) %>% mutate(word= wordStem(word)) %>%
#   count(word, sort = TRUE)
# 
# lemw77_W <- text77_df %>% unnest_tokens(word, text) %>% 
#   anti_join(stop_words) %>% mutate(word= textstem::lemmatize_words(word)) %>%
#   count(word, sort = TRUE)


word_count_stem <- function(data, q){
  attach(data)
  text_df <- tibble(row = 1:nrow(data), text = q)
  word_counts <- text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% mutate(word= wordStem(word)) %>%
    count(word, sort = TRUE)
  detach(data)
  return(word_counts)
}
word_counts77w <- word_count_stem(S32W,T3)
word_counts78w <- word_count_stem(S32W,T4)
word_counts78n <- word_count_stem(S32N,T5)
head(word_counts77w);head(word_counts78w);head(word_counts78n)


word_count_lem <- function(data, q){
  attach(data)
  text_df <- tibble(row = 1:nrow(data), text = q)
  word_counts <- text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% mutate(word= textstem::lemmatize_words(word)) %>%
    count(word, sort = TRUE)
  detach(data)
  return(word_counts)
}

word_countl77w <- word_count_lem(S32W,T3)
word_countl78w <- word_count_lem(S32W,T4)
word_countl78n <- word_count_lem(S32N,T5)
head(word_countl77w);head(word_countl78w);head(word_countl78n)
#------STEMMING/LEMMATIZATION SENTENCES--------------
stems77_W <- text77_df %>% mutate(text= textstem::stem_strings(text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

lems77_W <- text77_df %>% mutate(text= textstem::lemmatize_strings(text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)  %>%
  count(word, sort = TRUE)