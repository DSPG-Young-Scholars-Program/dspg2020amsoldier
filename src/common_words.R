library(dplyr);library(ggplot2);library(data.table)
library(tidytext);library(textstem);library(SnowballC)
library(readxl)

S32W <- as.data.table(read_excel("~/git/Survey_32N and 32W consolidated.xlsx", sheet = 1, col_names = TRUE))
S32N <- as.data.table(read_excel("~/git/Survey_32N and 32W consolidated.xlsx", sheet = 2, col_names=TRUE))

library(dplyr);library(ggplot2)
library(tidytext);library(textstem);library(SnowballC)
library(readxl)

S32W <- read_excel("~/git/Survey_32N and 32W consolidated.xlsx", sheet = 1, col_names = TRUE)
S32N <- read_excel("~/git/Survey_32N and 32W consolidated.xlsx", sheet = 2, col_names=TRUE)

## Using TidyText techniques whoooo

# put text into a dataframe. columns= row, text
# text77_df <- tibble(row = 1:nrow(S32W), text = S32W$T3) #Written response to "should soldiers be in separate outfits?"
# text78_df <- tibble(row = 1:nrow(S32W), text = S32W$T4) #Written response on overall thoughts on the survey

#----------tokenization & word count | Removing stop words | NOT stemmed------------------
# word_count77_W <- text77_df %>% unnest_tokens(word, text) %>% 
#   anti_join(stop_words) %>%
#   count(word, sort = TRUE)
# 
# word_count78_W <- text78_df %>% unnest_tokens(word, text) %>% 
#   anti_join(stop_words) %>%
#   count(word, sort = TRUE)

#do this in a function
#writing a function to do the same across all different dataset and questions (note, needs to be re-written for DB connection) 
word_count <- function(data, q){
  attach(data)

  text_df <- tibble(data, row = 1:nrow(data), text = q)
  #tokenize and count words
  word_counts <- as.data.table(text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    count(word, sort = TRUE))
  #divide metadata pairs by two
  word_counts[word=="unclear" | word=="deletion" | word=="insertion" | word == "circle" | word=="underline", n := round(n/2)]
  #reorder by descending
  word_counts <- word_counts[order(-n)]

  text_df <- tibble(row = 1:nrow(data), text = q)
  word_counts <- text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    count(word, sort = TRUE)

  detach(data)
  return(word_counts)
}
word_count63w <- word_count(S32W,T3)
word_count78w <- word_count(S32W,T4)
word_count78n <- word_count(S32N,T5)
head(word_count63w);head(word_count78w);head(word_count78n)




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

  word_counts <- as.data.table(text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% mutate(word= wordStem(word)) %>%
    count(word, sort = TRUE))
  #divide metadata pairs by two
  word_counts[word=="unclear" | word=="deletion" | word=="insertion" | word == "circle" | word=="underline", n := round(n/2)]
  #reorder by descending
  word_counts <- word_counts[order(-n)]

  word_counts <- text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% mutate(word= wordStem(word)) %>%
    count(word, sort = TRUE)
  detach(data)
  return(word_counts)
}
word_counts63w <- word_count_stem(S32W,T3)
word_counts78w <- word_count_stem(S32W,T4)
word_counts78n <- word_count_stem(S32N,T5)
head(word_counts63w);head(word_counts78w);head(word_counts78n)


word_count_lem <- function(data, q){
  attach(data)
  text_df <- tibble(row = 1:nrow(data), text = q)

  word_counts <- as.data.table(text_df %>% unnest_tokens(word, text) %>% 
                                 anti_join(stop_words) %>% mutate(word= textstem::lemmatize_words(word)) %>%
                                 count(word, sort = TRUE))
  #divide metadata pairs by two
  word_counts[word=="unclear" | word=="deletion" | word=="insertion" | word == "circle" | word=="underline", n := round(n/2)]
  #reorder by descending
  word_counts <- word_counts[order(-n)]

  word_counts <- text_df %>% unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% mutate(word= textstem::lemmatize_words(word)) %>%
    count(word, sort = TRUE)

  detach(data)
  return(word_counts)
}

word_countl63w <- word_count_lem(S32W,T3)
word_countl78w <- word_count_lem(S32W,T4)
word_countl78n <- word_count_lem(S32N,T5)
head(word_countl63w);head(word_countl78w);head(word_countl78n)
#------STEMMING/LEMMATIZATION SENTENCES--------------
# stems77_W <- text77_df %>% mutate(text= textstem::stem_strings(text)) %>% 
#   unnest_tokens(word, text) %>% 
#   anti_join(stop_words) %>%
#   count(word, sort = TRUE)
# 
# lems77_W <- text77_df %>% mutate(text= textstem::lemmatize_strings(text)) %>% 
#   unnest_tokens(word, text) %>% 
#   anti_join(stop_words)  %>%
#   count(word, sort = TRUE)



#---------- VISUALIZING EACH QUESTION----------------------

#separate outfits written response W (word_count63w | word_counts63w | word_countl63w)
#par(mfrow = c(1, 3))

ggplot(na.omit(word_count63w)[6:20,], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q63 no stems") + labs(x= "word", y = "count")
ggplot(na.omit(word_counts63w)[6:20, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q63 stems (Top 6-20 words)")+ labs(x= "word", y = "count")
ggplot(na.omit(word_countl63w)[6:20, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q63 lemmatization")+ labs(x= "word", y = "count")

#Overall impressions W (word_count78w | word_counts78w | word_countl78w)
ggplot(na.omit(word_count78w)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q78 no stems") + labs(x= "word", y = "count")
ggplot(na.omit(word_counts78w)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q78 stems (Top 15 words)")+ labs(x= "word", y = "count")
ggplot(na.omit(word_countl78w)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q78 lemmatization")+ labs(x= "word", y = "count")

#Overall impressions N (word_count7n | word_counts78n | word_countl78n)
ggplot(na.omit(word_count78n)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32N-Q78 no stems") + labs(x= "word", y = "count")
ggplot(na.omit(word_counts78n)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32N-Q78 stems (Top 15 words)")+ labs(x= "word", y = "count")
ggplot(na.omit(word_countl78n)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32N-Q78 lemmatization")+ labs(x= "word", y = "count")

ggplot(na.omit(word_count63w)[1:10, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q63 no stems") 
ggplot(na.omit(word_counts63w)[1:10, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q63 stems")
ggplot(na.omit(word_countl63w)[1:10, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q63 lemmatization")

#Overall impressions W (word_count78w | word_counts78w | word_countl78w)
ggplot(na.omit(word_count78w)[1:10, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q78 no stems") 
ggplot(na.omit(word_counts78w)[1:10, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q78 stems")
ggplot(na.omit(word_countl78w)[1:10, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32W-Q78 lemmatization")

#Overall impressions N (word_count7n | word_counts78n | word_countl78n)
ggplot(na.omit(word_count78n)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32N-Q78 no stems") 
ggplot(na.omit(word_counts78n)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32N-Q78 stems")
ggplot(na.omit(word_countl78n)[1:15, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle("S32N-Q78 lemmatization")



# plot_counts <- function(data, rows){
#   for (i in 1:length(data)){
#     ggplot(na.omit(data[i])[1:rows, ], aes(x = reorder(word,-n), y = as.numeric(n))) + geom_bar(stat="identity") + ggtitle(data[i])
#   }
# }
# 
# plot_counts(c(word_count78n, word_counts78n, word_countl78n),15)
