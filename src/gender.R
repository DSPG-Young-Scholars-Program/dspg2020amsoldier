library(tidytext)
library(data.table)
library("RPostgreSQL")
library(stringi)
library(textstem)
library(dplyr)
library(tidyr)
library(SnowballC)
library(igraph)
library(ggraph)
library(widyr)

# ----- visualizae bigrams function from tidytext ------------- #
visualize_bigrams <- function(bigrams, title) {
  set.seed(2020)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void() + theme(legend.position = "none",
                      plot.title = element_text(hjust = 0.5)) +
    ggtitle(title)
}


# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data
data <- dbGetQuery(conn, "SELECT * FROM american_soldier.survey_32_clean")
dbDisconnect(conn)

# Words to collapse for Gender analysis: white boi, 



##### ------------- Dictionary load -----------------------------------------######
gender_words <- fread("~/git/dspg2020amsoldier/data/dictionary/gender.csv", sep = ",") 
spatial_words <- fread("~/git/dspg2020amsoldier/data/dictionary/spatial_arrangement.csv", sep = ",")




gender_match <- paste(paste("\\b", gender_words$gender,"\\b", sep = ""), collapse="|")
space_match <- paste(paste("\\b", spatial_words$space,"\\b", sep = ""), collapse="|")
##### -------------- Subset data based on race and question ---------------------------#####
S32W_short <- data %>% filter(racial_group== "white") %>% select(outfits_comment) %>% filter(!is.na(outfits_comment))
S32W_short <- tibble(nrow=1:nrow(S32W_short), text = S32W_short$outfits_comment)

S32W_long <- data %>% filter(racial_group== "white") %>% select(long) %>% filter(!is.na(long))
S32W_long <- tibble(nrow=1:nrow(S32W_long), text = S32W_long$long)

S32N_long <- data %>% filter(racial_group== "black") %>% select(long) %>% filter(!is.na(long))
S32N_long <- tibble(nrow=1:nrow(S32N_long), text = S32N_long$long)

##### --------- filter for gender related responses, then create bigrams ----------- #####
#filter outfits comment for responses that contain gender relating words
text63 <- filter(S32W_short, grepl(gender_match, text))
#create bigrams
text63_bigrams <- text63 %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% count(word1, word2, sort = TRUE)


##### --------- Gender Analysis: create bigrams, then filter --------------- #####
short_bigrams_gender <- S32W_short %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% filter(grepl(gender_match, bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  mutate(word1 = textstem::lemmatize_words(word1), word2 = textstem::lemmatize_words(word2)) %>%
  mutate(word1 = wordStem(word1), word2 = wordStem(word2)) %>% 
  count(word1, word2, sort = TRUE)

longW_bigrams_gender <- S32W_long %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% filter(grepl(gender_match, bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)  %>% 
  mutate(word1 = textstem::lemmatize_words(word1), word2 = textstem::lemmatize_words(word2)) %>%
  mutate(word1 = wordStem(word1), word2 = wordStem(word2)) %>% 
  count(word1, word2, sort = TRUE)

longN_bigrams_gender <- S32N_long %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% filter(grepl(gender_match, bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  mutate(word1 = textstem::lemmatize_words(word1), word2 = textstem::lemmatize_words(word2)) %>%
  mutate(word1 = wordStem(word1), word2 = wordStem(word2)) %>%
  count(word1, word2, sort = TRUE)



##### ------- VISUALIZE Gender bigrams --------------- #########
visualize_bigrams(short_bigrams_gender, "White Soldiers' Outfits Response - Gender Words")
visualize_bigrams(longW_bigrams_gender, "White Soldiers' Long Response - Gender Words")
visualize_bigrams(longN_bigrams_gender, "Black Soldiers' Long Response - Gender Words")





##### -------------- Spatial Arrangement Bigrams ---------------- #####
short_bigrams_space <- S32W_short %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% filter(grepl(space_match, bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  mutate(word1 = textstem::lemmatize_words(word1), word2 = textstem::lemmatize_words(word2)) %>%
  mutate(word1 = wordStem(word1), word2 = wordStem(word2)) %>% 
  count(word1, word2, sort = TRUE)

longW_bigrams_space <- S32W_long %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% filter(grepl(space_match, bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)  %>% 
  mutate(word1 = textstem::lemmatize_words(word1), word2 = textstem::lemmatize_words(word2)) %>%
  mutate(word1 = wordStem(word1), word2 = wordStem(word2)) %>% 
  count(word1, word2, sort = TRUE)

longN_bigrams_space <- S32N_long %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  count(bigram, sort =TRUE) %>% filter(grepl(space_match, bigram)) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  mutate(word1 = textstem::lemmatize_words(word1), word2 = textstem::lemmatize_words(word2)) %>%
  mutate(word1 = wordStem(word1), word2 = wordStem(word2)) %>%
  count(word1, word2, sort = TRUE)


visualize_bigrams(short_bigrams_space, "White Soldiers' Outfits Response - Spatial Arrangement") 
visualize_bigrams(longW_bigrams_space, "White Soldiers' Long Response - Spatial Arrangement")
visualize_bigrams(longN_bigrams_space, "Black Soldiers' Long Response - Spatial Arrangement")




###### Co-occurrances - Spatial Arrangement #######
short_cors <- S32W_short %>% 
  #separate words into occurrances and indicate section (response)
  mutate(section = row_number()) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  #collapse unique pairs
  group_by(word) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(item1 %in% spatial_words$space)
  

longW_cors <- S32W_long %>% 
  #separate words into occurrances and indicate section (response)
  mutate(section = row_number()) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  #collapse unique pairs
  group_by(word) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(item1 %in% spatial_words$space)

longN_cors <- S32N_long %>% 
  #separate words into occurrances and indicate section (response)
  mutate(section = row_number()) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  #collapse unique pairs
  group_by(word) %>%
  pairwise_cor(word, section, sort = TRUE) %>%
  filter(item1 %in% spatial_words$space)

# text63_df <- tibble(nrow=1:nrow(S32W_short), text = S32W_short$outfits_comment)

# row_n_words <- S32W_short %>%
#   mutate(section = row_number()) %>%
#   filter(section > 0) %>%
#   unnest_tokens(word, text) %>%
#   filter(!word %in% stop_words$word) %>%
#   mutate(word= textstem::lemmatize_words(word)) %>%
#   mutate(word= wordStem(word))



# yes <- stri_detect_regex(S32W_short, gender_words$gender)
# S32W_short[which(yes),]
