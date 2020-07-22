library(dplyr)
library(tidytext)
library(SnowballC)

# read in rules and edge list
rules <- read.csv('./data/edge_list_cleaning_rules.csv')
edge_list <- read.csv('./data/32W_outfits_biterms_edge_list.csv')

# check that both were read in correctly
head(rules)
head(edge_list)

# filter by action
to_replace <- rules %>% filter(action == "replace")
to_remove <- rules %>% filter(action == "remove")
nrow(to_remove) # 10

sum(edge_list$source %in% to_remove$original.word) # 14 words to remove

# remove appropriate words from edge list
edge_list_clean <- edge_list %>%
  subset(., !source %in% to_remove$original.word) %>%
  subset(., !target %in% to_remove$original.word)

edge_list_clean %>%
  filter(source %in% to_remove$original.word | target %in% to_remove$original.word) %>%
  count() # should be 0


sum(edge_list_clean$source %in% to_replace$original.word) # 262
# replace appropriate words from edge list
# for source
new_source <- apply(edge_list_clean, 1, function(row) {
  if (row["source"] %in% to_replace$original.word) {
    tmp <- to_replace %>% filter(original.word == row["source"])
    new_word <- tmp$replace.with
    return(new_word);
  } else {
    return(row["source"]);
  }
});
new_source <- unlist(new_source, use.names = FALSE)
edge_list_clean$source <- new_source
# for target
new_target <- apply(edge_list_clean, 1, function(row) {
  if (row["target"] %in% to_replace$original.word) {
    tmp <- to_replace %>% filter(original.word == row["target"])
    new_word <- tmp$replace.with
    return(new_word);
  } else {
    return(row["target"]);
  }
});
new_target <- unlist(new_target, use.names = FALSE)
edge_list_clean$target <- new_target
# check computations
sum(edge_list_clean$source %in% to_remove$original.word) # 0
sum(edge_list_clean$source %in% to_replace$original.word) #

# convert to lower
edge_list_clean$source <- tolower(edge_list_clean$source)
edge_list_clean$target <- tolower(edge_list_clean$target)

# remove stop words
data(stop_words) # load stop words

# check how many rows contain stop words
# edge_list_clean %>%
#   filter(source %in% stop_words$word | target %in% stop_words$word) %>%
#   count() # should be 1786 before cleaning

# remove rows that have stop words
edge_list_clean <- edge_list_clean %>%
  subset(., !source %in% stop_words$word) %>%
  subset(., !target %in% stop_words$word)

# check how many rows in cleaned data contain stop words, should be zero
edge_list_clean %>%
  filter(source %in% stop_words$word | target %in% stop_words$word) %>%
  count() # should be 0 after cleaning

# remove plurality
edge_list_clean <- edge_list_clean %>%
  mutate(
    source = wordStem(source),
    target = wordStem(target)
  );

write.csv(edge_list_clean, "./data/32W_outfits_biterms_edge_list_clean.csv")
