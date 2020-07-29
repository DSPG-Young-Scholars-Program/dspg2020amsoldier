library(tidyverse)
library(tidytext)

# load data into environment
source(here::here("src", "load_data.R"));

data(stop_words)
word_counts <- s32 %>%
  filter(response_type == "long") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(racial_group, response_type, word) %>%
  count() %>%
  arrange(desc(n))

black_words <- word_counts %>% filter(racial_group == "black")
white_words <- word_counts %>% filter(racial_group == "white")
unique_black_words <- anti_join(black_words, white_words, by = "word")
unique_white_words <- anti_join(white_words, black_words, by = "word")

word_totals <- word_counts %>%
  group_by(racial_group) %>%
  summarize(sum = sum(n))

word_props <- word_counts %>%
  inner_join(word_totals) %>%
  mutate(prop = n / sum) %>%
  arrange(desc(prop))

black_props <- word_props %>%
  ungroup() %>%
  filter(racial_group == "black") %>%
  rename(black_prop = prop) %>%
  select(c("word", "black_prop"))

white_props <- word_props %>%
  ungroup() %>%
  filter(racial_group == "white") %>%
  rename(white_prop = prop) %>%
  select(c("word", "white_prop"))

word_props_joined <- full_join(black_props, white_props, by = "word") %>%
  replace_na(replace = list(black_props = 0, white_props = 0))

word_props_joined$rel_prop <- abs(word_props_joined$black_prop - word_props_joined$white_prop)
word_props_final <- word_props_joined %>% arrange(desc(rel_prop))
View(word_props_final)

# unique word frequency plots

unique_black_words %>%
  as.data.frame(.) %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(10, n) %>%
  ggplot(., aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frequency of Unique Words in Black Soldiers' Long Responses",
       x = "Word",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_flip()

unique_white_words %>%
  as.data.frame(.) %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(10, n) %>%
  ggplot(., aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frequency of Unique Words in White Soldiers' Long Responses",
       x = "Word",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_flip()

# word clouds
white_props %>%
  with(wordcloud(word, white_prop, max.words = 100))

black_props %>%
  with(wordcloud(word, black_prop, max.words = 100))

# seg v int -------------------------------------------------
word_counts_short <- s32 %>%
  filter(response_type == "short") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  group_by(racial_group, response_type, word, outfits) %>%
  filter(outfits == "['They should be in separate outfits']" | outfits == "['They should be together in the same outfits']") %>%
  count() %>%
  arrange(desc(n))

seg_words <- word_counts_short %>% filter(outfits == "['They should be in separate outfits']")
int_words <- word_counts_short %>% filter(outfits == "['They should be together in the same outfits']")
unique_seg_words <- anti_join(seg_words, int_words, by = "word")
unique_int_words <- anti_join(int_words, seg_words, by = "word")

word_totals_short <- word_counts_short %>%
  group_by(outfits) %>%
  summarize(sum = sum(n))

word_props_short <- word_counts_short %>%
  inner_join(word_totals_short) %>%
  mutate(prop = n / sum) %>%
  arrange(desc(prop))

seg_props <- word_props_short %>%
  ungroup() %>%
  filter(outfits == "['They should be in separate outfits']") %>%
  rename(seg_prop = prop) %>%
  select(c("word", "seg_prop"))

int_props <- word_props_short %>%
  ungroup() %>%
  filter(outfits == "['They should be together in the same outfits']") %>%
  rename(int_prop = prop) %>%
  select(c("word", "int_prop"))

word_props_short_joined <- full_join(seg_props, int_props, by = "word") %>%
  replace_na(replace = list(seg_props = 0, int_props = 0))

word_props_short_joined$rel_prop <- abs(word_props_short_joined$seg_prop - word_props_short_joined$int_prop)
word_props_final_short <- word_props_short_joined %>% arrange(desc(rel_prop))
View(word_props_final_short)

# unique word frequency plots

unique_seg_words %>%
  as.data.frame(.) %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(10, n) %>%
  ggplot(., aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frequency of Unique Words in Pro-Segregation White Soldiers' Short Responses",
       x = "Word",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_flip()

unique_int_words %>%
  as.data.frame(.) %>%
  arrange(desc(n)) %>%
  mutate(word = factor(word, levels = word)) %>%
  top_n(4, n) %>%
  ggplot(., aes(x = word, y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Frequency of Unique Words in Anti-Segregation White Soldiers' Short Responses",
       x = "Word",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_flip()

# word clouds
seg_props %>%
  with(wordcloud(word, seg_prop, max.words = 100))

int_props %>%
  with(wordcloud(word, int_prop, max.words = 100))
