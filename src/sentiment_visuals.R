library(ggplot2)
# to install ggradar, run the line commented out below
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(tibble)
library(scales)
library(fmsb)
library(data.table)


# normalize sentiment scores to length of response
# textn_df - Black long response
# text77_df - white outfit response
# text78_df - white outfit response

# normalize bing_and_nrc
# returns a vector containing the length of each response
# response_lengths <- apply(bing_and_nrc, 1, function(x) {
#   return(length(as.list(strsplit(textn_df[x["row"], ]$text, '\\s+')[[1]])))
# }); # returns integer vector
# 
# bing_and_nrc$response_length <- response_lengths
# bing_and_nrc_norm <- bing_and_nrc %>%
#   mutate(anger = anger / response_length,
#          anticipation = anticipation / response_length,
#          disgust = disgust / response_length,
#          fear = fear / response_length,
#          joy = joy / response_length,
#          negative = negative / response_length,
#          positive = positive / response_length,
#          sadness = sadness / response_length,
#          surprise = surprise / response_length,
#          trust = trust / response_length,
#          sentiment = sentiment / response_length)
# 
# # normalize bing_and_nrc_77
# response_lengths <- apply(bing_and_nrc_77, 1, function(x) {
#   return(length(as.list(strsplit(text77_df[x["row"], ]$text, '\\s+')[[1]])))
# });
# 
# bing_and_nrc_77$response_length <- response_lengths
# bing_and_nrc_77_norm <- bing_and_nrc_77 %>%
#   mutate(anger = anger / response_length,
#          anticipation = anticipation / response_length,
#          disgust = disgust / response_length,
#          fear = fear / response_length,
#          joy = joy / response_length,
#          negative = negative / response_length,
#          positive = positive / response_length,
#          sadness = sadness / response_length,
#          surprise = surprise / response_length,
#          trust = trust / response_length,
#          sentiment = sentiment / response_length)
# 
# # normalize bing_and_nrc_78
# response_lengths <- apply(bing_and_nrc_78, 1, function(x) {
#   return(length(as.list(strsplit(text78_df[x["row"], ]$text, '\\s+')[[1]])))
# });
# 
# bing_and_nrc_78$response_length <- response_lengths
# bing_and_nrc_78_norm <- bing_and_nrc_78 %>%
#   mutate(anger = anger / response_length,
#          anticipation = anticipation / response_length,
#          disgust = disgust / response_length,
#          fear = fear / response_length,
#          joy = joy / response_length,
#          negative = negative / response_length,
#          positive = positive / response_length,
#          sadness = sadness / response_length,
#          surprise = surprise / response_length,
#          trust = trust / response_length,
#          sentiment = sentiment / response_length)
sentiments

# black, long reponse
black_long_mean <- dplyr::as_data_frame(sentiments) %>%
  filter(response_type == "black_long") %>%
  select(c("anger",
           "anticipation",
           "disgust",
           "fear",
           "joy",
           "negative",
           "positive",
           "sadness",
           "surprise",
           "trust")) %>%
  summarise_all(mean)

bl_mean_melted <- melt(black_long_mean)
plot_data <- rbind(rep(max(bl_mean_melted$value), 10), rep(min(bl_mean_melted$value), 10), black_long_mean)
radarchart(plot_data,
           cglcol = "grey",
           cglty = 1)

# white, short response
white_short_mean <- dplyr::as_data_frame(sentiments) %>%
  filter(response_type == "white_short") %>%
  select(c("anger",
           "anticipation",
           "disgust",
           "fear",
           "joy",
           "negative",
           "positive",
           "sadness",
           "surprise",
           "trust")) %>%
  summarise_all(mean)

ws_mean_melted <- melt(white_short_mean)
plot_data <- rbind(rep(max(ws_mean_melted$value), 10), rep(min(ws_mean_melted$value), 10), white_short_mean)
radarchart(plot_data,
           cglcol = "grey",
           cglty = 1)

# white, long response
white_long_mean <- dplyr::as_data_frame(sentiments) %>%
  filter(response_type == "white_long") %>%
  select(c("anger",
           "anticipation",
           "disgust",
           "fear",
           "joy",
           "negative",
           "positive",
           "sadness",
           "surprise",
           "trust")) %>%
  summarise_all(mean)

wl_mean_melted <- melt(white_long_mean)
plot_data <- rbind(rep(max(wl_mean_melted$value), 10), rep(min(wl_mean_melted$value), 10), white_long_mean)
radarchart(plot_data,
           cglcol = "grey",
           cglty = 1)

## PLOT 3
# combined long responses chart
black_long <- copy(black_long_mean)
white_long <- copy(white_long_mean)

# combine repsonses
long <- rbind(black_long, white_long)
rownames(long) <- c("Black", "white")

# get min and max for plotting
long_melted <- melt(long)
minval <- min(long_melted$value)
maxval <- max(long_melted$value)

plot_data <- rbind(rep(maxval, 10), rep(minval, 10), long)

colors <- c("#e57200", "#232d4b")

radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "NRC Sentiment Analysis of Long Response")


# PLOT 4
# use bar plots instead to visualize
plot_data <- melt(copy(long))
plot_data$race <- c("black", "white")
ggplot(data = plot_data, mapping = aes(x = variable, y = value, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colors) +
  labs(title = "Mean Number of Words Associated with Each Sentiment for S32 Long Response",
       x = "Sentiment",
       y = "Mean Number of Words")

# PLOT 5
# visualize differences in sentiments between white and black soldiers
long_diff <- black_long - white_long
long_diff %>%
  as.data.table(.) %>%
  melt(.) %>%
  arrange(desc(value)) %>%
  ggplot(., aes(x = variable, y = value)) +
  geom_bar(stat = "identity")

# PLOT 6
# visualize difference in sentiments between white soliders who were for/against desegregation
S32W$row <- 1:nrow(S32W) # how row was computed in text77_df
levels(as.factor(S32W$outfits))

# against desegregation
S32W_against <- S32W %>%
  filter(outfits == "['They should be in separate outfits']")
nrow(S32W_against) # 2048

# for desegregation
S32W_for <- S32W %>%
  filter(outfits == "['They should be together in the same outfits']")
nrow(S32W_for) # 101

nrc_77_against <- bing_and_nrc_77_norm %>%
  filter(method == "NRC") %>%
  filter(row %in% S32W_against$row)

nrc_77_for <- bing_and_nrc_77_norm %>%
  filter(method == "NRC") %>%
  filter(row %in% S32W_for$row)

against_mean <- dplyr::as_data_frame(nrc_77_against) %>%
  filter(method == "NRC") %>%
  select(c("anger",
           "anticipation",
           "disgust",
           "fear",
           "joy",
           "negative",
           "positive",
           "sadness",
           "surprise",
           "trust")) %>%
  summarise_all(mean)

for_mean <- dplyr::as_data_frame(nrc_77_for) %>%
  filter(method == "NRC") %>%
  select(c("anger",
           "anticipation",
           "disgust",
           "fear",
           "joy",
           "negative",
           "positive",
           "sadness",
           "surprise",
           "trust")) %>%
  summarise_all(mean)

comb <- rbind(against_mean, for_mean)
rownames(comb) <- c("against integration", "for integration")

# get min and max for plotting
comb_melted <- melt(comb)
minval <- min(comb_melted$value)
maxval <- max(comb_melted$value)

plot_data <- rbind(rep(maxval, 10), rep(minval, 10), comb)

colors <- c("#e57200", "#232d4b")

radarchart(plot_data,
           cglcol = "grey",
           cglty = 1,
           pcol = colors,
           plty = 1, 
           plwd = 3, # line width
)

legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "NRC Sentiment Analysis of White Soliders'\n Comment on Outfit Integration")

# change labels to same_outfits integrated_words


