library(ggplot2)
# to install ggradar, run the line commented out below
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(tibble)
library(scales)
library(fmsb)
library(data.table)
library(here)

source(here::here("src", "sentiment_analysis.R"));
s32_sentiments

plot_single <- function(data, race, type) {
  group_mean <- dplyr::as_data_frame(data) %>%
    filter(racial_group == race & response_type == type) %>%
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
    summarise_all(mean);
  
  group_mean_melted <- melt(group_mean)
  plot_data <- rbind(rep(max(group_mean_melted$value), 10), rep(min(group_mean_melted$value), 10), group_mean);
  return(plot_data);
}

plot_double <- function(data1, data2, race1, race2, type1, type2) {
  group1_mean <- dplyr::as_data_frame(data1) %>%
    filter(racial_group == race1 & response_type == type1) %>%
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
    summarise_all(mean);
  
  group2_mean <- dplyr::as_data_frame(data2) %>%
    filter(racial_group == race2 & response_type == type2) %>%
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
    summarise_all(mean);
  
  # combine repsonses
  comb <- rbind(group1_mean, group2_mean)
  rownames(comb) <- c("group1", "group2")
  
  # get min and max for plotting
  comb_melted <- melt(comb)
  minval <- min(comb_melted$value)
  maxval <- max(comb_melted$value)
  
  plot_data <- rbind(rep(maxval, 10), rep(minval, 10), comb)
  return(plot_data);
}

plot_data <- plot_single(race = "black", type = "long", s32_sentiments);
radarchart(plot_data,
           cglcol = "grey", 
           cglty = 1)



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
plot_data <- plot_double(data1 = s32_sentiments,
              data2 = s32_sentiments,
              race1 = "black", 
              race2 = "white", 
              type1 = "long", 
              type2 = "long");

colors <- c("#e57200", "#232d4b");
radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "NRC Sentiment Analysis of Long Response");


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

nrc_77_against <- white_long %>%
  filter(row %in% S32W_against$row)

nrc_77_for <- white_long %>%
  filter(row %in% S32W_for$row)

against_mean <- dplyr::as_data_frame(nrc_77_against) %>%
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

# does removing negation significantly change results?
plot_data <- plot_double(
  data1 = s32_sentiments,
  data2 = s32_negation_removed_sentiments,
  race1 = "black",
  race2 = "black",
  type1 = "long",
  type2 = "long"
);

colors <- c("#e57200", "#232d4b");
radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "Effect of removing negative bigrams on sentiment scores.");

View(s32_sentiments)


sum(s32_sentiments == s32_negation_removed_sentiments)
nrow(s32_sentiments)

# negation impacts on white short for separate
plot_data <- plot_double(data1 = s32_sentiments %>% filter(outfits == "['They should be in separate outfits']"),
                         data2 = s32_negation_removed_sentiments %>% filter(outfits == "['They should be in separate outfits']"),
                         race1 = "white", 
                         race2 = "white", 
                         type1 = "short", 
                         type2 = "short");

colors <- c("#e57200", "#232d4b");
radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "Impact of Negation on Segregationist White Short Responses");

# negation impacts on white short for together
plot_data <- plot_double(data1 = s32_sentiments %>% filter(outfits == "['They should be together in the same outfits']"),
                         data2 = s32_negation_removed_sentiments %>% filter(outfits == "['They should be together in the same outfits']"),
                         race1 = "white", 
                         race2 = "white", 
                         type1 = "short", 
                         type2 = "short");

colors <- c("#e57200", "#232d4b");
radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "Impact of Negation on Integrationist White Short Responses");

# negation impacts on white long for separate
plot_data <- plot_double(data1 = s32_sentiments %>% filter(outfits == "['They should be in separate outfits']"),
                         data2 = s32_negation_removed_sentiments %>% filter(outfits == "['They should be in separate outfits']"),
                         race1 = "white", 
                         race2 = "white", 
                         type1 = "long", 
                         type2 = "long");

colors <- c("#e57200", "#232d4b");
radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "Impact of Negation on Segregationist White Long Responses");

# negation impacts on white long for integration
plot_data <- plot_double(data1 = s32_sentiments %>% filter(outfits == "['They should be together in the same outfits']"),
                         data2 = s32_negation_removed_sentiments %>% filter(outfits == "['They should be together in the same outfits']"),
                         race1 = "white", 
                         race2 = "white", 
                         type1 = "long", 
                         type2 = "long");

colors <- c("#e57200", "#232d4b");
radarchart(plot_data,
           cglcol = "grey", # color of net
           cglty = 1, # net line type
           pcol = colors, # line color
           cglwd = 1, # net width,
           plwd = 3, # line width
           plty = 1, # plot line type
)
legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )
title(main = "Impact of Negation on Integrationist White Long Responses");