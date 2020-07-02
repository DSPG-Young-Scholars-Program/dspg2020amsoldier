library(ggplot2)
# to install ggradar, run the line commented out below
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)
library(tibble)
library(scales)
library(fmsb)

# black, long reponse
class(bing_and_nrc)

nrc_mean <- dplyr::as_data_frame(bing_and_nrc) %>%
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

plot_data <- rbind(rep(5, 10), rep(0, 10), nrc_mean)
radarchart(plot_data,
           cglcol = "grey",
           cglty = 1)

# white, short response
bing_and_nrc_77
nrc_77_mean <- dplyr::as_data_frame(bing_and_nrc_77) %>%
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
head(nrc_77_mean)
nrc_77_mean_melted <- melt(nrc_77_mean)
min(nrc_77_mean_melted$value)
plot_data <- rbind(rep(max(nrc_77_mean_melted$value), 10), rep(min(nrc_77_mean_melted$value), 10), nrc_77_mean)
radarchart(plot_data,
           cglcol = "grey",
           cglty = 1)

# white, long response
bing_and_nrc_78
nrc_78_mean <- dplyr::as_data_frame(bing_and_nrc_78) %>%
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

nrc_78_mean_melted <- melt(nrc_78_mean)
plot_data <- rbind(rep(max(nrc_78_mean_melted$value), 10), rep(min(nrc_78_mean_melted$value), 10), nrc_78_mean)

radarchart(plot_data,
           cglcol = "grey",
           cglty = 1)

## PLOT 3
# combined long responses chart
black_long <- copy(nrc_mean)
white_long <- copy(nrc_78_mean)
# combine repsonses
long <- rbind(black_long, white_long)
rownames(long) <- c("black", "white")

# get min and max for plotting
long_melted <- melt(long)
minval <- min(long_melted$value)
maxval <- max(long_melted$value)

plot_data <- rbind(rep(maxval, 10), rep(minval, 10), long)

colors=c("#e57200", "#232d4b")

radarchart(plot_data,
           cglcol = "grey",
           cglty = 1,
           pcol = colors,
           plty=1)

legend(x=1, y=1, legend = rownames(plot_data)[-c(1,2)], bty = "n", pch = 20, col = colors )

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

