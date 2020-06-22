library(dplyr);library(ggplot2);library(data.table);library(tidyr);library(stringr)
library(tidytext);library(textstem);library(SnowballC)

##### READ DATA FROM DATABASE #############
library("RPostgreSQL")
# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(), 
                  dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data  
data0 <- dbGetQuery(conn, "SELECT *  
                                 FROM american_soldier.survey_32_combined")
# disconnect from postgresql
dbDisconnect(conn)

#NOTE: no distinction of white and black soldier responses. 


######### Get each question into single string of text #######################
#first unite the long response and it's continued text
data <- data0 %>% unite(long, long_comment:long_comment_cont, sep = " ", na.rm = TRUE)

#as character
long <- data$long;long <- long[long != ""]
outfit <- na.omit(data$outfits_comment)
text<- c(long, outfit)
#as data.frame
#long <- select(data, long)
#outfit <- na.omit(select(data, outfits_comment))

##### Look for all [unclear][/unclear] #####
#Other metadata tags are deletion, insertion, circle and underline
long.unclear <- unlist(str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"))
out.unclear <- unlist(str_extract_all(outfit, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"))
#returns any use of metadata tags
long.unclear <- long.unclear[long.unclear != ""]
out.unclear <- out.unclear[out.unclear != ""]

#perform analysis on entire collection of responses
text.unclear <- unlist(str_extract_all(text, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"))
text.unclear <- text.unclear[text.unclear != ""]
text.unclear <- as.data.frame(table(text.unclear))

#### Which words underlined? #####
out.underline <- unlist(str_extract_all(outfit, "\\[underline\\].*\\[\\/underline\\]"))
long.underline <- unlist(str_extract_all(long, "(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"))
long.underline <- long.underline[long.underline != ""]
out.underline <- out.underline[out.underline != ""]

text.underline <- unlist(str_extract_all(text, "(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"))
text.underline <- text.underline[text.underline != ""]
text.underline <- as.data.frame(table(text.underline))

### all bracketed words that represent spell corrections ####
text.bracket <- unlist(str_extract_all(text, "(?=\\[).*?(?<=\\])"))
remove <- c("[unclear]","[/unclear]","[paragraph]", "[Paragraph]","[insertion]","[/insertion]", "", "[deletion]", "[/deletion]", "[underline]", "[/underline]", "[circle]", "[/circle]")
for(i in 1:13){text.bracket <- text.bracket[text.bracket!= remove[i]]}
text.bracket <- gsub("\\[", "", text.bracket);text.bracket <- gsub("\\]", "", text.bracket)
text.bracket <- as.data.frame(table(text.bracket))


##### which words circled? #####
text.circle <- unlist(str_extract_all(text, "(?=\\[circle\\]).*?(?<=\\[\\/circle\\])"))
text.circle <- text.circle[text.circle != ""]
text.circle <- as.data.frame(table(text.circle))
