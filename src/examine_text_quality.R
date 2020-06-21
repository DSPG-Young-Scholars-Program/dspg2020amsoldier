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
long <- data$long
outfit <- na.omit(data$outfits_comment)

##### Look for all [unclear][/unclear] #####
#Other metadata tags are deletion, insertion, circle and underline
#str_extract_all(long[1:10], "\\[unclear\\]\\[\\/unclear\\]") returns metadata tags with no word in between
str_extract_all(long, "\\[unclear\\].*\\[\\/unclear\\]") #returns any use of metadata tags | drawback: if multiple of this tag are used, also returns all text in between sets of metadata tags.
#how to exclude character(0) from the results?

#find better way to summarize results

#### Which words underlined? #####
str_extract_all(long, "\\[underline\\].*\\[\\/underline\\]")
