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

#as data.frame
#long <- select(data, long)
#outfit <- na.omit(select(data, outfits_comment))

##### Look for all [unclear][/unclear] #####
#Other metadata tags are deletion, insertion, circle and underline
#str_extract_all(long[1:10], "\\[unclear\\]\\[\\/unclear\\]") returns metadata tags with no word in between
long.unclear <- unlist(str_extract_all(long, "\\[unclear\\].*\\[\\/unclear\\]"))
out.unclear <- unlist(str_extract_all(outfit, "\\[unclear\\].*\\[\\/unclear\\]"))
#returns any use of metadata tags | drawback: if multiple of this tag are used, also returns all text in between sets of metadata tags.

#if data.frame
#long <- mutate(long, unclear = str_extract_all(long, "\\[unclear\\].*\\[\\/unclear\\]"))

#how to exclude character(0) from the results?

long.unclear <- long.unclear[long.unclear != "character(0)"]
out.unclear <- out.unclear[out.unclear != "character(0)"]
#long$unclear[long$unclear == "character(0)"] <- NA #make character(0) NA..... this might help?
#find better way to summarize results



#### Which words underlined? #####
long.underline <- unlist(str_extract_all(long, "\\[underline\\].*\\[\\/underline\\]"))
out.underline <- unlist(str_extract_all(outfit, "\\[underline\\].*\\[\\/underline\\]"))

long.underline <- long.underline[long.underline != "character(0)"]
out.underline <- out.underline[out.underline != "character(0)"]