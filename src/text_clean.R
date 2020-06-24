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
data <- dbGetQuery(conn, "SELECT outfits_comment, long_comment, long_comment_cont, racial_group  
                                 FROM american_soldier.survey_32_combined")
# disconnect from postgresql
dbDisconnect(conn)
#first unite the long response and it's continued text
data <- data %>% unite(long, long_comment:long_comment_cont, sep = " ", na.rm = TRUE)



## change any answer that indicates no response to NA ##
#ex: "none", "[None]", "0" some of this filtering has been done in the LDA.R file on Master branch



## Remove the following metatags: [paragraph], [insertion][/insertion], [circle][/circle], [underline][/underline] ##
#Regex expression for [underline]: \\[underline\\]
#Regex expression for [/underline]: \\[\\/underline\\] 

