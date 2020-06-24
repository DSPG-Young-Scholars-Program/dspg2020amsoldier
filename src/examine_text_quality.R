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
data0 <- dbGetQuery(conn, "SELECT outfits_comment, long_comment, long_comment_cont, racial_group  
                                 FROM american_soldier.survey_32_combined")
# disconnect from postgresql
dbDisconnect(conn)
#first unite the long response and it's continued text
data <- data0 %>% unite(long, long_comment:long_comment_cont, sep = " ", na.rm = TRUE)

######### Get each question into single string of text #######################

S32W <- data[data$racial_group == "white",]
S32N <- data[data$racial_group == "black",]
#convert answers to character vectors
long <- data$long;long <- long[long != ""] #all text for long questions
outfit <- na.omit(data$outfits_comment) #all text for outfits question
text<- c(long, outfit) #entirety of text

#separate text by questions and racial group
b.long <- S32N$long;b.long <- b.long[b.long != ""] #all text for long questions

w.long <- S32W$long;w.long <- w.long[w.long != ""] #all text for long questions
w.outfit <- na.omit(S32W$outfits_comment) #all text for outfits question
w.text<- c(w.long, w.outfit)

#####  #####
#Other metadata tags are deletion, insertion, circle and underline
long.unclear <- unlist(str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"))
out.unclear <- unlist(str_extract_all(outfit, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"))
#returns any use of metadata tags
long.unclear <- long.unclear[long.unclear != ""]
out.unclear <- out.unclear[out.unclear != ""]

#### Entire collection for metadata tags ####

# Look for all [unclear][/unclear]
text.unclear <- unlist(str_extract_all(text, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"))
text.unclear <- text.unclear[text.unclear != ""]
text.unclear <- as.data.frame(table(text.unclear))

# Which words underlined?
out.underline <- unlist(str_extract_all(outfit, "\\[underline\\].*\\[\\/underline\\]"))
long.underline <- unlist(str_extract_all(long, "(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"))
long.underline <- long.underline[long.underline != ""]
out.underline <- out.underline[out.underline != ""]

text.underline <- unlist(str_extract_all(text, "(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"))
text.underline <- text.underline[text.underline != ""]
text.underline <- as.data.table(table(text.underline))[order(-N), ]

# all bracketed words that represent spell corrections 
text.bracket <- unlist(str_extract_all(text, "(?=\\[).*?(?<=\\])"))
remove <- c("[unclear]","[/unclear]","[paragraph]", "[Paragraph]","[insertion]","[/insertion]", "", "[deletion]", "[/deletion]", "[underline]", "[/underline]", "[circle]", "[/circle]")
for(i in 1:13){text.bracket <- text.bracket[text.bracket!= remove[i]]}
text.bracket <- gsub("\\[", "", text.bracket);text.bracket <- gsub("\\]", "", text.bracket)
text.bracket <- as.data.frame(table(text.bracket))


##### which words circled? #####
text.circle <- unlist(str_extract_all(text, "(?=\\[circle\\]).*?(?<=\\[\\/circle\\])"))
text.circle <- text.circle[text.circle != ""]
text.circle <- as.data.frame(table(text.circle))



####### write function to extract words involving any metadata tag ###########
extract_tag <- function(data, metatag){
  if (metatag == "bracket"){
    data.bracket <- unlist(str_extract_all(data, "(?=\\[).*?(?<=\\])"))
    remove <- c("[unclear]","[/unclear]","[paragraph]", "[Paragraph]","[insertion]","[/insertion]", "", "[deletion]", "[/deletion]", "[underline]", "[/underline]", "[circle]", "[/circle]")
    for(i in 1:13){data.bracket <- data.bracket[data.bracket!= remove[i]]}
    data.bracket <- gsub("\\[", "", data.bracket);data.bracket <- gsub("\\]", "", data.bracket)
    data.bracket <- as.data.table(table(data.bracket))[order(-N),]
    return(data.bracket)
  }
  else{
  #create regex pattern that matches the metadata tag
  rx <- paste("(?=\\[", metatag, "\\]).*?(?<=\\[\\/", metatag, "\\])", sep="")
  #extract all cases of use for that tag
  data.metatag <- unlist(str_extract_all(data, rx))
  #Remove missing data and store in data.frame.
  data.metatag <- data.metatag[data.metatag != ""]
  data.metatag <- as.data.table(table(data.metatag))[order(-N),]
  return(data.metatag)
  }
}


### Calculate proportion of tags for each written response ###
metatags <- c("unclear")
data <- mutate(data, outfit_tags = str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])"))

um <- str_extract_all(data$long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])")


### Investiate tags per row of short and long responses ###
#create data table for long and outfit questions
long <- data %>% select(long, racial_group) %>% 
  mutate(unclear= str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"),
         underline = str_extract_all(long,"(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"),
         insert = str_extract_all(long,"(?=\\[insertion\\]).*?(?<=\\[\\/insertion\\])"),
         delete = str_extract_all(long,"(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"),
         circle = str_extract_all(long,"(?=\\[circle\\]).*?(?<=\\[\\/circle\\])"),
         nwords=sapply(strsplit(str_replace_all(long, "[^[:alnum:]]", " "), " "), length),
         nunclear = lengths(unclear),
         tags = str_extract_all(long, "(?=\\[).*?(?<=\\])|(?=\\[\\/).*?(?<=\\])"),
         ntags = lengths(unclear) + lengths(insert)+ lengths(delete) + lengths(circle),
         prop.tags = lengths(tags)/nwords)
#need to remove "" 
outfit <- data %>% select(outfits_comment, racial_group) %>% filter(racial_group=="white") %>%
  mutate(unclear=str_extract_all(outfits_comment, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"),
         underline = str_extract_all(outfits_comment,"(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"),
         nwords=sapply(strsplit(outfits_comment, " "), length),
         nunclear = lengths(unclear),
         tags = str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])|(?=\\[\\/).*?(?<=\\])"),
         ntags = lengths(tags),
         prop.tags = ntags/nwords)

