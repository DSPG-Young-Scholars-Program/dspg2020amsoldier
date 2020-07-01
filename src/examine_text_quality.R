library(dplyr);library(ggplot2);library(data.table);library(tidyr);library(stringr)
library(tidytext);library(textstem);library(SnowballC);library(tm)

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
data <- data %>% unite(long, long_comment:long_comment_cont, sep = " ", na.rm = TRUE) %>% 
  mutate(long = tolower(ifelse(long=="", NA,long)), outfits_comment = tolower(ifelse(outfits_comment=="", NA,outfits_comment)), index= 1:nrow(data))

######### Get each question into single string of text #######################
# 
# S32W <- data[data$racial_group == "white",]
# S32N <- data[data$racial_group == "black",]
#convert answers to character vectors
long <- data$long;#all text for long questions
outfit <- na.omit(data$outfits_comment) #all text for outfits question
text<- c(long, outfit) #entirety of text

#separate text by questions and racial group
# b.long <- S32N$long;b.long <- b.long[b.long != ""] #all text for long questions
# 
# w.long <- S32W$long;w.long <- w.long[w.long != ""] #all text for long questions
# w.outfit <- na.omit(S32W$outfits_comment) #all text for outfits question
# w.text<- c(w.long, w.outfit)

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


######## ------Calculate proportion of tags for each written response----- ############
metatags <- c("unclear")
data <- mutate(data, outfit_tags = str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])"))

um <- str_extract_all(data$long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])")


### Investiate tags per row of short and long responses ###
#The return of "" represents a consecutive metatag
#counting each correct use of of metatags as 2 words.
#create data table for long and outfit questions
#a tag is any bracketed entity
long <- data %>% select(long, racial_group) %>% filter(!is.na(long), long != " ", long != "") %>%
  mutate(nwords=sapply(strsplit(gsub("\\]"," ",long), " "), length),
         unclear= str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"),
         nunclear=lengths(unclear)*2,
         #underline = str_extract_all(long,"(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"),
         #insert = str_extract_all(long,"(?=\\[insertion\\]).*?(?<=\\[\\/insertion\\])"),
         #delete = str_extract_all(long,"(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"),
         #circle = str_extract_all(long,"(?=\\[circle\\]).*?(?<=\\[\\/circle\\])"),
         tags = str_extract_all(long, "(?=\\[).*?(?<=\\])|(?=\\[\\/).*?(?<=\\])"),
         #ntags = lengths(unclear) + lengths(insert)+ lengths(delete) + lengths(circle),
         ntags = lengths(tags),
         prop.tags = ntags/nwords,
         prop.unclear = nunclear/nwords,
         question = "long") %>% rename(response = long)
 

outfits <- data %>% select(outfits_comment, racial_group) %>% filter(racial_group=="white", !is.na(outfits_comment)) %>%
  mutate(nwords=sapply(strsplit(gsub("\\]"," ",outfits_comment), " "), length),
         unclear= str_extract_all(outfits_comment, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"),
         nunclear=lengths(unclear)*2,
         #underline = str_extract_all(outfits_comment,"(?=\\[underline\\]).*?(?<=\\[\\/underline\\])"),
         #insert = str_extract_all(outfits_comment,"(?=\\[insertion\\]).*?(?<=\\[\\/insertion\\])"),
         #delete = str_extract_all(outfits_comment,"(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"),
         #circle = str_extract_all(outfits_comment,"(?=\\[circle\\]).*?(?<=\\[\\/circle\\])"),
         tags = str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])|(?=\\[\\/).*?(?<=\\])"),
         #ntags = lengths(unclear) + lengths(insert)+ lengths(delete) + lengths(circle),
         ntags = lengths(tags),
         prop.tags = ntags/nwords,
         prop.unclear = nunclear/nwords,
         question = "outfits") %>% rename(response=outfits_comment)

proportions <- rbind(long, outfits)

###### plot the proportions ###### 
#majority of comments have zero tags
ggplot(filter(proportions, question == "long"), aes(x=prop.unclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + ggtitle("Long: unclear tag per comment")
ggplot(filter(proportions, question == "outfits"), aes(x=prop.unclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") +ggtitle("Outfits: unclear tag per comment")

ggplot(filter(proportions, question == "long"), aes(x=prop.tags)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + ggtitle("Long: metatags per comment")
ggplot(filter(proportions, question == "outfits"), aes(x=prop.tags)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") +ggtitle("Outfits:metatags per comment")

#facet by race
ggplot(filter(proportions, question == "long", racial_group == "black"), aes(x=prop.tags)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + facet_wrap(~racial_group)+ ggtitle("S32N Long: metatags per comment")
ggplot(filter(proportions, question == "long", racial_group == "white"), aes(x=prop.tags)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + facet_wrap(~racial_group)+ ggtitle("S32W Long: metatags per comment")

ggplot(filter(proportions, question == "long", racial_group == "black"), aes(x=prop.unclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + facet_wrap(~racial_group)+ ggtitle("S32N Long: unclear per comment")
ggplot(filter(proportions, question == "long", racial_group == "white"), aes(x=prop.unclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + facet_wrap(~racial_group)+ ggtitle("S32W Long: unclear per comment")

ggplot(filter(proportions, question == "long"), aes(x=prop.unclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + facet_wrap(~racial_group)+ ggtitle("Long: unclear tag per comment by race")



##### Plot by counts of tags and unclear ######
# note: one pair of unclear tags counts as 2 words
ggplot(filter(proportions, question == "long"), aes(x=nunclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") + ggtitle("Long: number of unclear")
ggplot(filter(proportions, question == "outfits"), aes(x=nunclear)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 40, fill = "dark blue") +ggtitle("Outfits: number of unclear")
#issue, can't see the large counts being represented.
ggplot(filter(proportions, question == "long"), aes(x=ntags)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 50, fill = "dark blue") + ggtitle("Long: metatags per comment")
ggplot(filter(proportions, question == "outfits"), aes(x=ntags)) + geom_histogram(aes(y=..count../sum(..count..)), bins = 50, fill = "dark blue") +ggtitle("Outfits:metatags per comment")


#need to remove the return of "" results when there are consecutive metatags with no text inbetween
#issue: correctly detecting [unclear][/unclear][unclear][/unclear]
test59 <- long$long[59] #should result in : "[unclear][/unclear]" "[unclear][/unclear]"
str_extract_all(test59, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", simplify = TRUE) #CURRENT DEFAULT returns : "[unclear][/unclear]" "" :(
str_extract_all(test59, "(?<=\\[unclear\\]).*?(?=\\[\\/unclear\\])") # EVEN WORSEreturns: [1] "" ""
str_extract_all(test59, "(?=\\[unclear\\]).*(?<=\\[\\/unclear\\])")


weird <- long$long[3179]
weird.res <- str_extract_all(weird, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])")
n.weird <- strsplit(gsub("\\]"," ",weird), " ")
strsplit(str_replace_all(weird, "[^[:alnum:]]", " "), " ")



#### ------ LOOK AT UNCLEAR INSTANCES AND CORRECTIVE MEASURES-------------- ###########
outfit_unclear <- data %>% select(-long) %>% 
  mutate(outfits_comment = str_replace_all(outfits_comment, "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]", ""),#remove any unclear with no filler 
         #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
         unclear=str_extract_all(outfits_comment, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
         unclear = ifelse(unclear == "character(0)", NA, unclear),
         correct = rep("", nrow(data)))%>% filter(!is.na(outfits_comment), !is.na(unclear)) %>%
  unnest(unclear)

#Manually correct unclear instances
#fwrite(outfit_unclear, file="~/git/dspg2020amsoldier/data/outfit_unclear.csv", sep = ",") #export the unclear table to csv
#researcher manually enters the correction in the correct column
outfit_unclear <- fread("~/git/dspg2020amsoldier/data/outfit_unclear.csv", sep = ",") #read the csv file back in.

#loops through and corrects original dataset :))))
for (i in 1:nrow(outfit_unclear)){
  j<-outfit_unclear$index[i]
  data$outfits_comment[j] <- str_replace(data$outfits_comment[j], "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", outfit_unclear$correct[i])
}
#data$outfits_comment[3730] <- str_replace(data$outfits_comment[outfit_unclear$index[1]], "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", outfit_unclear$correct[1])

long_unclear <- data %>% select(-outfits_comment) %>% 
  mutate(long = str_replace_all(long, "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]", ""),#remove any unclear with no filler or with question mark
         #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
         unclear=str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
         unclear = ifelse(unclear == "character(0)", NA, unclear))%>% filter(!is.na(long), !is.na(unclear)) %>%
  unnest(unclear)

#removing unclear instances with stop words: still ~600 instances.
long2_unclear <- data %>% select(-outfits_comment) %>% 
  mutate(long = str_replace_all(removeWords(long, stop_words$word), "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s*\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]", ""),#remove any unclear with no filler or with question mark
         #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
         unclear=str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
         unclear = ifelse(unclear == "character(0)", NA, unclear))%>% filter(!is.na(long), !is.na(unclear)) %>%
  unnest(unclear)

sigh <- as.data.table(table(long_unclear))[order(-N),]#see how many unique instances now....
# find all unclear with question marks: "\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]"


#### ------ LOOK AT BRACKET INSTANCES -------------- ###########
# if (metatag == "bracket"){
#   data.bracket <- unlist(str_extract_all(data, "(?=\\[).*?(?<=\\])"))
#   remove <- c("[unclear]","[/unclear]","[paragraph]", "[Paragraph]","[insertion]","[/insertion]", "", "[deletion]", "[/deletion]", "[underline]", "[/underline]", "[circle]", "[/circle]")
#   for(i in 1:13){data.bracket <- data.bracket[data.bracket!= remove[i]]}
#   data.bracket <- gsub("\\[", "", data.bracket);data.bracket <- gsub("\\]", "", data.bracket)
#   data.bracket <- as.data.table(table(data.bracket))[order(-N),]
#   return(data.bracket)
# }

outfit_bracket <- data %>% select(-long) %>% 
  mutate(outfits_comment = str_replace_all(outfits_comment, "\\[unclear\\]|\\[\\/unclear\\]|\\[insertion\\]|\\[\\/insertion\\]|\\[deletion\\]|\\[\\/deletion\\]|\\[underline\\]|\\[\\/underline\\]|\\[circle\\]|\\[\\/circle\\]|paragraph", ""),#remove any unclear with no filler 
         #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
         bracket=str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])"), #identify unclear tag with text inside
         bracket = ifelse(bracket == "character(0)", NA, bracket))%>% filter(!is.na(outfits_comment), !is.na(bracket)) %>%
  unnest(bracket)

long_bracket <- data %>% select(-outfits_comment) %>% 
  mutate(long = str_replace_all(long, "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]|\\[\\/unclear\\]|\\[insertion\\]|\\[\\/insertion\\]|\\[deletion\\]|\\[\\/deletion\\]|\\[underline\\]|\\[\\/underline\\]|\\[circle\\]|\\[\\/circle\\]|paragraph", ""),#remove any unclear with no filler 
         #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
         bracket=str_extract_all(long, "(?=\\[).*?(?<=\\])"), #identify unclear tag with text inside
         bracket = ifelse(bracket == "character(0)", NA, bracket))%>% filter(!is.na(long), !is.na(bracket)) %>%
  unnest(bracket)

#### ------ LOOK AT DELETE INSTANCES -------------- ###########
outfit_delete <- data %>% select(-long) %>% 
  mutate(delete=str_extract_all(outfits_comment, "(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"), #identify unclear tag with text inside
         delete = ifelse(delete == "character(0)", NA, delete))%>% filter(!is.na(outfits_comment), !is.na(delete)) %>%
  unnest(delete)

long_delete <- data %>% select(-outfits_comment) %>% 
  mutate(delete=str_extract_all(long, "(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"), #identify unclear tag with text inside
         delete = ifelse(delete == "character(0)", NA, delete))%>% filter(!is.na(long), !is.na(delete)) %>%
  unnest(delete)