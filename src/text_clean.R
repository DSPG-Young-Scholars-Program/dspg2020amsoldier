library(dplyr);library(ggplot2);library(data.table);library(tidyr);library(stringr)
library(tidytext);library(textstem);library(SnowballC);library(naniar);library(textclean)

##### 1. READ DATA FROM DATABASE #############
library("RPostgreSQL")
# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data
data <- dbGetQuery(conn, "SELECT * FROM american_soldier.survey_32_combined")



#### 2. Initial Processing ########
data <- data %>% unite(long, long_comment:long_comment_cont, sep = " ", na.rm = TRUE) %>% # first unite the long response and it's continued text 
  mutate(long = tolower(long), outfits_comment = tolower(outfits_comment),  # make entire text lowercase 
         index= 1:nrow(data)) # add an index column



##### 3. Meaningless response to NA ######
#ex: "none", "[None]", "0" 
na_list <- c("none", "[none]", "noone", "nnone", "[blank]", "n/a", "i", ".", "no comment", "no comments", "have none",
              "no reason", "no reasons", "left blank", "[no answer]", "[slash] [slash]", "0", "12", 
             "i have no comments", "gujfujuj")

outfits_predicate <- data$racial_group == "white" & tolower(data$outfits_comment) %in% na_list
long_predicate <- tolower(data$long) %in% na_list

data_clean <- data %>%
  mutate(
    outfits_comment = ifelse(outfits_predicate, NA, outfits_comment),
    long = ifelse(long_predicate, NA, long)
  )



##### 4. Automated Tag Removal ######
#Remove the following metatags: [paragraph], [insertion][/insertion], [circle][/circle], [underline][/underline] ##
#Regex expression for [underline]: \\[underline\\]
#Regex expression for [/underline]: \\[\\/underline\\]

# remove [paragraph]
paragraph_pattern <- "\\[paragraph\\]"
data_clean$outfits_comment <- str_replace_all(data_clean$outfits_comment, paragraph_pattern, "")
data_clean$long <- str_replace_all(data_clean$long, paragraph_pattern, "")

# remove [insertion][/insertion]
insertion_pattern.1 <- "\\[insertion\\]"
insertion_pattern.2 <- "\\[\\/insertion\\]"

# testing insertion pattern
# insertion_test <- c("this doesn't have an insertion.", "this [insertion]does[/insertion] have one.")
# insertion_test %>%
#   str_replace(insertion_pattern.1, "") %>%
#   str_replace(insertion_pattern.2, "")

data_clean$outfits_comment <- data_clean$outfits_comment %>%
  str_replace_all(insertion_pattern.1, "") %>%
  str_replace_all(insertion_pattern.2, "")

data_clean$long <- data_clean$long %>%
  str_replace_all(insertion_pattern.1, "") %>%
  str_replace_all(insertion_pattern.2, "")

# remove [circle][/circle]
circle_pattern.1 <- "\\[circle\\]"
circle_pattern.2 <- "\\[\\/circle\\]"

# testing circle pattern
# circle_test <- c("[circle]this[/circle] is circled.", "this is not circled.")
# circle_test %>%
#   str_replace(circle_pattern.1, "") %>%
#   str_replace(circle_pattern.2, "")

data_clean$outfits_comment <- data_clean$outfits_comment %>%
  str_replace_all(circle_pattern.1, "") %>%
  str_replace_all(circle_pattern.2, "")

data_clean$long <- data_clean$long %>%
  str_replace_all(circle_pattern.1, "") %>%
  str_replace_all(circle_pattern.2, "")

# remove [underline][/underline]
underline_pattern.1 <- "\\[underline\\]"
underline_pattern.2 <- "\\[\\/underline\\]"

data_clean$outfits_comment <- data_clean$outfits_comment %>%
  str_replace_all(underline_pattern.1, "") %>%
  str_replace_all(underline_pattern.2, "")

data_clean$long <- data_clean$long %>%
  str_replace_all(underline_pattern.1, "") %>%
  str_replace_all(underline_pattern.2, "")

# remove [deletion][/deletion] and anything inside the tag
delete.rm <- "(?=\\[deletion\\]).*?(?<=\\[\\/deletion\\])"
delete.rm2 <- "\\[deletion\\]|\\[\\/deletion\\]"
data_clean <- data_clean %>% mutate(outfits_comment = str_replace_all(outfits_comment, delete.rm, ""), #first delete occurances with words inside
                                    long = str_replace_all(long, delete.rm, ""),
                                    outfits_comment = str_replace_all(outfits_comment, delete.rm2, ""), #second delete any occurances of the tag
                                    long = str_replace_all(long, delete.rm2, ""))


# remove [unclear][/unclear] with no meaningful filler or with question mark
unclear.rm <- "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]"
data_clean <- data_clean %>% mutate(outfits_comment = str_replace_all(outfits_comment, unclear.rm, ""),
                                    long = str_replace_all(long, unclear.rm, ""))



###### 5. Unclear metatag cleaning ###### 
# #------------------ Outfit unclear: Manually identify occurances of [unclear]text[/unclear]  ------------------------------------##
# #Create dataframe of unclear instances for outfit
# outfit_unclear <- data_clean %>% select(-long) %>%
#   mutate(unclear=str_extract_all(outfits_comment, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
#          unclear = ifelse(unclear == "character(0)", NA, unclear),
#          correct = rep("", nrow(data_clean)))%>% filter(!is.na(outfits_comment), !is.na(unclear)) %>%
#   unnest(unclear)
# 
# #Manually correct unclear instances
# fwrite(outfit_unclear, file="~/git/dspg2020amsoldier/data/outfit_unclear.csv", sep = ",") #export the unclear table to csv
# #researcher manually enters the correction in the correct column
# #---------------------------------------------------------------------------------------------------------------------##

# read in csv of unclear tag corrections for short outfits response. 
outfit_unclear <- fread("~/git/dspg2020amsoldier/data/outfit_unclear.csv", sep = ",") 
#loop through and corrects original dataset for short outfits response
for (i in 1:nrow(outfit_unclear)){
  j<-outfit_unclear$index[i]
  data_clean$outfits_comment[j] <- str_replace(data_clean$outfits_comment[j], "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", outfit_unclear$correct[i])
}

# #------------------ Long Unclear: Manually identify occurances of [unclear]text[/unclear] --------------------------------------##
# #Create dataframe of unclear instances for long response
# long_unclear <- data_clean %>% select(-outfits_comment) %>%
#   mutate(#long = str_replace_all(long, "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]", ""),#remove any unclear with no filler or with question mark
#          #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
#          unclear=str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
#          unclear = ifelse(unclear == "character(0)", NA, unclear),
#          correct = rep("", nrow(data_clean)))%>% filter(!is.na(long), !is.na(unclear)) %>%
#   unnest(unclear)
#fwrite(long_unclear, file="~/git/dspg2020amsoldier/data/long_unclear.csv", sep = ",") #export the unclear table to csv

##researcher manually enters the correction in the correct column
# #---------------------------------------------------------------------------------------------------------------------##

#researcher manually enters the correction in the correct column
long_unclear <- fread("~/git/dspg2020amsoldier/data/long_unclear.csv", sep = ",") #read the csv file back in.
for (i in 1:nrow(long_unclear)){#populate clean dataset with corrections
  j<-long_unclear$index[i]
  data_clean$long[j] <- str_replace(data_clean$long[j], "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", long_unclear$correct[i])
}
                                    
#remove unclear tags that occur alone [unclear] or [/unclear]
data_clean$outfits_comment <- str_replace_all(data_clean$outfits_comment, "\\[unclear\\]|\\[\\/unclear\\]", "")
data_clean$long <- str_replace_all(data_clean$long, "\\[unclear\\]|\\[\\/unclear\\]", "")



##### 6. Clean Brackets ###### 
# #------------------------------ Manually examine bracketed text --------------------------------------------------##
# # goal: replace the incorrect word with the correct bracketed word. In otherwords, bracketed word replaces its preceeding entity
# # ex: when it come to where a negro is alowed [allowed] in a white outfit than [then] i say to hell with the whole country"
# #     allowed replaces preceeding word alowed and then replaces preceeding word than
# 
# 
# # The code below has extracted all instances of a bracketed word along with the original text and its position in the original dataset.
# outfit_bracket <- data_clean %>% select(-long) %>%
#   mutate(outfits_comment = str_replace_all(outfits_comment, "\\[unclear\\]|\\[\\/unclear\\]", ""),#remove all unclear
#          bracket=str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])"), #identify unclear tag with text inside
#          bracket = ifelse(bracket == "character(0)", NA, bracket))%>% filter(!is.na(outfits_comment), !is.na(bracket))
# 
# long_bracket <- data_clean %>% select(-outfits_comment) %>%
#   mutate(long = str_replace_all(long, "\\[unclear\\]|\\[\\/unclear\\]", ""),#remove all unclear
#          bracket=str_extract_all(long, "(?=\\[).*?(?<=\\])"), #identify unclear tag with text inside
#          bracket = ifelse(bracket == "character(0)", NA, bracket))%>% filter(!is.na(long), !is.na(bracket))

# #manually correct bracketed instances
# fwrite(outfit_bracket, file="~/git/dspg2020amsoldier/data/outfit_bracket.csv", sep = ",") # export the bracket table to csv
# fwrite(long_bracket, file="~/git/dspg2020amsoldier/data/long_bracket.csv", sep = ",") # export the bracket table to csv
# #corrected files should be stored in the data folder with _corrected appended to original name
# #---------------------------------------------------------------------------------------------------------------------##


outfit_bracket_correct <- fread("~/git/dspg2020amsoldier/data/outfit_bracket_corrected.csv", sep = ",") #read the csv file back in.
for (i in 1:nrow(outfit_bracket_correct)){#populate clean dataset with corrections
  j<-outfit_bracket_correct$index[i]
  data_clean$outfits_comment[j] <- outfit_bracket_correct$correct[i]
}

long_bracket_correct <- fread("~/git/dspg2020amsoldier/data/long_bracket_corrected.csv", sep = ",") #read the csv file back in.
for (i in 1:nrow(long_bracket_correct)){#populate clean dataset with corrections
  j<-long_bracket_correct$index[i]
  data_clean$long[j] <- long_bracket_correct$correct[i]
}

##### 7. Manual Spell Checking ########
## -------------------------- Do spell checking -------------------------------------------------------##
# # unnest tokens using tidy text for both reponses.
# longword <- select(data.frame(unnest_tokens(filter(data_clean, !is.na(long)), word, long)), index, word)
# outfitword <- select(data.frame(unnest_tokens(filter(data_clean, !is.na(outfits_comment)), word, outfits_comment)), index, word)
# word.tm <- rbind(longword,outfitword) #combine tokens from both questions in one data.frame

# #get all unique words using tidytext
# words <- as.vector(unique(word.tm$word)) #store just unique occurances of words
# library(hunspell) #use hunspell to perform spell checking
# bad.words <- unique(unlist(hunspell(words))) #identify the bad words (n=3697)
# sugg.words <- unlist(lapply(hunspell_suggest(bad.words), function(x) x[1])) #suggested corrections
# 
# word.list <- as.data.frame(cbind(bad.words, sugg.words)) # get dataframe of bad words and their corrections
# 
# freq.word <- count(word.tm, word)
# freq.word <- inner_join(freq.word, word.list, by=c(word = "bad.words")) # n = 3370 total words to spell check
# spell_check <- freq.word %>% filter(n > 2) %>% select(-n) # (n=452) words to spell check | ones that occur more than twice
#fwrite(spell_check, file="~/git/dspg2020amsoldier/data/spell_check.csv", sep = ",") #export the spell check table to csv
##researcher corrects this list.
# #---------------------------------------------------------------------------------------------------------------------##


#read the csv file of correct spellings back in.
spell_check <- fread("~/git/dspg2020amsoldier/data/spell_check.csv", sep = ",") # (n=274)
spell_check <- mutate(spell_check, word = paste("\\b", word,"\\b", sep = "")) #so that stringr doesn't pick up on instances where it is part of another word

#replace any bad words with a suggested word
library(stringi)
data_clean$long <- stri_replace_all_regex(data_clean$long, spell_check$word, spell_check$sugg.words, vectorize_all = FALSE) 
data_clean$outfits_comment <- stri_replace_all_regex(data_clean$outfits_comment, spell_check$word, spell_check$sugg.words, vectorize_all = FALSE)



##### 8. Final Processing #####
#remove contractions : don't --> do not
data_clean <- data_clean %>% mutate(long = replace_contraction(long),
                                    outfits_comment = replace_contraction(outfits_comment))
# replace any empty response with NA
data_clean <- data_clean %>% mutate(long = ifelse(long==""|long==" ", NA,long),
                                    outfits_comment = ifelse(outfits_comment==""|outfits_comment==" ", NA,outfits_comment))



##### 9. Push clean data to database ####
dbWriteTable(conn, name = c("american_soldier", "survey_32_clean"), value=data_clean, overwrite=TRUE, row.names=FALSE)
dbDisconnect(conn)
