library(dplyr);library(ggplot2);library(data.table);library(tidyr);library(stringr)
library(tidytext);library(textstem);library(SnowballC);library(naniar);

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
  mutate(long = tolower(long), outfits_comment = tolower(outfits_comment), index= 1:nrow(data))


# eda
colnames(data)

# outfits_comment
# only look at white soldiers who have the response for the outfits comment
# lump "no reason" into NAs

na_list <- c("none", "[none]", "noone", "nnone", "[blank]", "n/a", "i", ".", "no comment", "no comments", "have none",
              "no reason", "no reasons", "left blank", "[no answer]", "[slash] [slash]", "0")

levels(as.factor(data$racial_group))
# only white soldiers responded to outfits comment
data %>% filter(!is.na(outfits_comment) & racial_group == "white") %>% count() # 1450
data %>% filter(!is.na(outfits_comment) & racial_group == "black") %>% count() # 0

# convert all responses to lower case
ldata <- data %>%
  mutate(
    outfits_comment = tolower(outfits_comment),
    long = tolower(long),
  )
# extract just black soldiers
black_soldiers <- ldata %>% filter(racial_group == "black")

# check that black soldiers don't have a response to outfits comment
# count of black soldier responses
nrow(black_soldiers) # 3464
sum(is.na(black_soldiers$outfits_comment)) # 3464, that matches

# check the number of NA responses for long for black soldiers
sum(is.na(black_soldiers$long)) # 0

# extract just white soliders
white_soldiers <- ldata %>% filter(racial_group == "white")

# count of white soldier reponses
nrow(white_soldiers) # 2324

# check the number of na values for the outfits comment
sum(is.na(white_soldiers$outfits_comment)) # 874

# check the number of na values for long for white soliders
sum(is.na(white_soldiers$long)) # 0

# number of outfits comment responses in na_list
ldata %>% filter(racial_group == "white" & outfits_comment %in% na_list) %>% count() # 90

## change any answer that indicates no response to NA ##
#ex: "none", "[None]", "0" some of this filtering has been done in the LDA.R file on Master branch

outfits_predicate <- data$racial_group == "white" & tolower(data$outfits_comment) %in% na_list
long_predicate <- tolower(data$long) %in% na_list

data_clean <- data %>%
  mutate(
    outfits_comment = ifelse(outfits_predicate, NA, outfits_comment),
    long = ifelse(long_predicate, NA, long)
  )

## Remove the following metatags: [paragraph], [insertion][/insertion], [circle][/circle], [underline][/underline] ##
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
                                   
                                    

# replace any empty response with NA
data_clean <- data_clean %>% mutate(long = ifelse(long==""|long==" ", NA,long), 
                                    outfits_comment = ifelse(outfits_comment==""|outfits_comment==" ", NA,outfits_comment))

# examine cleaned data
head(data_clean)

##### Examine bracketed text ######
# goal: replace the incorrect word with the correct bracketed word. In otherwords, bracketed word replaces its preceeding entity
# ex: when it come to where a negro is alowed [allowed] in a white outfit than [then] i say to hell with the whole country"
#     allowed replaces preceeding word alowed and then replaces preceeding word than

# EXCEPTIONS: however, due to the nature of text data there are exceptions to the rule....
# correcting abbreviations, account for punctuation in preceeding word : "i imagined i would get in the signal corps but instead i was placed in the m.p.[military police] escort guard co.[company]"
# punctuation only: "they just dont [don't] like to be separated from their friends [.]" this isn't replacing any entity, rather a supplement. 
#[sic] is used to indicate that the text has been transcribed verbatum, so I think we can just remove these. 
#since the bracketed words are a small proportion of corrections, we may have to just accept that there will be inaccuracies here and there. 

# The code below has extracted all instances of a bracketed word along with the original text and it's position in the original dataset.
outfit_bracket <- data_clean %>% select(-long) %>% 
  mutate(outfits_comment = str_replace_all(outfits_comment, "\\[unclear\\]|\\[\\/unclear\\]", ""),#remove all unclear 
         bracket=str_extract_all(outfits_comment, "(?=\\[).*?(?<=\\])"), #identify unclear tag with text inside
         bracket = ifelse(bracket == "character(0)", NA, bracket))%>% filter(!is.na(outfits_comment), !is.na(bracket)) %>%
  unnest(bracket)

long_bracket <- data_clean %>% select(-outfits_comment) %>% 
  mutate(long = str_replace_all(long, "\\[unclear\\]|\\[\\/unclear\\]", ""),#remove all unclear 
         bracket=str_extract_all(long, "(?=\\[).*?(?<=\\])"), #identify unclear tag with text inside
         bracket = ifelse(bracket == "character(0)", NA, bracket))%>% filter(!is.na(long), !is.na(bracket)) %>%
  unnest(bracket)