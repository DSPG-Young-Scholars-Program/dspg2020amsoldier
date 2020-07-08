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

# outfits_comment
# only look at white soldiers who have the response for the outfits comment
# lump "no reason" into NAs

na_list <- c("none", "[none]", "noone", "nnone", "[blank]", "n/a", "i", ".", "no comment", "no comments", "have none",
              "no reason", "no reasons", "left blank", "[no answer]", "[slash] [slash]", "0")

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

# correcting unclear text
# outfit_unclear <- data_clean %>% select(-long) %>%
#   mutate(unclear=str_extract_all(outfits_comment, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
#          unclear = ifelse(unclear == "character(0)", NA, unclear),
#          correct = rep("", nrow(data_clean)))%>% filter(!is.na(outfits_comment), !is.na(unclear)) %>%
#   unnest(unclear)

#Manually correct unclear instances
#fwrite(outfit_unclear, file="~/git/dspg2020amsoldier/data/outfit_unclear.csv", sep = ",") #export the unclear table to csv
#researcher manually enters the correction in the correct column
outfit_unclear <- fread("~/git/dspg2020amsoldier/data/outfit_unclear.csv", sep = ",") #read the csv file back in.

#loops through and corrects original dataset :))))
for (i in 1:nrow(outfit_unclear)){
  j<-outfit_unclear$index[i]
  data_clean$outfits_comment[j] <- str_replace(data_clean$outfits_comment[j], "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", outfit_unclear$correct[i])
}

# long_unclear <- data_clean %>% select(-outfits_comment) %>%
#   mutate(#long = str_replace_all(long, "\\[unclear\\]\\[\\/unclear\\]|\\[unclear\\]\\s\\[\\/unclear\\]|\\[unclear\\]\\s*\\?{1,}\\s*\\[\\/unclear\\]", ""),#remove any unclear with no filler or with question mark
#          #Note: there may result in additional white space."do you think [unclear][/unclear] will win the war" -> "do you think  will win the war"
#          unclear=str_extract_all(long, "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])"), #identify unclear tag with text inside
#          unclear = ifelse(unclear == "character(0)", NA, unclear),
#          correct = rep("", nrow(data_clean)))%>% filter(!is.na(long), !is.na(unclear)) %>%
#   unnest(unclear)
#fwrite(long_unclear, file="~/git/dspg2020amsoldier/data/long_unclear.csv", sep = ",") #export the unclear table to csv
#researcher manually enters the correction in the correct column
long_unclear <- fread("~/git/dspg2020amsoldier/data/long_unclear.csv", sep = ",") #read the csv file back in.

for (i in 1:nrow(long_unclear)){#populate clean dataset with corrections
  j<-long_unclear$index[i]
  data_clean$long[j] <- str_replace(data_clean$long[j], "(?=\\[unclear\\]).*?(?<=\\[\\/unclear\\])", long_unclear$correct[i])
}
                                    
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

# MY WORK
levels(as.factor(outfit_bracket$bracket))

replace_list <- c("[allowed]", "[and]", "[competition]", "[company]", "[couldn't]", "[different]",
                  "[don't]", "[drop]", "[get]", "[jealousy]", "[morale]", "[more]", "[negroes]",
                  "[occasionally]", "[october]", "[peculiarities]",
                  "[separate]", "[should]", "[soldier]", "[southerners]", "[their]", "[then]",
                  "[they]", "[together]", "[too]", "[undesirable]", "[won't]", "[would]", "[wouldn't]")
insert_list <- c("[.]", "[/.]", "[/be]", "[/if]", "[any?]", "[be]", "[common]", "[it's]")
remove_list <- c("[affect?/unclear]", "[layes]", "[sic]", "[something]", "[underline/]", "[underlined]")
acronyms_list <- c("[military police escort guard company]", "[military police]")

# use this code to go through tags and decide what to do
# tag <- "[wouldn't]"
# tmp <- outfit_bracket %>% filter(bracket == tag)

# some weird cases:
# check index 5580, one bracketed word not picked up
# [layes] looks like it would be inserted, but I don't think that's a word?
# "[non commissioned officers]"? check! how would I solve this


resolveBracketedWords <- function(row) {
  word <- row["bracket"]
  if (word %in% replace_list) {
    replaceBracketedWord(row)
  } else if (word %in% insert_list) {
    insertBracketedWord(row)
  } else {
    removeBracketedWord(row)
  }
}

replaceBracketedWord <- function(row) {
  # extract word inside brackets
  word <- row["bracket"];
  start <- str_locate_all(pattern = '\\[', word);
  end <- str_locate_all(pattern = '\\]', word);
  word <- substr(word, start[[1]][1] + 1, end[[1]][1] - 1);


  split <- as.list(strsplit(row["outfits_comment"], '\\s+')[[1]])
  bracket_index <- match(c("[common]"), split)
  split[bracket_index - 1] <- split[bracket_index]
  split <- split[-bracket_index]
}

insertBracketedWord <- function(row) {
  # extract word inside brackets
  word <- row["bracket"];
  start <- str_locate_all(pattern = '\\[', word);
  end <- str_locate_all(pattern = '\\]', word);
  word <- substr(word, start[[1]][1] + 1, end[[1]][1] - 1);

  # split outfit comments into individual words
  split <- as.list(strsplit(row["outfits_comment"], '\\s+')[[1]]);
  bracket_index <- match(c(row["bracket"]), split);

  # replace bracketed word in text with unbracketed word
  split[bracket_index] <- word;

  reformattedComment <- paste(split, sep = ' ', collapse = ' ');
  return(reformattedComment);
}

removeBracketedWord <- function() {
  print("remove")
}

# replace list:
# replace the word before with word in brackets
# how to deal with spaces?
tmp <- outfit_bracket %>% filter(index == 5689)
test1 <- data_clean[3520, ]
test1$outfits_comment
split <- as.list(strsplit(test1$outfits_comment, '\\s+')[[1]])
bracket_index <- match(c("[common]"), split)
split[bracket_index - 1] <- split[bracket_index]
split <- split[-bracket_index]

# insert list
# insert bracketed word without modifying text in any other way
outfit_bracket %>%
# remove list
# remove bracketed word

apply(X = outfit_bracket, MARGIN = 1, FUN = resolveBracketedWords)
# acryonyms list
# same methodology as replace list, but probably have to count periods to determine what characters to replace

sdata <- c('a', 'b', 'c')
# use this command to reform strings
paste(split, sep = ' ', collapse = ' ')
a <- "hello"
substr(a, 1, 3)
test_case <- "[common]"
start <- str_locate_all(pattern = '\\[', test_case)
end <- str_locate_all(pattern = '\\]', test_case)
substr(test_case, start[[1]][1] + 1, end[[1]][1] - 1)
