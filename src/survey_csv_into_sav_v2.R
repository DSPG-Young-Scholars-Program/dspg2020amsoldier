# LOAD LIBRARIES
source("src/libraries.R")

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)

csvs <- list.files("data/working/", pattern = "*s.csv")
survey_codes <- unique(str_match(csvs, "(AMS.+)_")[,2])

for (j in 1:length(survey_codes)) {
  print(paste("workin on", survey_codes[j]))
  files <- list.files("data/working/", pattern = paste0(survey_codes[j], ".*s.csv"), full.names = T)
  answerscsv <- read.csv(files[1], stringsAsFactors = F)
  questionscsv <- read.csv(files[2], stringsAsFactors = F, header = F)
  
  # create data.table of raw questions
  codebook_raw <- 
    data.table(
      id = 1:nrow(questionscsv),
      raw = questionscsv[,1]
    ) %>% 
    # get question nickname
    .[, q_nickname := str_replace(names(answerscsv), "\\.$", "")] %>% 
    # get answer options
    .[, codes_raw := str_match(raw, "\\s[0-9]?[0-9]?[0-9X]\\..*")] %>% 
    # parse codes
    .[, codes := str_match_all(codes_raw, "\\s[0-9X]{1,3}\\.")] %>% 
    # fix parentheses for future regex
    #.[, question := str_replace_all(question, "\\(", "\\\\\\(")] %>%
    .[, codes_raw := str_replace_all(codes_raw, "\\(", "\\\\\\(")] %>%
    #.[, question := str_replace_all(question, "\\)", "\\\\\\)")] %>% 
    .[, codes_raw := str_replace_all(codes_raw, "\\)", "\\\\\\)")] %>% 
    # fix asterics for future regex
    #.[, question := str_replace_all(question, "\\*", "\\\\\\*")] %>% 
    .[, codes_raw := str_replace_all(codes_raw, "\\*", "\\\\\\*")] %>% 
    # parse question
    .[, question := str_remove(str_remove(str_remove(raw, q_nickname), codes_raw), "\\.")] %>%
    # fill empty questions with q_nickname
    .[str_trim(question) == "", question := q_nickname]
    
  
  
  # create a data table with no columns
  dt_out <- data.table()
  # then for each question
  for (i in 1:nrow(codebook_raw)) {
    print(i)
    # get column name
    col_name <- codebook_raw[i,]$q_nickname
    # get question
    question <- codebook_raw[i,]$question
    # get answers
    answers <- as.numeric(answerscsv[, i])
    # get answer options
    answer_options <- parse_answer_options(codebook_dt = codebook_raw, 
                                           rownum = i, 
                                           col_name = "codes_raw", 
                                           code_list = unlist(codebook_raw[i,]$codes))
    # split answer options to numbers and texts and create a named vector
    split_nums_text <- str_match(answer_options, "^(.+?)\\s(.+)")
    answer_nums <- unique(as.numeric(str_trim(split_nums_text[,2])))
    answer_texts <- str_trim(split_nums_text[,3])
    if (length(answer_nums) != length(answer_texts)) {
      answer_nums <- NULL
      answer_texts <- NULL
    }
    names(answer_nums) <- answer_texts
    # create a labelled column (using haven)
    lbld_col <- haven::labelled(x = answers, labels = answer_nums, label = question)
    # add labelled column to data table  
    dt_out[, eval(col_name) := lbld_col]
  }
  
  # write spss file
  write_sav(dt_out, paste0("data/working/", survey_codes[j],".sav"))
}



# answerscsv <- read.csv("data/working/AMS0035_answers.csv", stringsAsFactors = F)
# questionsscsv <- read.csv("data/working/AMS0035_questions.csv", stringsAsFactors = F, header = F)


