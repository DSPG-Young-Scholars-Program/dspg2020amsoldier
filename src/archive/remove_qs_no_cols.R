remove_questions_without_columns <- function(questions_ls) {
  questions_ls[!is.na(stringr::str_match(questions_ls, "[RACJUY]\\.\\sCOLS?\\."))]  
}

