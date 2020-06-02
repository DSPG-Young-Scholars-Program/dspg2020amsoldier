update_parsed_names <- function(questions_ls, answers_prsd) {
  q_names <- stringr::str_match(questions_ls, "^([a-zA-Z0-9.]+)\\s")[,2] %>% 
    make.unique()
  colnames(answers_prsd) <- q_names
  answers_prsd
}
