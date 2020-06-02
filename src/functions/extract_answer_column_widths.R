extract_answer_column_widths <- function(questions_ls) {
  #browser()
  unlist(stringr::str_match_all(questions_ls, "[RACJUY]\\.\\s\\s?[Cc][Oo][Ll][Ss]?\\.?\\s[0-9]+-?[0-9]?[0-9]?")) %>% 
    data.table::data.table(V1 = .) %>% 
    .[, V2 := as.numeric(stringr::str_extract_all(V1, "\\d+")[[1]][1]), V1] %>% 
    .[, V3 := as.numeric(stringr::str_extract_all(V1, "\\d+")[[1]][2]), V1] %>% 
    .[is.na(V3), V4 := 1] %>% 
    .[!is.na(V3), V4 := V3 - V2 + 1] %>% 
    .[, V4]  
}
