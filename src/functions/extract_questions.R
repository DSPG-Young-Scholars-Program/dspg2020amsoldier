extract_questions <- function(lines) {
  
  question_indicies <- which(stringr::str_detect(lines, "^\\s*[QVN]\\.\\.?\\s?\\s?[0-9][0-9]?[0-9]?[a-zA-Z]?\\."))
  weird_question_indices <- which(stringr::str_detect(lines, "^\\s*V\\..*?Cross"))
  weird_question_indices_2 <- which(stringr::str_detect(lines, "QUESTION\\s[0-9][0-9]?[a-zA-Z]?\\sCONTINUED"))
  admin_qs_indicies <- which(stringr::str_detect(lines, "(^\\s*CARD\\.?\\s+$|^\\s*DECK\\.?(\\s|#|[0-9])+$|^\\s*BALLOT\\.?\\s+$|^\\s*FORM\\.?:?\\s+$)"))
  combined_indicies <- sort(c(question_indicies, weird_question_indices, weird_question_indices_2, admin_qs_indicies))
  combined_less_one <- combined_indicies - 1
  combined_less_one <- combined_less_one[2:length(combined_less_one)]
  end <- which(stringr::str_detect(lines, "[Cc][Oo][Ll].*[Nn][Oo][Tt]\\s[Uu][Ss][Ee][Dd]")) - 1
  if (length(end) == 0) end <- length(lines)
  combined_less_one[length(combined_less_one) + 1] <- max(end)
  index_ranges <- data.table::data.table(start = as.integer(combined_indicies), end = as.integer(combined_less_one))
  
  if (index_ranges[nrow(index_ranges), start] > index_ranges[nrow(index_ranges), end]) {
    index_ranges[nrow(index_ranges), end := length(lines)]
  }
  #browser()
  
  ret_ls <- list()
  for (i in 1:nrow(index_ranges)) {
    start <- index_ranges[i, 1][[1]]
    end <- index_ranges[i, 2][[1]]
    q_str <- paste(lines[start:end], collapse = "") %>% 
      stringr::str_replace_all("\\s+", " ") %>% 
      stringr::str_replace("^\\s+", "")
    ret_ls[i] <- list(q_str)
  }
  
  return(ret_ls)
}
