trim_to_question <- function(question_str) {
  stringr::str_match(questions, "^((Q|V|N|CARD|DECK|BALLOT|FORM)\\..*?)\\s[RACJUYX]\\.")[,2]
}
