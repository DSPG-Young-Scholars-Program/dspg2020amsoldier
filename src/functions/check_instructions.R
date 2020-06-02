check_instructions <- function(codebook, code = "X") {
  unlist(stringr::str_match_all(codebook, paste0("\\s", code, "\\.\\s.*")))
}
