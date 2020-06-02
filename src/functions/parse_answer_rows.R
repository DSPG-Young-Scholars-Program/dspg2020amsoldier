parse_answer_rows <- function(colwidths, answer_rows_file = "data/working/temp.fwf") {
  prsd <- read.fwf(answer_rows_file, colwidths)
  colnames(prsd) <- gsub("V", "R", colnames(prsd))
  return(prsd)
}