write_output_files <- function(dir = "data/working/", questions, prsd) {
  data.table::fwrite(questions, file = paste0("data/working/", question_file), col.names = FALSE)
  data.table::fwrite(prsd, file = paste0("data/working/", answer_file))
}
