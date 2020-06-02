
find_all_answer_and_codebook_files <- function(start_path = "data/original") {
  all_answer_files <- list.files(path = start_path, pattern = "*.CLEAN", full.names = TRUE, recursive = TRUE)
  answer_and_codebook_files <- data.table::data.table(answer_file = all_answer_files, codebook_file = "")
  
  for (i in 1:nrow(answer_and_codebook_files)) {
    basenm <- basename(tools::file_path_sans_ext(answer_and_codebook_files[i, answer_file]))
    study_num <- stringr::str_extract(basenm, "^.+? ")
    cdbk_file <- list.files(path = start_path, recursive = T, full.names = T, pattern = paste0(study_num, ".*CDBK"))[1]
    answer_and_codebook_files[i, codebook_file := cdbk_file]
  }  
  return(answer_and_codebook_files)
}

#dt <- find_all_answer_and_codebook_files()
