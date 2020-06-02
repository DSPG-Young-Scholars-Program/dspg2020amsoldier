set_session_variables <- function(codebook_file_path) {
  study_code <<- stringr::str_match(codebook_file_path, "[Cc][Oo][Dd][Ee][Bb][Oo][Oo][Kk]\\.(.+)\\.[Cc][Dd][Bb][Kk]")[, 2]
  question_file <<- paste0(study_code, "_questions.csv")
  answer_file <<- paste0(study_code, "_answers.csv")
}

# set_session_variables <- function(folder) {
#   codebook_folder <<- paste0(folder,"/Technical Documentation")
#   codebook_file_path <<- list.files(codebook_folder, pattern = "*.CDBK", full.names = TRUE)[1]
#   answer_folder <<- paste0(folder, "/Electronic Records")
#   study_code <<- stringr::str_match(codebook_file_path, "[Cc][Oo][Dd][Ee][Bb][Oo][Oo][Kk]\\.(.+)\\.[Cc][Dd][Bb][Kk]")[, 2]
#   question_file <<- paste0(study_code, "_questions.csv")
#   answer_file <<- paste0(study_code, "_answers.csv")
# }
