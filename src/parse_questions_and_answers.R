# LOAD LIBRARIES
source("src/libraries.R")

# LOAD FUNCTIONS
functions <- list.files("src/functions/", full.names = TRUE)
for (f in functions) source(f)

# RESET LOG
rm("logs/log")

parse_questions_and_answers <- function(codebook_file_path, answer_file_path) {
  print(codebook_file_path)
  readr::write_lines(codebook_file_path, "logs/log", append = TRUE)
  # # SET STUDY FOLDER
  # study_folder <- study_folder_path
  
  # CREATE SESSION VARIABLES
  set_session_variables(codebook_file_path)
  # IMPORT CODEBOOK
  print("import codebook")
  codebook_lines <- import_codebook_file(codebook_file_path)
  # IMPORT ANSWER FILE
  print("import answers")
  answerlines <- import_answer_file(answer_file_path)
  # CHECK INSTRUCTIONS
  instructions <- check_instructions(codebook_lines)
  print(instructions)
  # FIX NOT USED COLUMNS
  codebook_lines <- fix_col_not_used(instructions, codebook_lines)
  # EXTRACT QUESTIONS
  print("extract questions")
  questions <- extract_questions(codebook_lines)
  # CHECK FOR JOINED QUESTIONS
  print("fix combined questions")
  questions <- fix_coded_together(instructions, questions)
  # PARSE TO QUESTIONS AND OPTIONS
  print("parse questions")
  questions <- parse_questions(questions)
  # EXTRACT QUESTION COLUMN WIDTHS
  print("extract column widths")
  colwidths <- extract_answer_column_widths(codebook_lines)
  # TEST QUESTION EXTRACTION OUTPUT
  if (nrow(questions) != length(colwidths)) stop("NUMBER OF QUESTIONS DOES NOT MATCH NUMBER OF COLUMNS!")
  # CREATE ONE ROW PER RESPONDENT
  singlerows <- create_one_row_respondent(answerlines)
  write.table(singlerows, file = "data/working/temp.fwf", row.names = F, col.names = F, quote = F)
  # PARSE EACH ANSWER ROW WITH COLWIDTHS
  print("parse answers with col widths")
  prsd <- parse_answer_rows(colwidths, answer_rows_file = "data/working/temp.fwf")
  print(prsd[1,])
  write.table(prsd[1,], "logs/log", append = TRUE, row.names = F, col.names = F)
  # UPDATE PARSED ANSWERS WITH QUESTION NAMES
  prsd <- update_parsed_names(questions$V1, prsd)
  # WRITE OUTPUT FILES
  write_output_files(dir = "data/working/", questions, prsd)
  # CLEANUP
  unlink("data/working/temp.fwf")
}

# FIND ALL ANSWER AND CODEBOOK FILES AND PARSE
qs_n_as <- find_all_answer_and_codebook_files()
for (i in 1:nrow(qs_n_as)) {
  parse_questions_and_answers(qs_n_as[i, codebook_file], qs_n_as[i, answer_file])
}

