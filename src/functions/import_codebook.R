library(readr)
import_codebook_file <- function(codebook_file_path) {
  ## create safe file path for current host system
  safe_file_path <- file_path_escapes(codebook_file_path)
  ## remove non-ascii characters before import
  system(command = paste0("tr -cd '\\11\\12\\15\\40-\\176' < ", safe_file_path, " > ", paste0(safe_file_path, ".ascii")))
  ## open raw text file
  #codebook_raw <- readr::read_file(paste0(codebook_file_path, ".ascii"))
  codebook_lines <- readr::read_lines(paste0(codebook_file_path, ".ascii")) %>% 
    stringr::str_remove("^[0-9]") %>% 
    stringr::str_remove("^_") %>%
    stringr::str_remove("^-") %>%
    stringr::str_remove("^.*ROPER.*PAGE.*$")
  codebook_lines <- codebook_lines[nchar(codebook_lines) > 0L]
  return(codebook_lines)
}

import_codebook <- function(folder) {
  ## set codebook path
  codebook_file_path <- list.files(folder, pattern = "*.CDBK", full.names = TRUE)[1]
  ## create safe file path for current host system
  safe_file_path <- file_path_escapes(codebook_file_path)
  ## remove non-ascii characters before import
  system(command = paste0("tr -cd '\\11\\12\\15\\40-\\176' < ", safe_file_path, " > ", paste0(safe_file_path, ".ascii")))
  ## open raw text file
  #codebook_raw <- readr::read_file(paste0(codebook_file_path, ".ascii"))
  codebook_lines <- readr::read_lines(paste0(codebook_file_path, ".ascii")) %>% 
    stringr::str_remove("^[0-9]") %>% 
    stringr::str_remove("^_") %>%
    stringr::str_remove("^-") %>%
    stringr::str_remove("^.*ROPER.*PAGE.*$")
  codebook_lines <- codebook_lines[nchar(codebook_lines) > 0L]
  return(codebook_lines)
}
