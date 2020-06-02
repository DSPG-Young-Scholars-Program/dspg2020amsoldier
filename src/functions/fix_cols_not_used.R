# FIX COLUMN XX NOT USED THAT DOESN'T INCLUDE COLUMN 80
fix_col_not_used <- function(instructions, codebook_lines) {
  not_used_cols <- unlist(stringr::str_match_all(instructions, "X.*?COL.*?\\s+?[0-9][0-9]?[a-zA-Z]?\\s+?NOT USED"))
  not_used_cols <- not_used_cols[grepl("80", not_used_cols) == FALSE]
  #browser()
  if (length(not_used_cols) > 0) {
    for (i in 1:length(not_used_cols)) {
      new_val <- stringr::str_replace(not_used_cols[i], "X.", "Q.0. NOT USED Y.")
      codebook_lines <- stringr::str_replace(codebook_lines, not_used_cols[i], new_val)
    }  
  }
  codebook_lines
}
