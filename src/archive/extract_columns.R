extract_column_widths <- function(raw_text) {
  allcols <- stringr::str_extract_all(raw_text, "COLS?\\.\\s[0-9]+-?[0-9]?[0-9]?")
  allcolsdt <- data.table::as.data.table(allcols)
  allcolsdt[, V2 := as.numeric(stringr::str_extract_all(V1, "\\d+")[[1]][1]), V1]
  allcolsdt[, V3 := as.numeric(stringr::str_extract_all(V1, "\\d+")[[1]][2]), V1]
  allcolsdt[is.na(V3), V4 := 1]
  allcolsdt[!is.na(V3), V4 := V3 - V2 + 1]
  colwidths <- allcolsdt[, V4]
  return(colwidths)
}
