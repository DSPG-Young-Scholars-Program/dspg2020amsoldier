parse_answer_options <- function(codebook_dt = codebook_raw, rownum = 6, col_name = "codes_raw", code_list = c("1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "0.")) {
  code_list <- str_trim(code_list)
  str_lngth <- codebook_dt[rownum, nchar(get(col_name))]
  locs <- data.table(start = integer(), end = integer(), cd = character())
  for (i in 1:length(code_list)) {
    code <- code_list[i] %>% 
      str_replace_all("\\.", "\\\\\\.")
    start_end <- as.data.table(str_locate(codebook_dt[rownum, get(col_name)], code))
    start_end$cd <- code_list[i]
    locs <- rbindlist(list(locs, start_end))
  }
  
  vct <- locs$start
  vct <- vct[2:length(vct)]
  vct <- vct - 1
  vct <- c(vct, str_lngth)
  fnl_locs <- data.table(start = locs$start, end = vct)
  
  mylist <- list()
  for (i in 1:nrow(fnl_locs)) {
    y <- codebook_dt[rownum, .(s = str_sub(get(col_name), fnl_locs[i, 1], fnl_locs[i, 2]))]
    mylist <- c(mylist, y)
  }
  
  mylist
}
