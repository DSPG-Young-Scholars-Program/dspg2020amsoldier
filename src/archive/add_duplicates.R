# add duplicate rows in order
add_duplicate_rows <- function(df, rownum, dupnum) {
  dt <- data.table::as.data.table(df)
  dt1 <- dt[1:rownum, ]
  dt2 <- dt[(rownum + 1): nrow(df), ]
  for (i in 1:dupnum) {
    dup_dt <- dt[rownum, ]
    dt1 <- data.table::rbindlist(list(dt1, dup_dt))
  }
  dtf <- rbindlist(list(dt1, dt2))
  return(dtf)
}
