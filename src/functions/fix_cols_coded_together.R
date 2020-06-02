# COLUMNS CODED TOGETHER
fix_coded_together <- function(instructions, questions) {
  #browser()
  o <- stringr::str_match(instructions, "[\\s\\.]([1-9][0-9]?[0-9]?[A-Z]?).*?\\s.*?([1-9][0-9]?[0-9]?[A-Z]?).*?[Cc][Oo][Dd][Ee][Dd]\\s[Tt][Oo][Gg][Ee][Tt][Hh][Ee][Rr]")
  oo <- o[!is.na(o[,1]), , drop=FALSE]
  ooo <- oo[, 2:3, drop=FALSE]
  
  if (length(ooo[,1]) > 0) {
    for (i in 1:length(ooo[,1])) {
      w1 <- which(stringr::str_detect(questions, paste0("Q\\.", ooo[i, 1], "\\.")))
      w2 <- which(stringr::str_detect(questions, paste0("Q\\.", ooo[i, 2], "\\.")))
      questions[w1] <- paste(questions[w1], questions[w2])
      questions[w2] <- NULL
    }
  }
  questions
}