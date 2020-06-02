create_one_row_respondent <- function(answerlines) {
  #browser()
  answerlines_ls <- answerlines
  
  # nlines <- max(as.numeric(substr(answerlines_ls, 1, 1)))
  
  ansdt <- as.data.table(t(as.data.table(strsplit(answerlines_ls, "\n"))))
  ansdt[, V1 := stringr::str_replace_all(stringr::str_trim(V1), " ", "0")]
  ansdt[, idx := substr(V1, 5, 8)]
  #browser()
  sing <- ansdt[, .(nn = paste0(V1, collapse = "")), idx]
  
  # sing[, nn := stringr::str_replace_all(stringr::str_trim(nn, side = "right"), " ", "0")]
  
  return(sing[, .(nn)])
  
  # answerlines_no_sp <- stringr::str_replace_all(answerlines_ls, " ", "0")
  # 
  # 
  # ansdt[, V2 := stringr::str_replace_all(stringr::str_trim(V1), " ", "0")]
  # 
  # singlesdt <- data.table::data.table(recnum = seq_len(nrow(ansdt)), rec = "")
  # for (i in 1:nrow(ansdt)) {
  #   if (as.numeric(substr(answerlines_ls[i], 1, 1)) == 1) {
  #     current_row <- i
  #     singlesdt[current_row, rec := answerlines_ls[i]]
  #   } else {
  #     singlesdt[current_row, rec := paste0(answerlines_ls[current_row], answerlines[i])]
  #   }
  # }
  # singlesdt <- singlesdt[rec != ""]
  # singlesdt[, rec := stringr::str_replace_all(stringr::str_trim(rec), " ", "0")]
  # singlesdt[, .(rec)]
  # if (substr(answerlines_ls[1], 1, 1) != substr(answerlines_ls[2], 1, 1)) {
  #   ansdt <- as.data.table(t(as.data.table(strsplit(answerlines_ls, "\n"))))
  #   
  #   
  #   ansdt.firsts = ansdt[substr(V1, 1, 1) == 1]
  #   ansdt.seconds = ansdt[substr(V1, 1, 1) == 2]
  #   fnl <- cbind(ansdt.firsts, ansdt.seconds)   
  #   colnames(fnl) <- c("C1", "C2")
  #   fnl[, rec := paste0(C1, C2)]
  #   singlerecs <- fnl[, .(rec)]
  #   return(singlerecs)
  # } else {
  #   return(answerlines_ls)
  # }
}
