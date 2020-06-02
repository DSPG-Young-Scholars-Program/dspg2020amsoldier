# INTERVIEWER USED THE "E" CODE TO DELINEATE TWO DATA COLLECTION COLUMNS
# THESE ARE CHANGED TO "Q" QUESTIONS SO THAT THEY ARE PICKED UP CORRECTLY BY THE PARSER
codebook_file_path <- "data/original/Officers Orientation Study (AMS-85), November-December 1943/Technical Documentation/AMS-85_ Officers Orientation Study 11-12_1943, codebook.AMS0085.CDBK"
unfixed <-  readr::read_file(codebook_file_path)
fixed <- stringr::str_replace(unfixed, "E\\.\\sNUMBER OF ITEMS", "Q.74E. NUMBER OF ITEMS")
fixed <- stringr::str_replace(fixed, "E\\.\\sNUMBER OF MOVIES", "Q.100E. NUMBER OF MOVIES")
readr::write_file(unfixed, "data/original/Officers Orientation Study (AMS-85), November-December 1943/Technical Documentation/AMS-85_ Officers Orientation Study 11-12_1943, codebook.AMS0085.CDBK.unfixed")
readr::write_file(fixed, "data/original/Officers Orientation Study (AMS-85), November-December 1943/Technical Documentation/AMS-85_ Officers Orientation Study 11-12_1943, codebook.AMS0085.CDBK")