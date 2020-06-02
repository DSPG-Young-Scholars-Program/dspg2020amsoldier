qs_n_as_1 <- find_all_answer_and_codebook_files()

qs_n_as_2 <- find_all_answer_and_codebook_files(start_path = "data/test1/Survey Codebooks, Aggregate Data, and ASCII Files/")

library(tools)
for (i in 1:nrow(qs_n_as_1)) {
  if (md5sum(qs_n_as_1[i, "codebook_file"][[1]]) == md5sum(qs_n_as_2[i, "codebook_file"][[1]])) {
    #print("TRUE")
  } else {
    print("FALSE")
    print(qs_n_as_1[i, "codebook_file"][[1]])
    print(qs_n_as_2[i, "codebook_file"][[1]])
  }
}
