library(stringdist)

library("RPostgreSQL")
# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data
S32 <- dbGetQuery(conn, "SELECT *
                  FROM american_soldier.survey_32_combined")
# disconnect from postgresql
dbDisconnect(conn)

S32N = S32 %>% filter(racial_group == "black")
S32W = S32 %>% filter(racial_group == "white")


# text mining - mo --------------------------------------------------------
#T5 = long_comment, T3 = outfits_comment, T4 = long_comment
# this will create data frames out out of text
text77_df <- tibble(row = 1:nrow(S32W), text = S32W$outfits_comment) # Written response to "should soldiers be in separate outfits?"
text78_df <- tibble(row = 1:nrow(S32W), text = S32W$long_comment) # Written response on overall thoughts on the survey
textn_df <- tibble(row = 1:nrow(S32N), text = S32N$long_comment) # Written response to "should soldiers be in separate outfits?"

# look into duplicates
potential_duplicates <- sapply(textn_df$text, function(i) {
  similarity_scores <- stringsim(i, textn_df$text);
  count <- sum(similarity_scores[similarity_scores >= 0.5], na.rm = TRUE);
  return(count);
});
sim1 <- stringsim(textn_df[1, ]$text, textn_df$text)
sum(sim1[sim1 > 0.5], na.rm = TRUE)
print(1)