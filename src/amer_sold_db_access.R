library("RPostgreSQL")

# connect to postgresql to get data (in rivanna)
conn <- dbConnect(drv = PostgreSQL(),
                  dbname = "sdad",
                  host = "10.250.124.195",
                  port = 5432,
                  user = Sys.getenv("db_userid"),
                  password = Sys.getenv("db_pwd"))
# query the bipartite edgelist data from github data
pulled_data <- dbGetQuery(conn, "SELECT *
                                 FROM american_soldier.survey_32_combined
                                 LIMIT 1000")
# disconnect from postgresql
dbDisconnect(conn) # make sure to run bc there is an upper limit of 15 connections to the db

head(pulled_data) # works when not selected T variables
