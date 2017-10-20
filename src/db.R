#' Take raw cleaned scrape data and store to DB for retrieval and mass output
#' https://github.com/TK2575/fb_2017/blob/master/src/db.R

library(RMySQL)

write_week <- function(df) {
  write_to_db(df, 'raw_df')
}

write_to_db <- function(df, table_name) {
  con <- connect()
  lgl_c <- sapply(df, is.logical)
  df[,lgl_c] <- lapply(df[,lgl_c], as.numeric)
  
  dbWriteTable(con,
               value = df,
               name = table_name,
               append = TRUE,
               row.names = FALSE)
  dbDisconnect(con)
}

get_tables <- function() {
  con <- connect()
  results <- dbListTables(con)
  dbDisconnect(con)
  results
}

connect <- function() {
  dbConnect(
    RMySQL::MySQL(),
    dbname = "2017_ff",
    host = "localhost",
    port = 3306,
    user = "2017_ff",
    password = "2017_ff"
  )
}
