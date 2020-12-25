#' Take raw cleaned scrape data and store to DB for retrieval and mass output

library(RMySQL)

write_raw_data <- function(df) {
  write_to_db(df, 'raw_df')
}

write_roster <- function(df) {
  write_to_db(df, 'roster')
}

write_expanded_roster <- function(df) {
  write_to_db(df, 'roster_expanded')
}

write_match <- function(df) {
  write_to_db(df, 'matches')
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

get_data <- function(table) {
  con <- connect()
  df <- suppressWarnings(dbReadTable(con, table)) %>% as_tibble() %>% select(-1,-2)
  dbDisconnect(con)
  df[df == ''] <- NA
  df
}

num_to_lgl <- function(df, col) {
  for (i in col) {
    df[[i]] <- as.logical(df[[i]])
  }
  df
}

get_roster_data <- function() {
  df <- get_data('roster') %>% num_to_lgl('bench')
  colnames(df) <- c('Week', 'Team', 'Bench', 'Slot', 'Pos', 'Player', 'Points', 'Proj', 'Stats')
  df
}

get_expanded_roster_data <- function() {
  df <- get_data('roster_expanded') %>% num_to_lgl('bench')
  colnames(df) <- c('Week',
                    'Team',
                    'Bench',
                    'Slot',
                    'Pos',
                    'Player',
                    'Points',
                    'Proj',
                    'Rank_Ovrl',
                    'Rank_Proj',
                    'Rank_Pos',
                    'Perc_Owned',
                    'Pass_Yds',
                    'Pass_TD',
                    'Pass_Int',
                    'Rush_Att',
                    'Rush_Yds',
                    'Rush_TD',
                    'Rec_Tgt',
                    'Rec',
                    'Rec_Yds',
                    'Rec_TD',
                    'Ret_TD',
                    '2PT',
                    'Fum_Lost',
                    'FG_0-19',
                    'FG_20-29',
                    'FG_30-39',
                    'FG_40-49',
                    'FG_50+',
                    'PAT',
                    'Pts_vs',
                    'Sack',
                    'Safety',
                    'Def_Int',
                    'Fum_Rec',
                    'Def_TD',
                    'Blk_Kick',
                    'Yds_Allow')
  df
}

get_matches_data <- function() {
  df <- get_data('matches') %>% num_to_lgl('playoffs')
  colnames(df) <- c('Week', 'Team', 'Win', 'Opponent', 'Points', 'Net_vs_Proj', 'Bench_Points', 'Optimal_Points', 'Playoffs')
  df
}

get_raw_data <- function() {
  df <- get_data('raw_df')
  df <- num_to_lgl(df, c('bench'))
  colnames(df) <- c('Week','Team','Result','Opponent','Slot','Pos','Bench','Player','Proj','Points','Stats')
  df
}

connect <- function() {
  ipaddress <- config::get(file = here::here("conf", "credentials.yml"))$network$ipaddress
  RMySQL::dbConnect(
    RMySQL::MySQL(),
    dbname = "2020_ff",
    host = ipaddress,
    port = 3306,
    user = "2020_ff",
    password = "2020_ff"
  )
}

