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
  df <- suppressWarnings(dbReadTable(con, table)) %>% as.tibble() %>% select(-1,-2)
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
                    'Rank_Pos',
                    'Rank_Ovrl',
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
                    'FGM_0-19',
                    'FGM_20-29',
                    'FGM_30-39',
                    'FGM_40-49',
                    'FGM_50+',
                    'PAT',
                    'PAT_Miss',
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
  df <- get_data('matches') %>% num_to_lgl('win')
  colnames(df) <- c('Week', 'Team', 'Win', 'Opponent', 'Points', 'Net_vs_Proj', 'Bench_Points', 'Optimal_Points')
  df
}

get_raw_data <- function() {
  df <- get_data('raw_df')
  df <- num_to_lgl(df, c('win','bench'))
  colnames(df) <- c('Week','Team','Win','Opponent','Pos','Bench','Player','Proj','Points','Stats')
  df
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

