library(tidyverse)
library(here)

source_project <- function() {
  here::here("src") %>% setwd()
  sources <- c('scrape.R', 'clean.R', 'tidy.R', 'db.R', 'summary.R', 'csv_out.R')
  map(sources, source)
  here::here() %>% setwd()
}

source_project()

get_week_matches <- function(week_num) {
  df <-
    scrape_week(week_num)
  
  saveRDS(df, 
          here::here("temp", paste0("week",week_num,".Rds")))
  
  df2 <- 
    df %>% 
    clean_week(week_num)
  
  df <- df2
  rm(df2)
  
  saveRDS(df, 
          here::here("temp", paste0("week",week_num,".Rds")))
}

get_week_players <- function(week_num) {
  df <-
    scrape_player_data(week_num)
  
  saveRDS(df, 
          here::here("temp", paste0("week",week_num,"_players.Rds")))
  
  df <- 
    df %>% 
    clean_week_ranks(week_num) %>% 
    add_ranks()
  
  saveRDS(df, 
          here::here("temp", paste0("week",week_num,"_players.Rds")))
}

get_week <- function(week_num) {
  get_week_matches(week_num)
  Sys.sleep(10)
  get_week_players(week_num)
  Sys.sleep(10)
}

write_week <- function(week_num) {
  df <- readRDS(here::here("temp", paste0("week",week_num,".Rds")))
  
  write_raw_data(df)
  
  df %>% 
    make_roster_df() %>% 
    write_roster()
  
  df %>% 
    make_match_df() %>% 
    write_match()
  
  df <- readRDS(here::here("temp", paste0("week",week_num,".Rds")))
  
  get_roster_data() %>% 
    filter(Week == week_num) %>% 
    join_roster_ranks(df) %>% 
    write_expanded_roster()
}

#get_week(1)
#purrr::map(14:16, get_week)

#write_week_to_csv()
