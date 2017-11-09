#' Main method for calling other R scripts in this project

setwd("src")
sources <- c('scrape.R', 'clean.R', 'tidy.R', 'db.R', 'gs_out.R', 'summary.R')
lapply(sources, source)
setwd("../")


get_week <- function(week_num) {
  df <- scrape_week(week_num) %>% clean_week(week_num)
  write_raw_data(df)
  rst_df <- df %>% make_roster_df()
  df %>% make_match_df() %>% write_match()
  
  rnk_df <- scrape_player_data(week_num) %>% 
    clean_week_ranks(week_num) %>%
    add_ranks() 
  
  join_roster_ranks(rst_df,rnk_df) %>% write_roster()
}

get_all_player_data <- function(first_week,last_week) {
  df <- data.frame()
  for (i in seq_along(first_week:last_week)) {
    df <- get_week_player_data(i) %>%
      bind_rows(df)
  }
  df
}

get_week_player_data <- function(week_num) {
  scrape_player_data(week_num) %>%
    clean_week_ranks(week_num) %>%
    add_ranks()
}
#TODO explore nflscrapR https://www.r-bloggers.com/nfl-series/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29