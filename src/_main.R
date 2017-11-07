#' Main method for calling other R scripts in this project

setwd("src")
sources <- c('scrape.R', 'clean.R', 'tidy.R', 'db.R', 'gs_out.R', 'summary.R')
lapply(sources, source)
setwd("../")


#TODO: add player rank methods (merge, write where necessary)
get_week <- function(week_num) {
  df <- scrape_week(week_num) %>% clean_week(week_num)
  # TODO iterate multiple functions over df
  write_raw_data(df)
  df %>% make_roster_df() %>% write_roster()
  df %>% make_match_df() %>% write_match()
}
#TODO review accuracy of retrieved weekly data
#TODO explore nflscrapR https://www.r-bloggers.com/nfl-series/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29