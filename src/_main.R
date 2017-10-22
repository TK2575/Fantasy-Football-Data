#' Main method for calling other R scripts in this project

setwd("src")
sources <- c('scrape.R', 'clean.R', 'tidy.R', 'db.R', 'gs_out.R', 'summary.R')
lapply(sources, source)
setwd("../")


get_week <- function(week_num) {
  df <- scrape_week(week_num) %>% clean_week(week_num)
  # TODO iterate multiple functions over df
  write_raw_data(df)
  write_roster(df)
  write_match(df)
}

#TODO review accuracy of retrieved weekly data
#TODO explore nflscrapR https://www.r-bloggers.com/nfl-series/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29