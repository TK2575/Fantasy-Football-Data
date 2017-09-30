#' Main method for calling other R scripts in this project

setwd("src")
sources <- c('scrape.R', 'clean.R','output.R')
lapply(sources, source)
setwd("../")


get_week <- function(week_num) {
  scrape_week(week_num) %>%
    clean_week(week_num)
}

#TODO review accuracy of retrieved weekly data