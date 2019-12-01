library(tidyverse)
library(here)

source_project <- function() {
  here::here("src") %>% setwd()
  sources <- c('scrape.R', 'clean.R', 'tidy.R', 'db.R', 'summary.R')
  map(sources, source)
  here::here() %>% setwd()
}

source_project()

