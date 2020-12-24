#' Open Selenium session and scrape data from Yahoo Fantasy Football web pages
library('RSelenium')
library('purrr')
library('config')
library('rvest')
library(here)

base_url <- "https://football.fantasysports.yahoo.com/f1/109217/"

open_session <- function() {
  ipaddress <- config::get(file = here::here("conf", "credentials.yml"))$network$ipaddress
  rd <- remoteDriver(
    remoteServerAddr = ipaddress,
    port = 4445L
  )
  
  rd$open()
  rd
}

login <- function(rd) {
  credentials <- config::get(file = here::here("conf", "credentials.yml"))$yahoo
  rd$navigate(base_url)
  webElem <- rd$findElement('xpath', '//*[@id="login-username"]')
  webElem$sendKeysToElement(list(credentials$username, key = "enter"))
  Sys.sleep(10)
  #rd$setImplicitWaitTimeout(10000)
  webElem2 <- rd$findElement('xpath', '//*[@id="login-passwd"]')
  webElem2$sendKeysToElement(list(credentials$password, key = 'enter'))
  #rd$setImplicitWaitTimeout(4000)
  Sys.sleep(10)
}

close_session <- function(rd) {
  rd$close()
}

match_url <- function(week,team_id) {
  paste0(base_url,'matchup?week=',week,'&mid1=',team_id)
}

source_page <- function(rd) {
  rd$getPageSource()[[1]][1]
}

scrape_match_data <- function(rd,week,team_id) {
  rd$navigate(match_url(week,team_id)) 
  source_page(rd)
}

player_url <- function(pos,week,page) {
  url <- paste0(base_url,'players?status=ALL&pos=',pos,'&cut_type=9&stat1=S_W_',week,'&myteam=0&sort=AR&sdir=1')
  if (page > 1) {
    count <- (page-1) * 25
    url <- paste0(url,'&count=',count)
  }
  url
}

scrape_player_page <- function(rd,pos,week,page) {
  i <- 0
  l <- 0
  result <- NULL
  
  while (i < 5) {
    result <- try_scrape_player_page(rd,pos,week,page)
    l <- result %>% read_html() %>% html_nodes('#players-table table') %>% length()
    if (l > 0) {
      i <- 5
    } else {
      i < i + 1
      Sys.sleep(10)
    }
  }
  
  if (result %>% is.null()) {
    stop(paste0("Couldn't scrape valid player page: pos=",pos,", week=",week,", page=",page))
  } 
  result
}

try_scrape_player_page <- function(rd,pos,week,page) {
  rd$navigate(player_url(pos,week,page))
  source_page(rd)
}

scrape_player_pages <- function(rd,pos,week,pages) {
  map(as.list(1:pages), .f = scrape_player_page, rd = rd, pos = pos, week = week)
}

scrape_player_data <- function(week) {
  rd <- open_session()
  login(rd)
  
  qb <- scrape_player_pages(rd,'QB',week,3)
  rb <- scrape_player_pages(rd,'RB',week,17)
  wr <- scrape_player_pages(rd,'WR',week,17)
  te <- scrape_player_pages(rd,'TE',week,9)
  k <- scrape_player_pages(rd,'K',week,3)
  dst <- scrape_player_pages(rd,'DEF',week,2)
  
  close_session(rd)
  res <- list(qb,rb,wr,te,k,dst)
  names(res) <- c('QB','RB','WR','TE','K','DEF')
  res
}

scrape_week <- function(week) {
  rd <- open_session()
  login(rd)
  results <- map(as.list(1:14), .f = scrape_match_data, rd = rd, week = week)
  close_session(rd)
  results
}