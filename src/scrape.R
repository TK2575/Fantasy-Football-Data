#' Open Selenium session and scrape data from Yahoo Fantasy Football web pages
library('RSelenium')
library('purrr')

base_url <- 'https://football.fantasysports.yahoo.com/f1/25989/'

open_session <- function() {
  rsDriver(verbose = FALSE)  
}

login <- function(rd) {
  credentials <- read.csv("conf/config.csv", stringsAsFactors = FALSE)
  rd$client$navigate('https://football.fantasysports.yahoo.com/f1/25989/12')
  webElem <- rd$client$findElement('xpath', '//*[@id="login-username"]')
  webElem$sendKeysToElement(list(credentials[1,1], key = "enter"))
  rd$client$setImplicitWaitTimeout(10000)
  webElem2 <- rd$client$findElement('xpath', '//*[@id="login-passwd"]')
  webElem2$sendKeysToElement(list(credentials[1,2], key = 'enter'))
}

close_session <- function(rd) {
  rd$client$close()
  rd$server$stop()  
}

match_url <- function(week,team_id) {
  paste0(base_url,'matchup?week=',week,'&mid1=',team_id)
}

source_page <- function(rd) {
  rd$client$getPageSource()[[1]][1]
}

scrape_match_data <- function(rd,week,team_id) {
  rd$client$navigate(match_url(week,team_id)) 
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
  rd$client$navigate(player_url(pos,week,page))
  source_page(rd)
}

scrape_player_pages <- function(rd,pos,week,pages) {
  map(as.list(1:pages), .f = scrape_player_page, rd = rd, pos = pos, week = week)
}

#TODO: Merge with scrape_week()
scrape_player_data <- function(week) {
  rd <- open_session()
  login(rd)
  
  qb <- scrape_player_pages(rd,'QB',week,2)
  rb <- scrape_player_pages(rd,'RB',week,5)
  wr <- scrape_player_pages(rd,'WR',week,5)
  te <- scrape_player_pages(rd,'TE',week,3)
  flx <- scrape_player_pages(rd,'W%2FR%2FT',week,10)
  k <- scrape_player_pages(rd,'K',week,2)
  dst <- scrape_player_pages(rd,'DEF',week,2)
  
  close_session(rd)
  list(qb,rb,wr,te,flx,k,dst)
}

scrape_week <- function(week) {
  rd <- open_session()
  login(rd)
  results <- map(as.list(1:12), .f = scrape_match_data, rd = rd, week = week)
  #TODO add scrape_player_data
  close_session(rd)
  results
}