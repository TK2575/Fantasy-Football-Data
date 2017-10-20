#' Open Selenium session and scrape data from Yahoo Fantasy Football web pages
library('RSelenium')
library('purrr')

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

match_url <- function(week_id,team_id) {
  base_url <- 'https://football.fantasysports.yahoo.com/f1/25989/matchup?'
  paste0(base_url,'week=',week_id,'&mid1=',team_id)
}

scrape_data <- function(rd,week_id,team_id) {
  rd$client$navigate(match_url(week_id,team_id)) 
  rd$client$getPageSource()[[1]][1]
}

scrape_week <- function(week_id) {
  teams <- as.list(1:12)
  rd <- open_session()
  login(rd)
  results <- map(teams, .f = scrape_data, rd = rd, week_id = week_id)
  close_session(rd)
  return(results)
}