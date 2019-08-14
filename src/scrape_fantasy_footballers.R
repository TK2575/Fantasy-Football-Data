library(RSelenium)
library(here)
library(config)
library(rvest)
library(tidyverse)
library(janitor)

fprof <- makeFirefoxProfile(
  list(
    browser.download.folderList = 2L, 
    browser.download.defaultFolder = here("temp"),
    browser.download.dir = here("temp"),
    browser.download.downloadDir = here("temp"),
    browser.download.lastDir = here("temp"),
    browser.download.manager.showWhenStarting = FALSE, 
    browser.helperApps.neverAsk.saveToDisk = "text/csv"
  )
)

start_local_server <- function() {
  rsDriver(
    port = 2525L, 
    browser = "firefox", 
    extraCapabilities = fprof,
    verbose = FALSE
  )
}

get_fantasy_footballers_projections <- function(rd) {
  url <- "https://www.thefantasyfootballers.com/2019-ultimate-draft-kit/"
  
  client <- rd$client
  client$navigate(url)
  
  # login
  credentials <- config::get(file = here("conf", "credentials.yml"))$fantasyfootballers
  user_field <- client$findElement('xpath', '//*[@id="user_login"]')
  password_field <- client$findElement('xpath', '//*[@id="user_pass"]')
  
  user_field$sendKeysToElement(list(credentials$username))
  password_field$sendKeysToElement(list(credentials$password, key = "enter"))
  
  # get each position for each expert
  
  experts <- c("andy","jason","mike")
  positions <- c("QB","RB","WR","TE")
  
  result <- NULL
  
  for (expert in experts) {
    projection_url <- NULL
    for (position in positions) {
      projection_url <- paste0(url, "udk-", expert, "s-projections/?position=", position)
      
      client$navigate(projection_url)
      
      xpath <- paste0('//*[@id="UDKProjectionsTable',position,'"]')
      table_elem <- client$findElement(using = "xpath", xpath)
      
      table <-
        table_elem$getPageSource() %>% 
        .[[1]] %>% 
        read_html() %>% 
        html_table() %>% 
        .[[1]] %>% 
        as_tibble() %>% 
        clean_names() %>% 
        mutate(source = paste0("Fantasy Footballers - ", expert),
               position = position)
      
      if (is.null(result)) {
        result <- table
      } else {
        result <-
          result %>% 
          bind_rows(table)
      }
    }
  }
  client$close()
  #TODO name cleanup (split name, team, bye)
  result
}