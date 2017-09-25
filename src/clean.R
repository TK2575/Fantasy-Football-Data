#' Clean html data retrieved from scrape.R 

map(c("plyr", "RMySQL", "rvest", "stringr", "tidyr", "lubridate","dplyr","plotly","tibble","purrr"),
    library, 
    character.only = TRUE, 
    quietly = TRUE, 
    verbose = FALSE)

clean_match <- function(html) {
  teams_x <- '//*[contains(concat( " ", @class, " " ), concat( " ", "F-link", " " ))]/text()'
  matchup <- '#matchup table'
  bench <- '#bench-table table'
  names <- c('Stats.x','Plyr.x','Proj.x','Pts.x','Pos.x','Pos','Pos.y','Pts.y','Proj.y','Plyr.y','Stats.y')
  
  #TODO: Iterate
  m_df <-
    html %>% read_html %>% html_nodes(matchup) %>% html_table(fill = TRUE)
  b_df <- 
    html %>% read_html %>% html_nodes(bench) %>% html_table(fill = TRUE)
  teams <-
    html %>% read_html %>% html_nodes(xpath=teams_x) %>% html_text()
  
  m_df <- m_df[[1]]
  b_df <- b_df[[1]]
  teams <- teams[1:2]
  
  colnames(m_df) <- names
  colnames(b_df) <- names
  
  m_df <- m_df %>%
    as_tibble %>%
    clean_plyrs

  b_df <- b_df %>%
    as_tibble %>%
    clean_plyrs
  
  
  list(Matchup = m_df, Bench = b_df, Teams = teams)
}

clean_week <- function(html_list) {
  map(html_list,clean_match)
}

clean_plyrs <- function(df) {
  x <- lapply(df$Plyr.x, clean_plyr) %>% unlist
  y <- lapply(df$Plyr.y, clean_plyr) %>% unlist
  
  df %>% 
    add_column(Player.x = x, Player.y = y) %>%
    select(1, Player.x, 3:9, Player.y, 11)
}

clean_plyr <- function(char) {
  #TODO i only resets via lapply??
  i <- str_locate_all(pattern = '\\n', char) %>%
    unlist()
  
  char %>% 
    substr(i[1]+1,i[2]-1) %>%
    str_trim()
}

is_duplicate <- function(team_vec1, team_vec2) {
  result <- FALSE
  if (sum(match(team_vec1,team_vec2),na.rm=T) > 0) {
    result <- TRUE
  }
  return(result)
}

clean_numerics <- function(df) {
  columns <- c('Proj.x','Proj.y','Pts.x','Pts.y')
  for (i in columns) {
    df[[i]] <- suppressWarnings(as.numeric(df[[i]]))
    df[[i]][is.na(df[[i]])] <- 0
  }
  df
}

#TODO bind three results to single df
  #add bench indicator column (mutate, add logical)
  #bind rows
  #as tibble
  #clean plyrs (done)
  #clean numerics (done)
  #add teams, result
  #remove unneeded Pos columns


#TODO Expand stats column, separate script probably