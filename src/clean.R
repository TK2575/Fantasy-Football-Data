#' Clean html data retrieved from scrape.R 
library('purrr')
map(c("plyr", "RMySQL", "rvest", "stringr", "tidyr", "lubridate","dplyr","plotly","tibble"),
    library, 
    character.only = TRUE, 
    quietly = TRUE, 
    verbose = FALSE)

clean_match <- function(html) {
  teams <- get_teams(html) 
  m_df <- extract_html(html, 'matchup')
  b_df <- extract_html(html, 'bench')
  
  r <- init_bind(m_df, b_df, teams) %>% add_win()
  
  c_nm <- c('Pos','Bench','Team','Win','Opponent','Player','Proj','Points','Stats')
  
  xr <- r %>% select(Pos, Bench, Team.x, Win.x, Team.y, Player.x, Proj.x, Pts.x, Stats.x)
  yr <- r %>% select(Pos, Bench, Team.y, Win.y, Team.x, Player.y, Proj.y, Pts.y, Stats.y)
  
  colnames(xr) <- c_nm
  colnames(yr) <- c_nm
  
  xr %>% bind_rows(yr)
}

clean_week <- function(html_list,week_num) {
  map(html_list,clean_match) %>% 
    ldply(data.frame) %>% 
    as_tibble() %>% 
    mutate(Week = week_num) %>% 
    select(Week, Team, Win, Opponent, Pos, Bench, Player, Proj, Points, Stats)
}

clean_plyrs <- function(df) {
  x <- lapply(df$Plyr.x, clean_plyr) %>% unlist
  y <- lapply(df$Plyr.y, clean_plyr) %>% unlist
  
  df %>% 
    add_column(Player.x = x, Player.y = y) %>%
    select(-Plyr.x, -Plyr.y)
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

extract_html <- function(html, node) {
  c_nm <- c('Stats.x','Plyr.x','Proj.x','Pts.x','Pos.x','Pos','Pos.y','Pts.y','Proj.y','Plyr.y','Stats.y')
  
  bench <- FALSE
  if(node == 'bench') {
    bench <- TRUE
  }
  
  node_s <- switch(node, matchup = '#matchup table', bench = '#bench-table table')
  
  df <- html %>%
    read_html %>%
    html_nodes(node_s) %>%
    html_table(fill = TRUE)
  
  df <- df[[1]]
  colnames(df) <- c_nm
  
  df %>%
    as_tibble %>%
    mutate(Bench = bench) %>%
    clean_numerics()
  
}

prep_bind <- function(df) {
  c_nm <- c('Pos','Bench','Team','Opponent','Player','Proj','Points','Stats')
  
  df <- df %>%
    select(Pos, Bench, Team.x, Team.y, Player.x, Proj.x, Pts.x, Stats.x)
  
  colnames(df) <- c_nm
}

get_teams <- function(html) {
  teams_x <- '//*[contains(concat( " ", @class, " " ), concat( " ", "F-link", " " ))]/text()'
  
  teams <-
    html %>% read_html %>% html_nodes(xpath=teams_x) %>% html_text()
  
  teams[1:2]
}

init_bind <- function(m_df, b_df, teams) {
  m_df %>% 
    bind_rows(b_df) %>%
    clean_plyrs() %>% 
    mutate(Team.x = teams[1], Team.y = teams[2]) %>% 
    select(Pos, Bench, Team.x, Player.x, Proj.x, Pts.x, Stats.x, Team.y, Player.y, Proj.y, Pts.y, Stats.y)
}

add_win <- function(df) {
  scores <- df %>% filter(Pos == 'TOTAL', Bench == FALSE) %>% select(Pts.x, Pts.y)
  
  x <- FALSE
  y <- FALSE
  
  if(scores$Pts.x > scores$Pts.y) {
    x <- TRUE
  } else if (scores$Pts.y > scores$Pts.x) {
    y <- TRUE
  }
  
  df %>% mutate(Win.x = x, Win.y = y)
}

#TODO Debug current output of clean_week (only two teams?)
#TODO remove duplicates from clean_match output
#TODO Fix Bench player positions (roster spot versus position)
#TODO Expand stats column, separate script probably