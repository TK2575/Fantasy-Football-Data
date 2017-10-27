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
  
  xr %>% bind_rows(yr) %>% extract_all_pos()
}

clean_week <- function(html_list,week_num) {
  map(html_list,clean_match) %>% 
    ldply(data.frame) %>% 
    as_tibble() %>% 
    mutate(Week = week_num) %>% 
    select(Week, Team, Win, Opponent, Pos, Bench, Player, Proj, Points, Stats) %>% 
    distinct(.keep_all = TRUE)
}

clean_plyrs <- function(df) {
  x <- lapply(df$Plyr.x, clean_plyr) %>% unlist
  y <- lapply(df$Plyr.y, clean_plyr) %>% unlist
  
  df %>% 
    add_column(Player.x = x, Player.y = y) %>%
    select(-Plyr.x, -Plyr.y)
}

extract_all_pos <- function(df) {
  z <- lapply(df$Player, extract_pos) %>% unlist
  y <- lapply(df$Player,trim_plyr) %>% unlist
  
  df %>%
    add_column(Pos_new = z) %>%
    select(-Pos) %>%
    rename(Pos = Pos_new) %>%
    add_column(Plyr_new = y) %>% 
    select(-Player) %>%
    rename(Player = Plyr_new)
}

trim_plyr <- function(char) {
  k <- str_locate_all(pattern = '-', char) %>%
    unlist()
  
  char %>%
    substr(0,max(k)-1) %>%
    str_trim()
}

v_trim_plyr <- Vectorize(trim_plyr)

clean_plyr <- function(char) {
  #TODO i only resets via lapply?? - cause the method isn't vectorized
  i <- str_locate_all(pattern = '\\n', char) %>%
    unlist()
  
  char %>% 
    substr(i[1]+1,i[2]-1) %>%
    str_trim()
}

v_cln_plyr <- Vectorize(clean_plyr)

extract_pos <- function(char) {
  j <- str_locate_all(pattern = '-', char) %>%
    unlist()
  
  char %>%
    substr(max(j)+1,nchar(char)) %>%
    str_trim()
}

v_ext_pos <- Vectorize(extract_pos)

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

# TODO: merge with clean_week
clean_week_ranks <- function(html_list,week) {
  res <- data.frame(Player=character(),
                    Owner=character(),
                    Points=double(),
                    Perc_Owned=double(),
                    Rank_Ovrl=double(),
                    Pos=character(),
                    stringsAsFactors=F) %>%
    as_tibble()
  
  for (i in seq_along(html_list)) {
    for (j in seq_along(html_list[[i]])) {
      new_rows <- html_list[[i]][[j]] %>%
        clean_raw_ranks()
      
      res <- res %>%
        bind_rows(new_rows)
    }
  }
  
  res
}

#TODO: further testing with Flex, TE positions, validate data
clean_raw_ranks <- function(html) {
  df <- extract_rank_html(html)
  colnames(df) <- c('Player','Owner','Points','Perc_Owned','Rank_Ovrl')
  df <- as_tibble(df)
  df$Player <- v_cln_plyr(df$Player)
  df <- df %>% mutate(Pos = v_ext_pos(Player))
  df$Player <- v_trim_plyr(df$Player)
  df$Perc_Owned <- v_cnvrt_perc(df$Perc_Owned)
  int_col <- c('Points','Rank_Ovrl')
  df[,int_col] <- suppressWarnings(lapply(df[,int_col], as.numeric))
  df
}

#TODO: Merge extract_html and extract_rank_html
extract_rank_html <- function(html) {
  df <- html %>%
    read_html() %>%
    html_nodes('#players-table table') %>%
    html_table(fill = T)
  
  df <- df[[1]][c(2,5,8,9,11)]
  colnames(df) <- df[1,]
  df <- df[-1,]
  df
}

cnvrt_perc <- function(chr) {
  chr %>%
    substr(0,nchar(chr)-1) %>%
    as.numeric()/100
}

v_cnvrt_perc <- Vectorize(cnvrt_perc)

#TODO Expand stats column, separate script probably