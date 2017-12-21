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
  
  r <- init_bind(m_df, b_df, teams) %>% add_result()
  
  c_nm <- c('Pos','Bench','Team','Result','Opponent','Player','Proj','Points','Stats')
  
  xr <- r %>% select(Pos, Bench, Team.x, Result.x, Team.y, Player.x, Proj.x, Pts.x, Stats.x)
  yr <- r %>% select(Pos, Bench, Team.y, Result.y, Team.x, Player.y, Proj.y, Pts.y, Stats.y)
  
  colnames(xr) <- c_nm
  colnames(yr) <- c_nm
  
  # filtering out rows where team is NA due to bye weeks
  xr %>% bind_rows(yr) %>% extract_all_pos() %>% filter(!is.na(Team))
}

clean_week <- function(html_list,week_num) {
  map(html_list,clean_match) %>% 
    ldply(data.frame) %>% 
    as_tibble() %>% 
    mutate(Week = week_num) %>% 
    select(Week, Team, Result, Opponent, Slot, Pos, Bench, Player, Proj, Points, Stats) %>% 
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
    select(-Player) %>%
    rename(Slot = Pos) %>%
    add_column(Pos = z, Player = y)
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
  i <- str_locate_all(pattern = '\\n', char) %>%
    unlist()
  
  char %>% 
    substr(i[1]+1,i[2]-1) %>%
    str_trim()
}

v_cln_plyr <- Vectorize(clean_plyr)

trim_first_name <- function(char) {
  l <- str_locate(pattern = ' ', char) %>%
    unlist()
  
  paste0(substr(char,1,1), '. ', substr(char,min(l)+1,nchar(char)))
}

v_trim_first_name <- Vectorize(trim_first_name)

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

add_result <- function(df) {
  scores <- df %>% filter(Pos == 'TOTAL', Bench == FALSE) %>% select(Pts.x, Pts.y)
  
  x <- 'Tie'
  y <- 'Tie'
  
  if (is.na(scores$Pts.x) | is.na(scores$Pts.y)) {
    x <- NA
    y <- NA
  }
  if (scores$Pts.x > scores$Pts.y) {
    x <- 'Win'
    y <- 'Loss'
  } else if (scores$Pts.y > scores$Pts.x) {
    y <- 'Win'
    x <- 'Loss'
  }
  
  df %>% mutate(Result.x = x, Result.y = y)
}

clean_week_ranks <- function(html_list,week) {
  qb <- html_list[['QB']] %>% extract_plyr_html_list()
  rb <- html_list[['RB']] %>% extract_plyr_html_list()
  wr <- html_list[['WR']] %>% extract_plyr_html_list()
  te <- html_list[['TE']] %>% extract_plyr_html_list()
  k <- html_list[['K']] %>% extract_plyr_html_list(mode='k')
  def <- html_list[['DEF']] %>% extract_plyr_html_list(mode='def')
  
  bind_rows(qb,rb,wr,te,k,def) %>% 
    mutate(Week = week)
}

extract_plyr_html_list <- function(html_list, mode='offense') {
  df <- data.frame()
  for (i in seq_along(html_list)) {
    new_rows <- html_list[[i]] %>%
      extract_plyr_html()
    if (mode == 'offense') {
      new_rows <- new_rows[-c(2:5,23)]
      colnames(new_rows) <- c('Player',
                              'Points',
                              'Perc_Owned',
                              'Rank_Proj',
                              'Rank_Ovrl',
                              'Pass_Yds',
                              'Pass_TD',
                              'Pass_Int',
                              'Rush_Att',
                              'Rush_Yds',
                              'Rush_TD',
                              'Rec_Tgt',
                              'Rec',
                              'Rec_Yds',
                              'Rec_TD',
                              'Ret_TD',
                              '2PT',
                              'Fum_Lost')
      df <- df %>%
        bind_rows(new_rows)
    } else if (mode == 'k') {
      new_rows <- new_rows[-c(2:5,8,22)]
      colnames(new_rows) <- c('Player',
                              'Points',
                              'Perc_Owned',
                              'Rank_Ovrl',
                              'FG_0-19',
                              'FG_20-29',
                              'FG_30-39',
                              'FG_40-49',
                              'FG_50+',
                              'FGM_0-19',
                              'FGM_20-29',
                              'FGM_30-39',
                              'FGM_40-49',
                              'FGM_50+',
                              'PAT',
                              'PAT_Miss')
      df <- df %>%
        bind_rows(new_rows)
    } else if (mode == 'def') {
      new_rows <- new_rows[-c(2:5,8,19)]
      colnames(new_rows) <- c('Player',
                              'Points',
                              'Perc_Owned',
                              'Rank_Ovrl',
                              'Pts_vs',
                              'Sack',
                              'Safety',
                              'Def_Int',
                              'Fum_Rec',
                              'Def_TD',
                              'Blk_Kick',
                              'Yds_Allow',
                              'Ret_TD')
      df <- df %>%
        bind_rows(new_rows)
    }
  }
  df <- as_tibble(df)
  df$Player <- v_cln_plyr(df$Player)
  if (mode != 'def') {
    df$Player <- v_trim_first_name(df$Player)
  }
  df$Perc_Owned <- v_cnvrt_perc(df$Perc_Owned)
  df <- df %>% mutate(Pos = v_ext_pos(Player))
  df$Player <- v_trim_plyr(df$Player)
  not_int <- grep(paste(c('Player','Pos'),collapse="|"),colnames(df))
  df[,-(not_int)] <- suppressWarnings(lapply(df[,-(not_int)], as.numeric))
  df
}

extract_plyr_html <- function(html) {
  df <- html %>%
    read_html %>%
    html_nodes('#players-table table') %>%
    html_table(fill=T)
  
  df <- df[[1]][-1]
  colnames(df) <- df[1,]
  df <- df[-1,]
  if ("Forecast" %in% colnames(df)) {
    df[["Forecast"]] <- NULL
  }
  df
}

cnvrt_perc <- function(chr) {
  chr %>%
    substr(0,nchar(chr)-1) %>%
    as.numeric()/100
}

v_cnvrt_perc <- Vectorize(cnvrt_perc)