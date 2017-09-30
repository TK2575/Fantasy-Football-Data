#' Takes data from clean.R and preps, writes to Google Sheets

make_team_df <- function(df) {
  df %>% 
    filter(!is.na(Player) & !is.na(Pos)) %>% 
    select(Week, Team, Bench, Pos, Player, Points, Proj, Stats)
}

make_match_df <- function(df) {
  #Points
  #Bench Points
  #Net vs Proj
  #Optimal Points
  
  tot_points <- df %>%
    filter(Bench == FALSE & is.na(Player) & is.na(Pos)) %>%
    mutate(Net_vs_Proj = Points-Proj) %>%
    select(Week, Team, Points, Net_vs_Proj)
  
  bench_points <- df %>% 
    filter(Bench == TRUE & !is.na(Player) & !is.na(Pos)) %>%
    group_by(Week, Team) %>% 
    summarize(Bench_Points = sum(Points))
  
  qb_points <- max_for_pos(df,'QB')
  
  rb_points <- max_for_2pos(df,'RB')
  
  wr_points <- max_for_2pos(df, 'WR')
  
  te_points <- max_for_pos(df,'TE')
  
  flex_points <- df %>%
    filter(!is.na(Player) & Pos %in% c('RB','WR','TE') & !is.na(Points)) %>%
    group_by(Week, Team, Pos) %>% 
    arrange(Week,Team, desc(Points)) %>%
    mutate(rank = rank(desc(Points))) %>%
    filter((rank > 2 & Pos %in% c('RB','WR')) | (rank > 1 & Pos == 'TE')) %>%
    group_by(Week,Team) %>%
    summarize(Flex_Points = max(Points))
  
  k_points <- max_for_pos(df,'K')
  
  def_points <- max_for_pos(df,'DEF')
  
  keys <- c('Week','Team')
  
  #TODO Test this
  optimal_points <- qb_points %>% 
    left_join(rb_points, by=keys) %>%
    left_join(wr_points, by=keys) %>%
    left_join(te_points, by=keys) %>%
    left_join(flex_points, by=keys)
    left_join(k_points, by = keys) %>%
    left_join(def_points, by = keys) %>%
    group_by(Week,Team) %>%
  #TODO sum remaining columns
    summarize(Optimal_Points = sum())
  
  #TODO join to main data, write method for join/keys
      
} 
#TODO Fix dynamic column naming
name_column <- function(pos) {
  paste0(pos,'_Points')
}

max_for_pos <- function(df,pos) {
  col <- name_column(pos)
  df %>% 
    filter(!is.na(Player) & Pos %in% pos) %>%
    group_by(Week, Team) %>%
    summarize(Pts = max(Points)) %>%
    mutate(!!col = Pts)
}

max_for_2pos <- function(df,pos) {
  col <- name_column(pos)
  df %>%
    filter(!is.na(Player) & Pos == pos & !is.na(Points)) %>%
    group_by(Week,Team) %>%
    arrange(Week,Team, desc(Points)) %>%
    mutate(rank = rank(desc(Points))) %>%
    filter(rank < 3) %>%
    summarize(col = sum(Points))
}

#TODO: This doesn't work, but is there a way it could and is it worth it?
#Maybe using this: https://www.r-bloggers.com/dynamic-columnvariable-names-with-dplyr-using-standard-evaluation-functions/
# multi_max_for_pos <- function(df,pos) {
#   col <- name_column(pos)
#   fltr <- 'rank < 3'
#   sum <- 'sum(Points)'
#   
#   if (pos == 'Flex') {
#     fltr <- "(rank > 2 & Pos %in% c('RB','WR')) | (rank > 1 & Pos == 'TE')"
#     pos <- c('RB','WR','TE')
#     sum <- 'max(Points)'
#   }
#   
#   df %>%
#     filter(!is.na(Player) & !is.na(Points) & Pos %in% pos) %>%
#     group_by(Week,Team,Pos) %>%
#     arrange(Week,Team,Pos) %>%
#     mutate(rank = rank(desc(Points))) %>%
#     filter(fltr) %>%
#     summarize(col = sum)
# }