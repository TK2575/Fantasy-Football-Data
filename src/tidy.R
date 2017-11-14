#' Takes data from clean.R and preps for outputs
library(dplyr)

make_roster_df <- function(df) {
  df %>% 
    filter(Slot != 'TOTAL') %>%
    filter(!(Bench == T & is.na(Player))) %>%
    select(Week, Team, Bench, Slot, Pos, Player, Points, Proj, Stats)
}

join_roster_ranks <- function(rst_df, rnk_df) {
  rst_df %>% 
    select(-Stats) %>% 
    full_join(rnk_df, by = c('Week','Player','Pos','Points')) %>%
    filter(!is.na(Team) | Points != 0 | Perc_Owned != 0) %>%
    select(Week, Team, Bench, Slot, Pos, Player, Points, Proj, Rank_Pos, Rank_Ovrl, Perc_Owned, everything())
}

add_ranks <- function(df) {
  df %>%
    select(-Rank_Ovrl) %>%
    arrange(desc(Points)) %>%
    mutate(Rank_Ovrl = rank(desc(Points),
                            na.last = 'keep',
                            ties.method = 'min')) %>%
    group_by(Pos) %>%
    mutate(Rank_Pos = rank(Rank_Ovrl,
                           na.last = 'keep',
                           ties.method = 'min')) %>%
    ungroup() %>%
    select(Week, Pos, Player, Points, Perc_Owned, Rank_Ovrl, Rank_Pos, everything())
    
}

make_match_df <- function(df) {
  results <- df %>%
    select(Week, Team, Win, Opponent) %>%
    distinct(.keep_all = T)
  
  tot_points <- df %>%
    filter(Bench == F & Slot == 'TOTAL') %>%
    group_by(Week, Team) %>%
    mutate(Net_vs_Proj = Points-Proj) %>%
    select(Week, Team, Points, Net_vs_Proj)
  
  bench_points <- df %>% 
    filter(Bench == T & Slot == 'TOTAL') %>%
    rename(Bench_Points = Points) %>% 
    select(Week, Team, Bench_Points)
  
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
  
  #TODO write method for join/keys
  keys <- c('Week','Team')
  
  optimal_points <- qb_points %>% 
    left_join(rb_points, by=keys) %>%
    left_join(wr_points, by=keys) %>%
    left_join(te_points, by=keys) %>%
    left_join(flex_points, by=keys) %>%
    left_join(k_points, by=keys) %>%
    left_join(def_points, by = keys)
  
  optimal_points[is.na(optimal_points)] <- 0
  
  optimal_points <- optimal_points %>%
    group_by_(.dots = keys) %>%
    summarize(Optimal_Points = sum(QB_Points, RB_Points, WR_Points, TE_Points, Flex_Points, K_Points, DEF_Points))
  
  results %>% 
    left_join(tot_points,by=keys) %>%
    left_join(bench_points,by=keys) %>%
    left_join(optimal_points,by=keys)
      
} 

name_column <- function(pos) {
  paste0(pos,'_Points')
}

max_for_pos <- function(df,pos) {
  col <- name_column(pos)
  df %>% 
    filter(!is.na(Player) & Pos %in% pos) %>%
    group_by(Week, Team) %>%
    summarize_(.dots = setNames('max(Points)',col))
}

max_for_2pos <- function(df,pos) {
  col <- name_column(pos)
  df %>%
    filter(!is.na(Player) & Pos == pos & !is.na(Points)) %>%
    group_by(Week,Team) %>%
    arrange(Week,Team, desc(Points)) %>%
    mutate(rank = rank(desc(Points))) %>%
    filter(rank < 3) %>%
    summarize_(.dots = setNames('sum(Points)',col))
  
  #TODO Make flex friendly
  #This doesn't work, but is there a way it could and is it worth it?
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
}