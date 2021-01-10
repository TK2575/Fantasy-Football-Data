#' Build various summary views

library(tidyr)
library(dplyr)

team_summary <- function(matches_df) {
  matches_df %>%
    group_by(Team) %>%
    dplyr::summarize(Points_per_Week = mean(Points),
              Proj_per_Week = mean(Points-Net_vs_Proj),
              Avg_vs_Proj = mean(Net_vs_Proj),
              Avg_Optimal = mean(Optimal_Points),
              Lineup_efficiency = sum(Points)/sum(Optimal_Points),
              Wins = sum(dplyr::if_else(Win=="Win",1,0))) %>%
    add_league_average()
}

team_pos_summary <- function(roster_df,mode='sum') {
  results <- roster_df %>% 
    filter(Bench == F) %>%
    group_by(Pos, Team)
  
  if (mode == 'vs_proj') {
    results <- results %>% 
      dplyr::summarize(Avg_vs_Proj = sum(Points)-sum(Proj)) %>% 
      tidyr::spread(Pos, Avg_vs_Proj)
  } else {
    results <- results %>%
      dplyr::summarize(Avg = sum(Points)/max(Week)) %>%
      tidyr::spread(Pos, Avg)
  } 
  
  results <- results %>% 
    select(Team, 'Q/WR/T', RB, WR, TE, K, DEF) %>%
    add_league_average()
  
  dec <- sapply(results, is.numeric)
  results %>% mutate(Total = rowSums(.[dec]))
}

team_slot_summary <- function(roster_df, mode='sum') {
  sngl_slts <- roster_df %>%
    dplyr::filter(Slot %in% c('Q/WR/T','TE','W/R/T','K','DEF'), Bench == F)
  
  rb <- roster_df %>% 
    split_slots('RB')
  
  wr <- roster_df %>%
    split_slots('WR')
  
  results <- sngl_slts %>% bind_rows(rb, wr) %>% group_by(Team, Slot)
  
  if (mode =='vs_proj') {
    results <- results %>% 
      dplyr::summarize(Avg_vs_Proj = sum(Points)-sum(Proj)) %>% 
      tidyr::spread(Slot, Avg_vs_Proj)
  } else {
    results <- results %>%
      dplyr::summarize(Avg = sum(Points)/max(Week)) %>%
      tidyr::spread(Slot, Avg)
  } 
  
  results <- results %>% 
    ungroup() %>%
    select(Team, 'Q/WR/T', RB1, RB2, WR1, WR2, TE, 'W/R/T', K, DEF) %>%
    add_league_average()
  
  dec <- sapply(results, is.numeric)
  results %>% mutate(Total = rowSums(.[dec]))
}

split_slots <- function(rstr_df, mode) {
  rstr_df <- rstr_df %>% 
    dplyr::filter(Bench == F) %>% 
    dplyr::filter(Slot == mode)
  
  rstr_df <- arrange(rstr_df, Team, Week)
  odd <- rstr_df[c(seq(1,nrow(rstr_df),2)), ]
  even <- rstr_df[c(seq(2,nrow(rstr_df),2)), ]
  
  odd[['Slot']] <- paste0(mode,1)
  even[['Slot']] <- paste0(mode,2)
  
  bind_rows(odd,even) %>% arrange(Week, Team, Slot)
}

player_summary <- function(roster_df) {
  roster_df %>%
    filter(!is.na(Player)) %>%
    group_by(Pos, Player, Team, Bench) %>%
    dplyr::summarize(Avg_Points = mean(Points),
              Avg_vs_Proj = round(mean(Points - Proj),1),
              Weeks = n()) %>%
  ungroup()
}

ex_player_summary <- function(x_rstr_df) {
  x_rstr_df %>%
    filter(!is.na(Player) & !is.na(Team)) %>%
    group_by(Pos, Player, Full_Name, Team, Bench) %>%
    dplyr::summarize(Avg_Points = round(mean(Points),2),
              Avg_vs_Proj = round(mean(Points - Proj),1),
              Avg_Pos_Rank = round(mean(Rank_Pos)),
              Weeks = n()) %>%
    ungroup() %>% 
    mutate(Player_Name = if_else(is.na(Full_Name), Player, Full_Name)) %>% 
    select(-Player, -Full_Name) %>% 
    rename(Player = Player_Name) %>% 
    select(Pos, Player, Team, Bench, Avg_Points, Avg_vs_Proj, Avg_Pos_Rank, Weeks)
}

player_scoring_summary <- function(x_rstr_df) {
  pos_ordered <- c('Q/WR/T','RB','WR','TE','W/R/T','K','DEF')
  
  df <- 
    x_rstr_df %>% 
    filter(Pos != 'DEF', !is.na(Player), Points != 0)
  
  df <- 
    x_rstr_df %>% 
    filter(Pos == 'DEF', !is.na(Player), !is.na(Yds_Allow)) %>% 
    bind_rows(df)
  
  df %>%
    group_by(Pos, Player) %>%
    dplyr::summarize(Total_Points = sum(Points, na.rm = T),
              Avg_Points = round(median(Points, na.rm = T),2),
              Avg_Pos_Rank = round(median(Rank_Pos, na.rm = T)),
              Weeks_Active = n(),
              Weeks_Played = sum(!is.na(Team) & Bench == F)) %>%
    ungroup() %>% 
    filter(Weeks_Active != 0) %>% 
    arrange(desc(Total_Points))
}

add_league_average <- function(df) {
  la <- suppressWarnings(summarize_all(df,funs(mean)))
  la$Team <- 'League Average'
  df %>% bind_rows(la) %>% mutate_if(is.numeric, funs(round(., 4)))
}

adv_team_summary <- function(matches_df) {
  result <- matches_df %>%
    group_by(Week) %>%
    mutate(Avg_Points = mean(Points)) %>%
    ungroup() %>%
    mutate(Percentile = rank(Points)/n(),
           Score_Plus = Points/Avg_Points,
           Score_Plus_Percentile = rank(Score_Plus)/n()) %>%
    group_by(Team) %>%
    dplyr::summarize(Wins = sum(Win),
              SIW = sum(Percentile),
              SIW_Plus = sum(Score_Plus_Percentile),
              Points_per_Game = mean(Points),
              STDEV = sd(Points),
              Luck_Wins = Wins-SIW) %>%
    arrange(desc(Wins), desc(SIW))
  
  lg_avgs <- result %>% 
    select(SIW, Points_per_Game, STDEV) %>%
    summarize_all(.funs = c(Avg='mean', SD='sd'))
  
  #' Winning probability (for upcoming games, requires scrape of upcoming schedule)
  #' Opponent score
  #' Opponent SIW
  #' Opponent Percentile
  #' 
  #' 
  #' Points z-score = (pts per game - league avg pts per game) / league sd of sd
  #' SIW z-score = (SIW - league average SIW) / league sd of SIW
  #' Oppenent SIW = sum of opponent percentile
  #' Opponent Pts/Week = mean opponent pts
  #' 
  #' Remaining strength of schedule = 
  #' leage average pts/game x total number of teams (14) - team's pts/game - opponent pts/game x (max) weeks 
  #' /divided by/
  #' 13 - (max) weeks
  #' 
  #' SIW/game = average percentile
  #' wSIW = mean(SIW) (note, doesn't consider week rating, neither does wSIW)
  #' Total points = sum points
  #' Projected Wins = Wins + sum of remaining win probability
  #' Opponent SIW vs. actual wins = Number of weeks - wins - opponent SIW
  #' 
  #' Questions for Faust
  #' 1) variations in week weighting (skip, don't want wSIW)
  #' 2) adjustment to percentile calc
  #' 3) what of this do we actually want? (don't want wSIW)

  
  result[,-1:-2] <- round(result[,-1:-2],1)
  result
}

avg_and_sd <- function(df) {
  
}