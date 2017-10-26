#' Build various summary views

library(tidyr)
library(dplyr)

# TODO add W-L record
team_summary <- function(matches_df) {
  matches_df %>%
    group_by(Team) %>%
    summarize(Points_per_Week = mean(Points),
              Proj_per_Week = mean(Points-Net_vs_Proj),
              Avg_vs_Proj = mean(Net_vs_Proj),
              Avg_Optimal = mean(Optimal_Points),
              Lineup_efficiency = sum(Points)/sum(Optimal_Points),
              Wins = sum(Win)) %>%
    add_league_average()
}

team_pos_summary <- function(roster_df,mode='sum') {
  results <- roster_df %>% 
    filter(Bench == F) %>%
    group_by(Pos, Team)
  
  if (mode == 'vs_proj') {
    results <- results %>% 
      summarize(Avg_vs_Proj = sum(Points)-sum(Proj)) %>% 
      spread(Pos, Avg_vs_Proj)
  } else {
    results <- results %>%
      summarize(Avg = sum(Points)/max(Week)) %>%
      spread(Pos, Avg)
  } 
  
  results <- results %>% 
    select(Team, QB, RB, WR, TE, K, DEF) %>%
    add_league_average()
  
  dec <- sapply(results, is.numeric)
  results %>% mutate(Total = rowSums(.[dec]))
}

player_summary <- function(roster_df) {
  roster_df %>%
    group_by(Pos, Player, Team, Bench) %>%
    summarize(Total_vs_Proj = sum(Points) - sum(Proj),
              Weeks = n()) 
}

add_league_average <- function(df) {
  la <- suppressWarnings(summarize_all(df,funs(mean)))
  la$Team <- 'League Average'
  df %>% bind_rows(la) %>% mutate_if(is.numeric, funs(round(., 4)))
}
