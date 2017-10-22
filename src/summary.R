#' Build various summary views

library(tidyr)
library(dplyr)

# TODO add W-L record
team_summary <- function(matches_df) {
  matches_df %>%
    group_by(Team) %>%
    summarize(Points_per_Week = mean(Points),
              Proj_per_Week = mean(Points+Net_vs_Proj),
              Avg_vs_Proj = mean(Net_vs_Proj),
              Avg_Optimal = mean(Optimal_Points),
              Lineup_efficiency = sum(Points)/sum(Optimal_Points)) %>%
    add_league_average()
}

# TODO add Total column
team_pos_summary <- function(roster_df) {
  roster_df %>% 
    filter(Bench == F) %>%
    group_by(Pos, Team) %>%
    summarize(Avg_vs_Proj = sum(Points)-sum(Proj)) %>% 
    spread(Pos, Avg_vs_Proj) %>%
    select(Team, QB, RB, WR, TE, K, DEF) %>%
    add_league_average()
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
