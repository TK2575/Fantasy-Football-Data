#' Prep weekly data to excel spreadsheet

library(tidyverse)

write_week_to_csv <- function(week_num = 0) {
  mtchs <- get_matches_data() 
  if (week_num > 0) {
    mtchs <- mtchs %>% filter(Week == week_num)
  }
  
  rstr <- get_roster_data()
  
  tm_scrng_smry <- mtchs %>% team_summary()
  pstn_scrng_smry <- rstr %>% team_slot_summary()
  pstn_scrng_vs_prjctn <- rstr %>% team_slot_summary(mode='vs_proj')
  
  if (week_num > 0) {
    rstr <- rstr %>% filter(Week == week_num)
  }
  
  rstr_xpnd <- get_expanded_roster_data() 
  
  plyr_scrng_smry <- get_expanded_roster_data() %>% ex_player_summary()
  plyr_scrng_smry2 <- get_expanded_roster_data() %>% player_scoring_summary()
  
  if (week_num > 0) {
    rstr_xpnd <- rstr_xpnd %>% filter(Week == week_num)
  }
  
  setwd("out")
  
  write_csv(mtchs, "1_matches.csv")
  write_csv(tm_scrng_smry, "2_team_scoring_summary.csv")
  write_csv(pstn_scrng_smry, "3_position_scoring_summary.csv")
  write_csv(pstn_scrng_vs_prjctn, "4_position_scoring_summary_vs_projection.csv")
  write_csv(rstr, "5_rosters.csv")
  write_csv(plyr_scrng_smry, "6_player_scoring_summary.csv")
  write_csv(plyr_scrng_smry2, "7_player_scoring_summary_beta.csv")
  write_csv(rstr_xpnd, "8_rosters_expanded.csv")
  
  setwd("../")
}
