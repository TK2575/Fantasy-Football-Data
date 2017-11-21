#' Export data to Google Sheets

library(googlesheets)

gs_connect <- function() {
  gs_ls()
  gs_title("2017 Fantasy Football Results")
}

gs_add_week <- function(week_num) {
  get_matches_data() %>% filter(Week == week_num) %>% gs_add_data(mode='Matches')
  get_roster_data() %>% filter(Week == week_num) %>% gs_add_data(mode='Roster')
  gs_update_summaries()
}

gs_update_summaries <- function() {
  get_matches_data() %>% team_summary() %>% gs_full_table(mode='Team Scoring Summary')
  get_roster_data() %>% team_pos_summary() %>% gs_full_table(mode='Position Scoring Summary')
  get_roster_data() %>% team_pos_summary(mode='vs_proj') %>% gs_full_table(mode='Position Scoring vs Projection')
  get_expanded_roster_data() %>% ex_player_summary() %>% gs_full_table(mode='Player Scoring Summary')
}

gs_add_all_data <- function() {
  gs_full_table(get_roster_data(), 'Roster')
  gs_full_table(get_matches_data(), 'Matches')
  gs_update_summaries()
}

# TODO delete contents of sheet instead of actual sheet? i.e. gs_edit_cells()
gs_full_table <- function(df, mode) {
  doc <- gs_connect()
  gs_ws_delete(doc, mode, verbose=F)
  doc <- gs_connect()
  gs_ws_new(doc, mode, input = df)
}

gs_add_data <- function(df, mode) {
  doc <- gs_connect()
  gs_add_row(doc, mode, input=df, verbose=F)
}


# TODO add dimensions to gs_ws_new calls to match the input dimensions