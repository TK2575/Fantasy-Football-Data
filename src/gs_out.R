#' Export data to Google Sheets

library(googlesheets)

gs_connect <- function() {
  gs_ls()
  gs_title("2017 Fantasy Football Results")
}

gs_all_data <- function() {
  gs_full_table(get_roster_data(), 'Roster')
  gs_full_table(get_matches_data(), 'Matches')
}

gs_full_table <- function(df, mode) {
  doc <- gs_connect()
  gs_ws_delete(doc, mode, verbose=F)
  gs_ws_new(doc, mode, input = df)
}

gs_write_week <- function(df, mode) {
  doc <- gs_connect()
  gs_add_row(doc, mode, input=df, verbose=F)
}

# TODO add week-specific write method
# TODO add dimensions to gs_ws_new calls to match the input dimensions