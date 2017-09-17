library(httr)
library(RJSONIO)

key <- 'dj0yJmk9VVhMa3NJSGtpT2VlJmQ9WVdrOVpqWnpha1pSTXpBbWNHbzlNQS0tJnM9Y29uc3VtZXJzZWNyZXQmeD05Yg--'
secret <- 'e1a5efad051506779f5ed64d869411ff0c19bccf'

GetSig<- function(key,secret){
  options("httr_oob_default" = TRUE)
  tokenURL      <- "https://api.login.yahoo.com/oauth/v2/get_request_token" # Get request token
  accessTokenURL<- "https://api.login.yahoo.com/oauth/v2/get_token" # Request access token
  authorizeURL  <- "https://api.login.yahoo.com/oauth/v2/request_auth" #Get user auth
  
  App   <- httr::oauth_app("yahoo",key,secret)
  yahoo <- httr::oauth_endpoint(request=tokenURL,authorizeURL,accessTokenURL)
  token <- httr::oauth1.0_token(yahoo,App)
  sig   <- httr::config(token = token)
  sig
}

GetGameID<- function(sig){
  
  target<-"http://fantasysports.yahooapis.com/fantasy/v2/game/nfl?format=json"
  x<- httr::GET(target,sig)
  y<- RJSONIO::fromJSON(as.character(x), asText=T)
  y$fantasy_content$game[[1]]$game_id
}

GetTeamData<-function(GameID,LeagueID='25989',team=12,DataType="stats",sig){
  base       <- "http://fantasysports.yahooapis.com/fantasy/v2/team/"
  formatting <- switch(DataType,
                       stats     = "/stats?format=json",
                       standings = "/standings?format=json",
                       matchups  = "/matchups?format=json")
  
  x <- paste(GameID,".l.",LeagueID,".t.",team,sep="")
  y <- paste(base,x,formatting,sep="")
  z <- httr::GET(y,sig) 
  
}