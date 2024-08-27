#function that returns a data frame with players with a specific number of penalties in a season by filtering by the number of penalties
fixednumber_penalties_season <- function(year, penalties, penalty_severity = NULL) {
  if (is.null(penalty_severity)){
    playerswith_number_penalties <- filter(player_games_season(year, penalty_severity), penalty_count == penalties)
    return(playerswith_number_penalties)
  } else if (penalty_severity == "Major"){
    playerswith_number_penalties <- filter(player_games_season(year, penalty_severity), penalty_count == penalties)
    return(playerswith_number_penalties)
  } else if (penalty_severity == "Minor"){
    playerswith_number_penalties <- filter(player_games_season(year, penalty_severity), penalty_count == penalties)
    return(playerswith_number_penalties)
  } 
  
}