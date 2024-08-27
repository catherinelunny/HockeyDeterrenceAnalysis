# Data frame that shows the number of games and penalties for players during a specific season. can specify penalty severity minor or major or with all together
# input the year that is the first half of the desired season (for the 2019-2020 season input 2019)
player_games_season <- function(year, penalty_severity = NULL) {
  # for all penalties together
  if (is.null(penalty_severity)){
    player_games <- season_plays(year) %>%
      # counting by player
      group_by(player_id) %>%
      summarize(game_count = n_distinct(unique(game_id)),
                penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"])))
  } else if (penalty_severity == "Major") {
    player_games <- season_plays(year) %>%
      group_by(player_id) %>%
      summarize(game_count = n_distinct(unique(game_id)),
                penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn" & penaltySeverity == penalty_severity])))
  } else if (penalty_severity == "Minor") {
    player_games <- season_plays(year) %>%
      group_by(player_id) %>%
      summarize(game_count = n_distinct(unique(game_id)),
                penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn" & penaltySeverity == penalty_severity])))
  }
  
  return(player_games)
}