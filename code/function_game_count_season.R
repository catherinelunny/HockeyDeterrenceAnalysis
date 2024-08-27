#variable counting the number of games in the season - input the year that is the first half of the desired season (for example for the 2019-2020 season input 2019)
game_count_season <- function(year) {
  # uses the season_plays function to make the data frame
  game_count <- n_distinct(season_plays(year)$game_id)
  return(game_count)
}