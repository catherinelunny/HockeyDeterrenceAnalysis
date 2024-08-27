# makes a data frame of the plays in a specific season - input the year that is the first half of the desired season (for example for the 2019-2020 season input 2019)
season_plays <- function(year) {
  specific_season <- paste0(year, year+1)
  seasondf <- filter(all_player_plays, season == specific_season)
  
  return(seasondf)
}