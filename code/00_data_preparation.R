library(tidyverse)

# Open data
plays_players <- read.csv("data/plays_players.csv")
penalties <- read.csv("data/game_penalties.csv")
players <- read.csv("data/player_info.csv")

filtered_penalties <- penalties %>%
  filter(play_id %in% plays_players$'play_id')

complete_data <- cbind(plays_players, filtered_penalties)

unique_elements <- unique(complete_data$player_id)
print(unique_elements)

complete_data <- complete_data[, c("play_id", "game_id", "player_id", "playerType", "penaltySeverity", "penaltyMinutes")]

saveRDS(complete_data,"intermediate_data/complete_data.rds")
