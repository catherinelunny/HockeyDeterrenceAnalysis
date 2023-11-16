library(tidyverse)

# Open data
plays_players <- read.csv("data/plays_players.csv")
penalties <- read.csv("data/game_penalties.csv")
players <- read.csv("data/player_info.csv")
plays_players1 <- readRDS("intermediate_data/plays_players1.rds")
games <- read.csv("data/games.csv")
#write.csv(games, file = "data/games.csv", row.names = FALSE)

games$date_time_GMT <- substr(games$date_time_GMT, 1, 10)
games$date_time_GMT <- as.Date(games$date_time_GMT)
games_ordered <- games %>% arrange(date_time_GMT)

filtered_penalties <- penalties %>%
  filter(play_id %in% plays_players$'play_id')

#complete_data only contains penalties plays/players
complete_data <- cbind(plays_players, filtered_penalties)
all_plays <- merge(plays_players1, penalties, by = "play_id", all = TRUE)
filtered_all_plays <- all_plays %>%
  filter(playerType != "DrewBy")
filtered_all_plays$penaltyMinutes <- coalesce(filtered_all_plays$penaltyMinutes, 0)

unique_elements <- unique(complete_data$player_id)
print(unique_elements)

complete_data <- complete_data[, c("play_id", "game_id", "player_id", "playerType", "penaltySeverity", "penaltyMinutes")]

saveRDS(complete_data,"intermediate_data/complete_data.rds")

complete_data <- complete_data %>%
  mutate(accumulated_column = cumsum(penaltyMinutes))

complete_data$percentPenalties <- complete_data$'penaltyMinutes' / sum(complete_data$'penaltyMinutes')

complete_data <- complete_data %>%
  group_by(penaltySeverity) %>%
  mutate(category_counts = n())

complete_data <- complete_data %>%
  group_by(player_id) %>%
  mutate(totalPenaltyMinutes = sum(penaltyMinutes))

unique_players <- complete_data %>%
  filter(!duplicated(player_id))

complete_data <- complete_data %>%
  mutate(accumulated_column = cumsum(penaltyMinutes))

complete_data$percentPenalties <- complete_data$'penaltyMinutes' / sum(complete_data$'penaltyMinutes')

complete_data <- complete_data %>%
  group_by(penaltySeverity) %>%
  mutate(category_counts = n())

complete_data <- complete_data %>%
  group_by(player_id) %>%
  mutate(totalPenaltyMinutes = sum(penaltyMinutes))

unique_players <- complete_data %>%
  filter(!duplicated(player_id))

#sorting players total penalty minutes in descending order
sorted_df <- unique_players[order(-unique_players$totalPenaltyMinutes), ]

# sorted_df$increasingMinutes <- sorted_df$totalPenaltyMinutes[order(sorted_df$totalPenaltyMinutes)]


# creating a column that totals up all of the penalty minutes
sorted_df$cumMinutes <- cumsum(sorted_df$totalPenaltyMinutes)

#creating a column ranking players from most penalty minutes to least
sorted_df$ranked_players <- rank(-sorted_df$totalPenaltyMinutes)

sorted_df$playPercentages <- (sorted_df$totalPenaltyMinutes / sum(sorted_df$totalPenaltyMinutes)) * 100

sorted_df$playerPercentages <- (sorted_df$ranked_players / sum(sorted_df$ranked_players)) * 100

sorted_df$cumPlays <- cumsum(sorted_df$playPercentages)

sorted_df$cumPlayers <- cumsum(sorted_df$playerPercentages)

#sorting players total penalty minutes in descending order
sorted_df <- unique_players[order(-unique_players$totalPenaltyMinutes), ]

# sorted_df$increasingMinutes <- sorted_df$totalPenaltyMinutes[order(sorted_df$totalPenaltyMinutes)]


# creating a column that totals up all of the penalty minutes
sorted_df$cumMinutes <- cumsum(sorted_df$totalPenaltyMinutes)

#creating a column ranking players from most penalty minutes to least
sorted_df$ranked_players <- rank(-sorted_df$totalPenaltyMinutes)

sorted_df$playPercentages <- (sorted_df$totalPenaltyMinutes / sum(sorted_df$totalPenaltyMinutes)) * 100

sorted_df$playerPercentages <- (sorted_df$ranked_players / sum(sorted_df$ranked_players)) * 100

sorted_df$cumPlays <- cumsum(sorted_df$playPercentages)

sorted_df$cumPlayers <- cumsum(sorted_df$playerPercentages)


#working with the data containing ALL players and plays
filtered_all_plays <- filtered_all_plays %>%
  mutate(accumulated_column = cumsum(penaltyMinutes))

filtered_all_plays$percentPenalties <- filtered_all_plays$'penaltyMinutes' / sum(filtered_all_plays$'penaltyMinutes')

filtered_all_plays <- filtered_all_plays %>%
  group_by(penaltySeverity) %>%
  mutate(category_counts = n())

filtered_all_plays <- filtered_all_plays %>%
  group_by(player_id) %>%
  mutate(totalPenaltyMinutes = sum(penaltyMinutes))

unique_players1 <- filtered_all_plays %>%
  filter(!duplicated(player_id))

#sorting players total penalty minutes in descending order
sorted_df1 <- unique_players1[order(-unique_players1$totalPenaltyMinutes), ]

# sorted_df$increasingMinutes <- sorted_df$totalPenaltyMinutes[order(sorted_df$totalPenaltyMinutes)]


# creating a column that totals up all of the penalty minutes
sorted_df1$cumMinutes <- cumsum(sorted_df1$totalPenaltyMinutes)

#creating a column ranking players from most penalty minutes to least
sorted_df1$ranked_players <- rank(-sorted_df1$totalPenaltyMinutes)

sorted_df1$playPercentages <- (sorted_df1$totalPenaltyMinutes / sum(sorted_df1$totalPenaltyMinutes)) * 100

sorted_df1$playerPercentages <- (sorted_df1$ranked_players / sum(sorted_df1$ranked_players)) * 100

sorted_df1$cumPlays <- cumsum(sorted_df1$playPercentages)

sorted_df1$cumPlayers <- cumsum(sorted_df1$playerPercentages)

all_plays$game_id <- as.character(all_plays$game_id)
all_plays$player_id <- as.character(all_plays$player_id)

all_plays <- merge(all_plays, games_ordered, by = "game_id", all.x = TRUE)

#making a data frame for each year
# season20002001 <- filter(all_plays, season == "20002001")
# season20012002 <- filter(all_plays, season == "20012002")
# season20022003 <- filter(all_plays, season == "20022003")
# season20032004 <- filter(all_plays, season == "20032004")
# season20042005 <- filter(all_plays, season == "20042005")
# season20052006 <- filter(all_plays, season == "20052006")
# season20062007 <- filter(all_plays, season == "20062007")
# season20072008 <- filter(all_plays, season == "20072008")
# season20082009 <- filter(all_plays, season == "20082009")
# season20092010 <- filter(all_plays, season == "20092010")
# season20102011 <- filter(all_plays, season == "20102011")
# season20112012 <- filter(all_plays, season == "20112012")
# season20122013 <- filter(all_plays, season == "20122013")
# season20132014 <- filter(all_plays, season == "20132014")
# season20142015 <- filter(all_plays, season == "20142015")
# season20152016 <- filter(all_plays, season == "20152016")
# season20162017 <- filter(all_plays, season == "20162017")
# season20172018 <- filter(all_plays, season == "20172018")
# season20182019 <- filter(all_plays, season == "20182019")
season20192020 <- filter(all_plays, season == "20192020")
# season20202021 <- filter(all_plays, season == "20202021")
# season20212022 <- filter(all_plays, season == "20212022")

player_games <- all_plays %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
  )

 player_games20002001 <- season20002001 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 fixedwindow_2000 <- filter(player_games2000, penalty_count == 2)


# Bar graph showing the distribution of all of the games played by players who had 2 penalties in 2000
fixedwindow2000 <- ggplot(fixedwindow_2000, aes(x = player_id, y = game_count) ) +
  geom_bar(stat = "identity")
print(fixedwindow2000)

 player_games20012002 <- season20012002 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20022003 <- season20022003 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20032004 <- season20032004 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20042005 <- season20042005 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20052006 <- season20052006 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20062007 <- season20062007 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20072008 <- season20072008 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
                          penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20082009 <- season20082009 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20092010 <- season20092010 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20102011 <- season20102011 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20112012 <- season20112012 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20122013 <- season20122013 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20132014 <- season20132014 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20142015 <- season20142015 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20152016 <- season20152016 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 player_games20162017 <- season20162017 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )
 
 player_games20172018 <- season20172018 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )


 player_games20182019 <- season20182019 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

 # Trial Analysis for the year 2019
 player_games20192020 <- season20192020 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )

# attempt at using the method outlined in the paper, a probability distribution of the amount of games in between each penalty for players
fixedwindow_20192020 <- filter(player_games20192020, penalty_count == 2)

twopenalties20192020 <- filter(season20192020, player_id %in% fixedwindow_20192020$player_id)
twopenalties20192020 <- filter(twopenalties20192020, playerType == "PenaltyOn")
twopenalties20192020 <- distinct(twopenalties20192020)
twopenalties20192020$game_order <- dense_rank(twopenalties20192020$date_time_GMT)


result20192020 <- twopenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

result20192020$percent <- result20192020$game_difference / n_distinct(unique(season20192020$game_id))



