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

# percent of players vs percent of penalties
scatter_plot <- ggplot(data = sorted_df, aes(x = cumPlayers , y = cumPlays)) +
  geom_point() +  
  labs(x = "Players", y = "Penalties", title = "Scatter Plot") +
  theme_minimal() 

print(scatter_plot)

# histogram where player_id and game_id are of type integer
ggplot(complete_data, aes(x = player_id, group = game_id, color = game_id)) +
  geom_histogram(binwidth = 500, alpha = 1)

# takes a really long time to run
# ggplot(complete_data , aes(x = game_id, group = player_id, color = player_id)) +
  # geom_histogram(binwidth = 500)

# complete_data$game_id <- as.character(complete_data$game_id)
# complete_data$player_id <- as.character(complete_data$player_id)

# plotting with player_id and game_id of type character, takes a really long time to run
# ggplot(complete_data , aes(x = player_id, fill = game_id)) +
  # geom_bar()

