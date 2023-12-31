#making histograms for each player's penalties
for (number in unique_elements) {
  filtered_data <- complete_data %>%
    filter(player_id == number)
  graph <- ggplot(filtered_data, aes(x = game_id)) + 
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = number, x = "Game", y = "Frequency")
  print(graph)
  file_name <- "/Users/catherinelunny/Downloads/archive/histogram.png"
  ggsave(file_name = file_path, plot = graph)
}

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

#plot for percent of penalties vs percent of players that commit them
scatter_plot <- ggplot(data = sorted_df, aes(x = cumPlayers , y = cumPlays)) +
  geom_point() +  
  labs(x = "Players", y = "Penalties", title = "Scatter Plot") +
  theme_minimal() 

print(scatter_plot)
png("/Users/catherinelunny/Downloads/scatterplot.png")


#creating data frames for different penalty types- 
minorPenalty <- complete_data %>%
  filter(penaltySeverity == 'Minor')

majorPenalty <- complete_data %>%
  filter(penaltySeverity == 'Major')

benchMinor <- complete_data %>%
  filter(penaltySeverity == 'Bench Minor')

gameMisconduct <- complete_data %>%
  filter(penaltySeverity == "Game Misconduct")

Misconduct <- complete_data %>%
  filter(penaltySeverity == 'Misconduct')

penaltyShot <- complete_data %>%
  filter(penaltySeverity == 'Penalty Shot')

match <- complete_data %>%
  filter(penaltySeverity == 'Match')