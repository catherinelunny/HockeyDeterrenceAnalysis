library(tidyverse)

all_player_plays <- readRDS("intermediate_data/all_player_plays.rds")
normalized_player_data <- readRDS("intermediate_data/normalized_player_data.rds")
averages <- readRDS("intermediate_data/averages.rds")

# histogram of the average amount of plays per season for players
q <- ggplot(averages, 
            aes(x = avg_plays_season)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# plays in a season on average", y= "# of players")

ggsave(filename = "results/hist_avg_games.png",plot = q, bg = "white")

# 
# library(ggplot2)
# 
# t <- ggplot(normalized_player_data, aes(x = normalized_plays, fill = player_id)) +
#   geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.7) +
#   labs(title = "Distribution of Normalized Plays Across Players",
#        x = "Normalized Plays",
#        y = "Frequency",
#        fill = "Player ID") +
#   theme_minimal()
# ggsave(filename = "results/norm_total_minutes.png",plot = t, bg = "white")

# normalized_playergames <- player_games
# normalized_playergames$normalized_games <- (player_games$game_count-min(player_games$game_count))/(max(player_games$game_count)-min(player_games$game_count))
# normalized_playergames <- subset(normalized_playergames, select = c(player_id, normalized_games))
# 
# q <- ggplot(normalized_playergames, 
#             aes(x = normalized_games)) +
#   geom_histogram(bins = 100) +
#   theme_minimal() +
#   labs(x="# of unique games", y= "# of players")
# 
# print(q)
