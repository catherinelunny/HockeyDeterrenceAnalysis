all_player_plays <- readRDS("intermediate_data/all_player_plays.rds")
normalized_player_data <- readRDS("intermediate_data/normalized_player_data.rds")
averages <- readRDS("intermediate_data/averages.rds")


normalized_player_data <- all_player_plays %>%
  group_by(player_id, season) %>%
  summarise(total_plays = length(unique(play_id))) %>%
  group_by(player_id) %>%
  mutate(normalized_plays = total_plays / sum(total_plays))
saveRDS(normalized_player_data,"intermediate_data/normalized_player_data.rds")

averages <- normalized_player_data %>%
  group_by(player_id) %>%
  summarise(seasons_played = length(unique(season)),
            total_plays = sum(total_plays)) %>%
  mutate(avg_plays_season = total_plays / seasons_played)
saveRDS(averages,"intermediate_data/averages.rds")

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
