# Alex' snippet
tab_games_by_players <- readRDS("intermediate_data/tab_games_by_players.rds")
all_player_plays <- readRDS("intermediate_data/all_player_plays.rds")
tab_seasons_by_players <- readRDS("intermediate_data/tab_seasons_by_players.rds")

# Histogram with the distribtuion of games played from the summary table
p <- ggplot(tab_games_by_players, 
            aes(x = unique_games)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of unique games", y= "# of players")
print(p)

ggsave(filename = "results/hist_games_played.png",plot = p, bg = "white")

t <- ggplot(tab_seasons_by_players, 
            aes(x = unique_seasons)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of unique seasons", y= "# of players")
print(p)

ggsave(filename = "results/hist_seasons_played.png",plot = t, bg = "white")