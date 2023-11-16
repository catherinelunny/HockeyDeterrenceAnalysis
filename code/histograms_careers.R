# Alex' snippet
# Summary table
tab_games_by_players <- complete_data %>% 
  group_by(player_id) %>% 
  summarise(unique_games = length(unique(game_id)))

# Histogram with the distribtuion of games played from the summary table
p <- ggplot(tab_games_by_players, 
            aes(x = unique_games)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of unique games", y= "# of players")
print(p)

ggsave(filename = "results/hist_games_played.png",plot = p, bg = "white")


tab_seasons_by_players <- all_plays %>%
  group_by(player_id) %>%
  summarise(unique_seasons = length(unique(season)))

t <- ggplot(tab_seasons_by_players, 
            aes(x = unique_seasons)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of unique seasons", y= "# of players")
print(p)

ggsave(filename = "results/hist_seasons_played.png",plot = t, bg = "white")