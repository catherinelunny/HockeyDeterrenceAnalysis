# plots that look at the number of seasons and games played by players throughout their careers

library(tidyverse)

tab_games_by_players <- readRDS("intermediate_data/tab_games_by_players.rds")
all_player_plays <- readRDS("intermediate_data/all_player_plays.rds")
tab_seasons_by_players <- readRDS("intermediate_data/tab_seasons_by_players.rds")
tab_penalties_by_players <- readRDS("intermediate_data/tab_penalties_by_players.rds")

# Alex' snippet

# Histogram with the distribution of games played from the summary table
p <- ggplot(tab_games_by_players, 
            aes(x = unique_games)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of unique games", y= "# of players")
print(p)

ggsave(filename = "results/hist_games_played.png",plot = p, bg = "white")

# histogram of the  number of unique seasons played by the players
t <- ggplot(tab_seasons_by_players, 
            aes(x = unique_seasons)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of unique seasons", y= "# of players")
print(t)

ggsave(filename = "results/hist_seasons_played.png",plot = t, bg = "white")

# histogram of the  number of penalties had by players
r <- ggplot(tab_penalties_by_players, 
            aes(x = unique_penalties)) +
  geom_histogram(bins = 100) +
  theme_minimal() +
  labs(x="# of penalties", y= "# of players")
print(r)

ggsave(filename = "results/hist_penalties.png",plot = r, bg = "white")