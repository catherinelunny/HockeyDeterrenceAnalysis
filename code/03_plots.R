library(tidyverse)

complete_data <- readRDS("intermediate_data/complete_data.rds")
plays_players1 <- readRDS("intermediate_data/plays_players1.rds")

# SETTING UP PLOTS THAT USE DATA THAT ONLY CONTAINS PLAYERS/GAMES WITH PENALTIES

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
  labs(x = "% of players", y = "% of total penalty time",
       title = "Concentration of total penalty time among players") +
  theme_minimal() 

ggsave("results/concentration_plot.png",scatter_plot,bg = "white")

print(scatter_plot)


n_of_bins <- 10
cut_breaks <- seq(0,1,length.out = n_of_bins+1)
cut_breaks_labels <- sprintf(cut_breaks, fmt = "%.2f")
cut_breaks_labels <- paste(cut_breaks_labels[-length(cut_breaks_labels)],
                           cut_breaks_labels[-1],
                           sep = "-")

# Calculate the grouping of penalties within each player career:
careers <- complete_data %>% 
  arrange(player_id, game_id) %>% 
  group_by(player_id) %>% 
  mutate(game_id_scaled = (game_id-min(game_id))/(max(game_id)-min(game_id)),
         game_percentile = cut(game_id_scaled,
                               breaks = cut_breaks,
                               labels = cut_breaks_labels,
                               include.lowest = T,ordered_result = T),
         total_penalty_minutes = sum(penaltyMinutes),
         total_games_with_penalties = n()) %>% 
  ungroup() %>% 
  select(player_id,game_percentile,penaltyMinutes,total_penalty_minutes,total_games_with_penalties) %>% 
  arrange(player_id, game_percentile)

p <- ggplot(careers, aes(x = total_penalty_minutes)) +
  geom_histogram(color="black", fill="white") +
  theme_minimal() +
  labs(x= "Total minutes of penality during career", y ="Count of players",
       caption = "Median is 424 minutes (~7 hours).")

ggsave("results/total_penalty_minutes_during_career.png",p,bg="white")
print(p)

career_data <- careers %>% 
  group_by(player_id,game_percentile) %>% 
  reframe(penalty_minutes_share = sum(penaltyMinutes) / dplyr::first(total_penalty_minutes)) %>% 
  ungroup() %>% 
  complete(player_id,game_percentile,
           fill = list(penalty_minutes_share = 0),explicit = F) %>% 
  na.omit() %>% 
  ungroup()

mean_share_table <- career_data %>% 
  group_by(game_percentile) %>% 
  summarise(mean_share_of_total_penalty_minutes = round(mean(penalty_minutes_share),3))

#knitr::kable(mean_share_table,format = "simple")

p <- ggplot(career_data,
            aes(x = game_percentile,
                y = penalty_minutes_share)) +
  geom_boxplot() +
  labs(x = "Percentile of player's career timeline (left = earlier in career)",
       y = "Share of penalty minutes over whole career",
       caption = "Each dot shows the contribution of each player's penalties\nduring the career stage to the total of penalty minutes over the career.\nEach player's career is split into 10 percentiles based on play_id.") +
  theme_minimal()

ggsave("results/boxplot_penalty_time_over_career.png",p,bg="white")
print(p)  

# histogram where player_id and game_id are of type integer
p <- ggplot(complete_data,
            aes(x = player_id, group = game_id, color = game_id)) +
  geom_histogram(binwidth = 500, alpha = 1)
print(p)

# takes a really long time to run
# ggplot(complete_data , aes(x = game_id, group = player_id, color = player_id)) +
# geom_histogram(binwidth = 500)

# complete_data$game_id <- as.character(complete_data$game_id)
# complete_data$player_id <- as.character(complete_data$player_id)

# plotting with player_id and game_id of type character, takes a really long time to run
# ggplot(complete_data , aes(x = player_id, fill = game_id)) +
# geom_bar()

# SAME PLOTS BUT USING DATA THAT CONTAINS GAMES/PLAYERS THAT DO NOT HAVE ANY PENALTIES

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


# percent of players vs percent of penalties
scatter_plot <- ggplot(data = sorted_df1, aes(x = cumPlayers , y = cumPlays)) +
  geom_point() +  
  labs(x = "% of players", y = "% of total penalty time",
       title = "Concentration of total penalty time among players") +
  theme_minimal() 

ggsave("results/concentration_plot_allplays.png",scatter_plot,bg = "white")

print(scatter_plot)


n_of_bins <- 10
cut_breaks <- seq(0,1,length.out = n_of_bins+1)
cut_breaks_labels <- sprintf(cut_breaks, fmt = "%.2f")
cut_breaks_labels <- paste(cut_breaks_labels[-length(cut_breaks_labels)],
                           cut_breaks_labels[-1],
                           sep = "-")

# Calculate the grouping of penalties within each player career:
careers1 <- filtered_all_plays %>% 
  arrange(player_id, game_id) %>% 
  group_by(player_id) %>% 
  mutate(game_id_scaled = (game_id-min(game_id))/(max(game_id)-min(game_id)),
         game_percentile = cut(game_id_scaled,
                               breaks = cut_breaks,
                               labels = cut_breaks_labels,
                               include.lowest = T,ordered_result = T),
         total_penalty_minutes = sum(penaltyMinutes),
         total_games_with_penalties = sum(penaltyMinutes != 0),
         total_games = n()) %>%
  ungroup() %>% 
  select(player_id,game_percentile,penaltyMinutes,total_penalty_minutes,total_games_with_penalties, total_games) %>% 
  arrange(player_id, game_percentile)

median(careers1$total_penalty_minutes)

p <- ggplot(careers1, aes(x = total_penalty_minutes)) +
  geom_histogram(color="black", fill="white") +
  theme_minimal() +
  labs(x= "Total minutes of penality during career", y ="Count of players",
       caption = "Median is 369 minutes (~6 hours).")

ggsave("results/total_penalty_minutes_during_career_allplayers.png",p,bg="white")
print(p)

career_data1 <- careers1 %>% 
  group_by(player_id,game_percentile) %>% 
  reframe(penalty_minutes_share = sum(penaltyMinutes) / dplyr::first(total_penalty_minutes)) %>% 
  ungroup() %>% 
  complete(player_id,game_percentile,
           fill = list(penalty_minutes_share = 0),explicit = F) %>% 
  na.omit() %>% 
  ungroup()

mean_share_table1 <- career_data1 %>% 
  group_by(game_percentile) %>% 
  summarise(mean_share_of_total_penalty_minutes = round(mean(penalty_minutes_share),3))

#knitr::kable(mean_share_table,format = "simple")

p <- ggplot(career_data1,
            aes(x = game_percentile,
                y = penalty_minutes_share)) +
  geom_boxplot() +
  labs(x = "Percentile of player's career timeline (left = earlier in career)",
       y = "Share of penalty minutes over whole career",
       caption = "Each dot shows the contribution of each player's penalties\nduring the career stage to the total of penalty minutes over the career.\nEach player's career is split into 10 percentiles based on play_id.") +
  theme_minimal()

ggsave("results/boxplot_penalty_time_over_career_allplayers.png",p,bg="white")
print(p)  

# histogram where player_id and game_id are of type integer
p <- ggplot(filtered_all_plays,
       aes(x = player_id, group = game_id, color = game_id)) +
  geom_histogram(binwidth = 500, alpha = 1)
print(p)

# takes a really long time to run
# ggplot(complete_data , aes(x = game_id, group = player_id, color = player_id)) +
  # geom_histogram(binwidth = 500)

# complete_data$game_id <- as.character(complete_data$game_id)
# complete_data$player_id <- as.character(complete_data$player_id)

# plotting with player_id and game_id of type character, takes a really long time to run
# ggplot(complete_data , aes(x = player_id, fill = game_id)) +
  # geom_bar()

