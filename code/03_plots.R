library(tidyverse)

complete_data <- readRDS("intermediate_data/complete_data.rds")
plays_players1 <- readRDS("intermediate_data/plays_players1.rds")

# SETTING UP PLOTS THAT USE DATA THAT ONLY CONTAINS PLAYERS/GAMES WITH PENALTIES


# percent of players vs percent of penalties
scatter_plot <- ggplot(data = sorted_df, aes(x = cumPlayers , y = cumPlays)) +
  geom_point() +  
  labs(x = "% of players", y = "% of total penalty time",
       title = "Concentration of total penalty time among players") +
  theme_minimal() 

ggsave("results/concentration_plot.png",scatter_plot,bg = "white")



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



# percent of players vs percent of penalties
scatter_plot <- ggplot(data = sorted_df1, aes(x = cumPlayers , y = cumPlays)) +
  geom_point() +  
  labs(x = "% of players", y = "% of total penalty time",
       title = "Concentration of total penalty time among all players") +
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
  labs(x= "Total minutes of penality during career- All Players", y ="Count of players",
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
  labs(x = "Percentile of every player's career timeline (left = earlier in career)",
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

#complete_data$game_id <- as.character(complete_data$game_id)
#complete_data$player_id <- as.character(complete_data$player_id)

# plotting with player_id and game_id of type character, takes a really long time to run
# ggplot(complete_data , aes(x = player_id, fill = game_id)) +
  # geom_bar()

games <- ggplot(data = all_plays) + 
  geom_histogram(mapping = aes(x = game_id))
ggsave("results/games.png",p,bg="white")

player_games <- all_plays %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
  )

career_games <- ggplot(data = player_games) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 10, alpha = 0.5) +
  ggtitle("Career Games Played")
print(career_games)
ggsave("results/careergames.png", career_games)




#plot of number of penalties a player had in a given year
filtered_year2019 <- filter(year2019, playerType == "PenaltyOn")
filtered_year2019 <- distinct(filtered_year2019)
plays2019 <- ggplot(filtered_year2019,
                    aes(x = player_id)) +
  geom_bar(stat = "count")
print(plays2019)
ggsave("results/2019PenaltyCount.png", plays2019)



# Bar graph showing the distribution of all of the games played by players who had 2 penalties in 2000
fixedwindow2019 <- ggplot(fixedwindow_2019, aes(x = player_id, y = game_count) ) +
  geom_bar(stat = "identity")
print(fixedwindow2019)
ggsave("results/2019twopenalties.png", fixedwindow2019)

player_games2020 <- year2020 %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
  )

# Print the result
print(player_games)

#plots of total number of games played in a year for each player
game_count_bar <- ggplot(data = player_games) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5)
print(game_count_bar)

game_count_bar2000 <- ggplot(data = player_games2000) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2000 Games Played")
print(game_count_bar2000)

game_count_bar2001 <- ggplot(data = player_games2001) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2001 Games Played")
print(game_count_bar2001)

game_count_bar2002 <- ggplot(data = player_games2002) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2002 Games Played")
print(game_count_bar2002)

game_count_bar2003 <- ggplot(data = player_games2003) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2003 Games Played")
print(game_count_bar2003)

game_count_bar2004 <- ggplot(data = player_games2004) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2004 Games Played")
print(game_count_bar2004)

game_count_bar2005 <- ggplot(data = player_games2005) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2005 Games Played")
print(game_count_bar2005)

game_count_bar2006 <- ggplot(data = player_games2006) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2006 Games Played")
print(game_count_bar2006)

game_count_bar2007 <- ggplot(data = player_games2007) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2007 Games Played")
print(game_count_bar2007)

game_count_bar2008 <- ggplot(data = player_games2008) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2008 Games Played")
print(game_count_bar2008)

game_count_bar2009 <- ggplot(data = player_games2009) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2009 Games Played")
print(game_count_bar2009)

game_count_bar2010 <- ggplot(data = player_games2010) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2010 Games Played")
print(game_count_bar2010)

game_count_bar2011 <- ggplot(data = player_games2011) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2011 Games Played")
print(game_count_bar2011)

game_count_bar2012 <- ggplot(data = player_games2012) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2012 Games Played")
print(game_count_bar2012)

game_count_bar2013 <- ggplot(data = player_games2013) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2013 Games Played")
print(game_count_bar2013)

game_count_bar2014 <- ggplot(data = player_games2014) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2014 Games Played")
print(game_count_bar2014)

game_count_bar2015 <- ggplot(data = player_games2015) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2015 Games Played")
print(game_count_bar2015)

game_count_bar2016 <- ggplot(data = player_games2016) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2016 Games Played")
print(game_count_bar2016)

game_count_bar2017 <- ggplot(data = player_games2017) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2017 Games Played")
print(game_count_bar2017)

game_count_bar2018 <- ggplot(data = player_games2018) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
  ggtitle("2018 Games Played")
print(game_count_bar2018)

game_count_bar2019 <- ggplot(data = player_games2019) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 10, alpha = 0.5) +
  ggtitle("2019 Games Played")
print(game_count_bar2019)
ggsave("results/2019gamesplayed.png", game_count_bar2019)



# percent of players vs percent of penalties
scatter_plot <- ggplot(data = sorted_df, aes(x = cumPlayers , y = cumPlays)) +
  geom_point() +  
  labs(x = "Players", y = "Penalties", title = "Scatter Plot") +
  theme_minimal() 

ggsave(filename = "results/players-penalties-scatter.png",
       plot = scatter_plot,bg = "white")



# takes a really long time to run
# ggplot(complete_data , aes(x = game_id, group = player_id, color = player_id)) +
# geom_histogram(binwidth = 500)

# complete_data$game_id <- as.character(complete_data$game_id)
# complete_data$player_id <- as.character(complete_data$player_id)

# plotting with player_id and game_id of type character, takes a really long time to run
# ggplot(complete_data , aes(x = player_id, fill = game_id)) +
# geom_bar()


  