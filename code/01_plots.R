library(tidyverse)
unique_player_play_info <- readRDS("intermediate_data/unique_player_play_info.rds")
careers <- readRDS("intermediate_data/careers.rds")
career_data <- readRDS("intermediate_data/career_data.rds") 
season20192020 <- readRDS("intermediate_data/season20192020.rds")
fixedwindow_20192020 <- readRDS("intermediate_data/fixedwindow_20192020.rds")
player_games <- readRDS("intermediate_data/player_games.rds")


# percent of players vs percent of penalties concentration plot
scatter_plot <- ggplot(data = unique_player_play_info, aes(x = playerPercentages , y = cumPlays)) +
  geom_point() +  
  labs(x = "% of players", y = "% of total penalty time",
       title = "Concentration of total penalty time among players") +
  theme_minimal() 

ggsave("results/concentration_plot.png",scatter_plot,bg = "white")


# histogram showing the distribution of total penalty minutes in each player's career
p <- ggplot(careers, aes(x = total_penalty_minutes)) +
  geom_histogram(color="black", fill="white") +
  theme_minimal() +
  labs(x= "Total minutes of penality during career", y ="Count of players",
       caption = "Median is 356 minutes (~6 hours).")

ggsave("results/total_penalty_minutes_during_career.png",p,bg="white")


#knitr::kable(mean_share_table,format = "simple")


# plot showing distribution of player's penalties throughout their careers (OLD). 
p <- ggplot(career_data,
            aes(x = game_percentile,
                y = penalty_minutes_share)) +
  geom_boxplot() +
  labs(x = "Percentile of player's career timeline (left = earlier in career)",
       y = "Share of penalty minutes over whole career",
       caption = "Each dot shows the contribution of each player's penalties\nduring the career stage to the total of penalty minutes over the career.\nEach player's career is split into 10 percentiles based on play_id.") +
  theme_minimal()

ggsave("results/old_boxplot_penalty_time_over_career.png",p,bg="white")





# histogram where player_id and game_id are of type integer
# p <- ggplot(filtered_all_plays,
#        aes(x = player_id, group = game_id, color = game_id)) +
#   geom_histogram(binwidth = 500, alpha = 1)
# print(p)

# games <- ggplot(data = all_player_plays) + 
#   geom_histogram(mapping = aes(x = game_id))
# ggsave("results/games.png",p,bg="white")


# career_games <- ggplot(data = player_games) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 10, alpha = 0.5) +
#   ggtitle("Career Games Played")
# ggsave("results/careergames.png", career_games)




#plot of number of penalties a player had in a given year
filtered_season20192020 <- filter(season20192020, playerType == "PenaltyOn")
filtered_season20192020 <- distinct(filtered_season20192020)
plays20192020 <- ggplot(filtered_season20192020,
                    aes(x = player_id)) +
  geom_bar(stat = "count")
print(plays20192020)
ggsave("results/2019PenaltyCount.png", plays20192020)



# Bar graph showing the distribution of all of the games played by players who had 2 penalties in 2000
fixedwindow20192020 <- ggplot(fixedwindow_20192020, aes(x = player_id, y = game_count) ) +
  geom_bar(stat = "identity")
print(fixedwindow20192020)
ggsave("results/20192020twopenalties.png", fixedwindow20192020)

# player_games2020 <- year2020 %>%
#   group_by(player_id) %>%
#   summarize(game_count = n_distinct(unique(game_id)),
#             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#   )
# 
# # Print the result
# print(player_games)

#plots of total number of games played in a year for each player
game_count_bar <- ggplot(data = player_games) +
  geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5)
print(game_count_bar)

# game_count_bar2000 <- ggplot(data = player_games2000) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2000 Games Played")
# print(game_count_bar2000)
# 
# game_count_bar2001 <- ggplot(data = player_games2001) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2001 Games Played")
# print(game_count_bar2001)
# 
# game_count_bar2002 <- ggplot(data = player_games2002) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2002 Games Played")
# print(game_count_bar2002)
# 
# game_count_bar2003 <- ggplot(data = player_games2003) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2003 Games Played")
# print(game_count_bar2003)
# 
# game_count_bar2004 <- ggplot(data = player_games2004) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2004 Games Played")
# print(game_count_bar2004)
# 
# game_count_bar2005 <- ggplot(data = player_games2005) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2005 Games Played")
# print(game_count_bar2005)
# 
# game_count_bar2006 <- ggplot(data = player_games2006) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2006 Games Played")
# print(game_count_bar2006)
# 
# game_count_bar2007 <- ggplot(data = player_games2007) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2007 Games Played")
# print(game_count_bar2007)
# 
# game_count_bar2008 <- ggplot(data = player_games2008) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2008 Games Played")
# print(game_count_bar2008)
# 
# game_count_bar2009 <- ggplot(data = player_games2009) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2009 Games Played")
# print(game_count_bar2009)
# 
# game_count_bar2010 <- ggplot(data = player_games2010) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2010 Games Played")
# print(game_count_bar2010)
# 
# game_count_bar2011 <- ggplot(data = player_games2011) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2011 Games Played")
# print(game_count_bar2011)
# 
# game_count_bar2012 <- ggplot(data = player_games2012) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2012 Games Played")
# print(game_count_bar2012)
# 
# game_count_bar2013 <- ggplot(data = player_games2013) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2013 Games Played")
# print(game_count_bar2013)
# 
# game_count_bar2014 <- ggplot(data = player_games2014) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2014 Games Played")
# print(game_count_bar2014)
# 
# game_count_bar2015 <- ggplot(data = player_games2015) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2015 Games Played")
# print(game_count_bar2015)
# 
# game_count_bar2016 <- ggplot(data = player_games2016) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2016 Games Played")
# print(game_count_bar2016)
# 
# game_count_bar2017 <- ggplot(data = player_games2017) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2017 Games Played")
# print(game_count_bar2017)
# 
# game_count_bar2018 <- ggplot(data = player_games2018) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 1.5) +
#   ggtitle("2018 Games Played")
# print(game_count_bar2018)
# 
# game_count_bar2019 <- ggplot(data = player_games2019) +
#   geom_bar(mapping = aes(x = player_id, y = game_count), stat = "identity", width = 10, alpha = 0.5) +
#   ggtitle("2019 Games Played")
# print(game_count_bar2019)
# ggsave("results/2019gamesplayed.png", game_count_bar2019)






  