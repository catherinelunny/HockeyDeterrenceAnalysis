library(tidyverse)
library(ggplot2)
library(patchwork)

player_penalties <- read.csv("data/plays_players.csv")
penalties <- read.csv("data/game_penalties.csv")
players <- read.csv("data/player_info.csv")
plays_players1 <- readRDS("data/plays_players1.rds")
games <- read.csv("data/games.csv")

filtered_penalties <- readRDS("intermediate_data/filtered_penalties.rds")
games_ordered <- readRDS("data/games_ordered.rds")
complete_penalty_info <- readRDS("intermediate_data/complete_penalty_info.rds")
all_player_plays <- readRDS("intermediate_data/all_player_plays.rds")
careers <- readRDS("intermediate_data/careers.rds")
career_data <- readRDS("intermediate_data/career_data.rds")
mean_share_table <- readRDS("intermediate_data/mean_share_table.rds")
player_games <- readRDS("intermediate_data/player_games.rds")
tab_games_by_players <- readRDS("intermediate_data/tab_games_by_players.rds")
tab_seasons_by_players <- readRDS("intermediate_data/tab_seasons_by_players.rds")
tab_penalties_by_players <- readRDS("intermediate_data/tab_penalties_by_players.rds")
normalized_player_data <- readRDS("intermediate_data/normalized_player_data.rds")
averages <- readRDS("intermediate_data/averages.rds")
player_game_count <- readRDS("intermediate_data/player_game_count.rds")

source("code/function_waiting_times.R")
source("code/function_most_common_num_pens.R")
source("code/function_fixednumber_penalties_season.R")
source("code/function_season_plays.R")
source("code/function_game_count_season.R")
source("code/function_player_games_season.R")

# # all of the steps to make the plot for one waiting time. This is what is used for all of the different penalty amounts, the specific year and index data frame just has to be brought in
# 
# # calling the list of data frames created by the waiting_times function for the season and then going data frame by data frame in the list of waiting times
# minorwaitingtimesseason <- waiting_times(year, "Minor") (put specific season in)
# minorwaitingtimesseason_index <- as.data.frame(minorwaitingtimes20192020[index]) (index in the waiting_times list- 1 is the most frequent number of repeat Minor penalties, 2 is second most, 3 is third most)
# 
# # the highest game count for a player in the given data frame, used to determine the axis of the plot and the largest possible waiting time to determine how many rows are needed in the minorwaitingtimesdf
# most_games <- max(minorwaitingtimesseason_index$game_count)
# 
# # dataframe counting the observed amount of each observed waiting time for the plot
# counts_obs <- minorwaitingtimesseason_index %>%
#   group_by(game_diff) %>%
#   summarize(obs_count = n())
# 
# # main dataframe that will have all possible waiting times and counts of the observed and random iterations. this can be the same for all seasons when you do it
# minorwaitingtimes <- data.frame(game_diff = 0:most_games)
# minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
# # if there is a game difference in the range that is not observed, it will come up as NA. since we want to look at all waiting times, this command changes the NA to 0
# for (diff in 1:nrow(minorwaitingtimes)) {
#   if (is.na(minorwaitingtimes$obs_count[diff])) {
#     minorwaitingtimes$obs_count[diff] <- 0
#   } 
# }
# 
# 
# #loop picking random games out of the games each player played each season to have penalties in
# # can set the amount of loops higher, will have to change the range of values below to the new upper bound + 2. ie in this case we are looking at i in 1:100, so the minorwaitingtimes data frame will have 102 columns (the 100 random plus the observed and the player ids)
# # list that will hold the results of all 100 iterations
# randomminorwaitingtimes_list <- list()
# for (i in 1:100) {
#   # list that will hold the results of each player in each iteration
#   randomminorwaitingtimes1_list <- list()
#   for (player in minorwaitingtimesseason_index$player_id) {
#     num_games <- minorwaitingtimesseason_index$game_count[minorwaitingtimesseason_index$player_id == player] (each player's game count)
#     num_penalties <- sum(minorwaitingtimesseason_index$player_id == player) + 1 (# of pens each player had in the season. will be the same for each player from previous filtering. + 1 b/c minorwaitingtimesseason_index amount of waiting tims is the amount of penalties - 1)
#     randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
#     randomgames_ordered <- sort(randomgames)
#     player_diffs <- filter(minorwaitingtimesseason_index, player_id == player) (create a data frame for each specfic player and then will be combined later)
#     
#     randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
#     for (game in 1:(num_penalties - 1)){ ( -1 since the last penalty does not have a waiting time after it)
#       penalty_game <- randomgames_ordered[game] (going through each random game and calculating waiting times, then adding to df and storing in the list for each player)
#       randomminorwaitingtimes$penalty_game[game] <- penalty_game
#       
#       next_penalty_game <- randomgames_ordered[game + 1]
#       randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
#       
#       game_diff <- abs(next_penalty_game - penalty_game)
#       randomminorwaitingtimes$game_diff[game] <- game_diff
#       
#       randomminorwaitingtimes$game_count[game] <- num_games
#     }
#     randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
#   }
#   
#   
#   randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
#   randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
#   
#   
#   game_counts <- randomminorwaitingtimes_list[[i]] %>% (game_diff counts for each iteration)
#     group_by(game_diff) %>%
#     summarize(count = n())
#   
#   counts <- data.frame(game_diff = 0:most_games)
#   counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
#   for (diff in 1:nrow(minorwaitingtimes)) {
#     if (is.na(counts$count[diff])) {
#       counts$count[diff] <- 0
#     }
#   }
#   colname <- paste0("count_simulation_", i) (adding the counts for each iteration to the main df)
#   minorwaitingtimes[[colname]] <- counts$count
# }
# 
# # loop for getting the upper and lower bounds for the plot
# quantile_list <- list()
# 
# # pulling the values from each row (which are the counts for each waiting time from each iteration)
# for (i in 1:nrow(minorwaitingtimes)) {
#   values <- as.numeric(minorwaitingtimes[i, 3:102])
#   quantiles <- quantile(values, probs = c(0.025, 0.975))
#   quantile_list[[i]] <- quantiles
# }
# 
# quantiles_df <- do.call(rbind, quantile_list) (combining all of the individual quantiles so everything is in one df)
# # making a data frame with all of the simulation values and quantiles
# obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
# minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)
# 
# minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
#   rename(lower_bound = "2.5%",
#          upper_bound = "97.5%")
# 
# p <- ggplot(data = minorwaitingtimes_quantiles) + 
#   geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
#   geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
#   geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
#   geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
#   geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
#   geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
#   scale_y_continuous(breaks = (0:200)) +
#   scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
#   labs(title = paste0("Waiting Times of Players with ", num_penalties, "Minor Penalties in the year-year Season"), x = "Game difference", y = "Count of players")+
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank())
# 
# p
# 
# ggsave("results/minorwaitingtimesseason_index.png",p,bg="white")
# 
# # stops here - this does everything to create the plot for the waiting times between penalties for players with the most common penalty amount

#2019-2020
#waiting times for the top 3 most frequent amount of penalties for players to have
minorwaitingtimes20192020 <- waiting_times(2019, "Minor")
minorwaitingtimes20192020_1 <- as.data.frame(minorwaitingtimes20192020[1])

most_games <- max(minorwaitingtimes20192020_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20192020_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20192020_1$player_id) {
    num_games <- minorwaitingtimes20192020_1$game_count[minorwaitingtimes20192020_1$player_id == player]
    num_penalties <- sum(minorwaitingtimes20192020_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20192020_1, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20192020_1.png",p,bg="white")

#2019-2020
#waiting times for players with the second most common penalty amount

minorwaitingtimes20192020_2 <- as.data.frame(minorwaitingtimes20192020[2])

most_games <- max(minorwaitingtimes20192020_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20192020_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20192020_2$player_id) {
    num_games <- minorwaitingtimes20192020_2$game_count[minorwaitingtimes20192020_2$player_id == player]
    num_penalties <- sum(minorwaitingtimes20192020_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20192020_2, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20192020_2.png",p,bg="white")

#2019-2020
#waiting times for players with the third most common penalty amount

minorwaitingtimes20192020_3 <- as.data.frame(minorwaitingtimes20192020[3])

most_games <- max(minorwaitingtimes20192020_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20192020_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20192020_3$player_id) {
    num_games <- minorwaitingtimes20192020_3$game_count[minorwaitingtimes20192020_3$player_id == player]
    num_penalties <- sum(minorwaitingtimes20192020_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20192020_3, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20192020_3.png",p,bg="white")


#2018-2019
#waiting times for players with the most common penalty amount

minorwaitingtimes20182019 <- waiting_times(2018, "Minor")
minorwaitingtimes20182019_1 <- as.data.frame(minorwaitingtimes20182019[1])

most_games <- max(minorwaitingtimes20182019_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20182019_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20182019_1$player_id) {
    num_games <- minorwaitingtimes20182019_1$game_count[minorwaitingtimes20182019_1$player_id == player]
    num_penalties <- sum(minorwaitingtimes20182019_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20182019_1, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2018-2019 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20182019_1.png",p,bg="white")

#2018-2019
#waiting times for players with the second most common penalty amount

minorwaitingtimes20182019_2 <- as.data.frame(minorwaitingtimes20182019[2])

most_games <- max(minorwaitingtimes20182019_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20182019_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20182019_2$player_id) {
    num_games <- minorwaitingtimes20182019_2$game_count[minorwaitingtimes20182019_2$player_id == player]
    num_penalties <- sum(minorwaitingtimes20182019_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20182019_2, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2018-2019 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20182019_2.png",p,bg="white")

#2018-2019
#waiting times for players with the third most common penalty amount

minorwaitingtimes20182019_3 <- as.data.frame(minorwaitingtimes20182019[3])

most_games <- max(minorwaitingtimes20182019_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20182019_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20182019_3$player_id) {
    num_games <- minorwaitingtimes20182019_3$game_count[minorwaitingtimes20182019_3$player_id == player]
    num_penalties <- sum(minorwaitingtimes20182019_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20182019_3, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2018-2019 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20182019_3.png",p,bg="white")



#2017-2018
#waiting times for players with the most common penalty amount

minorwaitingtimes20172018 <- waiting_times(2017, "Minor")
minorwaitingtimes20172018_1 <- as.data.frame(minorwaitingtimes20172018[1])

most_games <- max(minorwaitingtimes20172018_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20172018_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20172018_1$player_id) {
    num_games <- minorwaitingtimes20172018_1$game_count[minorwaitingtimes20172018_1$player_id == player]
    num_penalties <- sum(minorwaitingtimes20172018_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20172018_1, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2017-2018 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20172018_1.png",p,bg="white")

#2017-2018
#waiting times for players with the second most common penalty amount

minorwaitingtimes20172018_2 <- as.data.frame(minorwaitingtimes20172018[2])

most_games <- max(minorwaitingtimes20172018_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20172018_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20172018_2$player_id) {
    num_games <- minorwaitingtimes20172018_2$game_count[minorwaitingtimes20172018_2$player_id == player]
    num_penalties <- sum(minorwaitingtimes20172018_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20172018_2, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2017-2018 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20172018_2.png",p,bg="white")

#2017-2018
#waiting times for players with the third most common penalty amount

minorwaitingtimes20172018_3 <- as.data.frame(minorwaitingtimes20172018[3])

most_games <- max(minorwaitingtimes20172018_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20172018_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20172018_3$player_id) {
    num_games <- minorwaitingtimes20172018_3$game_count[minorwaitingtimes20172018_3$player_id == player]
    num_penalties <- sum(minorwaitingtimes20172018_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20172018_3, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2017-2018 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20172018_3.png",p,bg="white")


#2016-2017
#waiting times for players with the most common penalty amount

minorwaitingtimes20162017 <- waiting_times(2016, "Minor")
minorwaitingtimes20162017_1 <- as.data.frame(minorwaitingtimes20162017[1])

most_games <- max(minorwaitingtimes20162017_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20162017_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20162017_1$player_id) {
    num_games <- minorwaitingtimes20162017_1$game_count[minorwaitingtimes20162017_1$player_id == player]
    num_penalties <- sum(minorwaitingtimes20162017_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20162017_1, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2016-2017 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20162017_1.png",p,bg="white")

#2016-2017
#waiting times for players with the second most common penalty amount

minorwaitingtimes20162017_2 <- as.data.frame(minorwaitingtimes20162017[2])

most_games <- max(minorwaitingtimes20162017_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20162017_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20162017_2$player_id) {
    num_games <- minorwaitingtimes20162017_2$game_count[minorwaitingtimes20162017_2$player_id == player]
    num_penalties <- sum(minorwaitingtimes20162017_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20162017_2, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2016-2017 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20162017_2.png",p,bg="white")

#2016-2017
#waiting times for players with the third most common penalty amount

minorwaitingtimes20162017_3 <- as.data.frame(minorwaitingtimes20162017[3])

most_games <- max(minorwaitingtimes20162017_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20162017_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20162017_3$player_id) {
    num_games <- minorwaitingtimes20162017_3$game_count[minorwaitingtimes20162017_3$player_id == player]
    num_penalties <- sum(minorwaitingtimes20162017_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20162017_3, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2016-2017 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20162017_3.png",p,bg="white")


#2015-2016
#waiting times for players with the most common penalty amount

minorwaitingtimes20152016 <- waiting_times(2015, "Minor")
minorwaitingtimes20152016_1 <- as.data.frame(minorwaitingtimes20152016[1])

most_games <- max(minorwaitingtimes20152016_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20152016_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20152016_1$player_id) {
    num_games <- minorwaitingtimes20152016_1$game_count[minorwaitingtimes20152016_1$player_id == player]
    num_penalties <- sum(minorwaitingtimes20152016_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20152016_1, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2015-2016 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20152016_1.png",p,bg="white")

#2015-2016
#waiting times for players with the second most common penalty amount

minorwaitingtimes20152016_2 <- as.data.frame(minorwaitingtimes20152016[2])

most_games <- max(minorwaitingtimes20152016_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20152016_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20152016_2$player_id) {
    num_games <- minorwaitingtimes20152016_2$game_count[minorwaitingtimes20152016_2$player_id == player]
    num_penalties <- sum(minorwaitingtimes20152016_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20152016_2, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2015-2016 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20152016_2.png",p,bg="white")

#2015-2016
#waiting times for players with the third most common penalty amount

minorwaitingtimes20152016_3 <- as.data.frame(minorwaitingtimes20152016[3])

most_games <- max(minorwaitingtimes20152016_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20152016_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20152016_3$player_id) {
    num_games <- minorwaitingtimes20152016_3$game_count[minorwaitingtimes20152016_3$player_id == player]
    num_penalties <- sum(minorwaitingtimes20152016_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20152016_3, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2015-2016 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20152016_3.png",p,bg="white")


#2014-2015
# waiting times for players with the most common penalty amount

minorwaitingtimes20142015 <- waiting_times(2014, "Minor")
minorwaitingtimes20142015_1 <- as.data.frame(minorwaitingtimes20142015[1])

most_games <- max(minorwaitingtimes20142015_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20142015_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20142015_1$player_id) {
    num_games <- minorwaitingtimes20142015_1$game_count[minorwaitingtimes20142015_1$player_id == player]
    num_penalties <- sum(minorwaitingtimes20142015_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20142015_1, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2014-2015 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20142015_1.png",p,bg="white")

#2014-2015
#waiting times for players with the second most common penalty amount

minorwaitingtimes20142015_2 <- as.data.frame(minorwaitingtimes20142015[2])

most_games <- max(minorwaitingtimes20142015_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20142015_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20142015_2$player_id) {
    num_games <- minorwaitingtimes20142015_2$game_count[minorwaitingtimes20142015_2$player_id == player]
    num_penalties <- sum(minorwaitingtimes20142015_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20142015_2, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2014-2015 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20142015_2.png",p,bg="white")

#2014-2015
#waiting times for players with the third most common penalty amount

minorwaitingtimes20142015_3 <- as.data.frame(minorwaitingtimes20142015[3])

most_games <- max(minorwaitingtimes20142015_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- minorwaitingtimes20142015_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
minorwaitingtimes <- data.frame(game_diff = 0:most_games)
minorwaitingtimes <- merge(minorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(minorwaitingtimes)) {
  if (is.na(minorwaitingtimes$obs_count[diff])) {
    minorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomminorwaitingtimes_list <- list()
for (i in 1:100) {
  randomminorwaitingtimes1_list <- list()
  for (player in minorwaitingtimes20142015_3$player_id) {
    num_games <- minorwaitingtimes20142015_3$game_count[minorwaitingtimes20142015_3$player_id == player]
    num_penalties <- sum(minorwaitingtimes20142015_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(minorwaitingtimes20142015_3, player_id == player)
    
    randomminorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomminorwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomminorwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomminorwaitingtimes$game_diff[game] <- game_diff
      
      randomminorwaitingtimes$game_count <- num_games
    }
    randomminorwaitingtimes1_list[[player]] <- randomminorwaitingtimes
  }
  
  randomminorwaitingtimes1 <- do.call(rbind, randomminorwaitingtimes1_list)
  randomminorwaitingtimes_list[[i]] <- as.data.frame(randomminorwaitingtimes1)
  
  
  game_counts <- randomminorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(minorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  minorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(minorwaitingtimes)) {
  values <- as.numeric(minorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsminorwaitingtimes <- minorwaitingtimes[, c("game_diff", "obs_count")]
minorwaitingtimes_quantiles <- cbind(obsminorwaitingtimes, quantiles_df)

minorwaitingtimes_quantiles <- minorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = minorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:200)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Minor Penalties in the 2014-2015 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/minorwaitingtimes20142015_3.png",p,bg="white")