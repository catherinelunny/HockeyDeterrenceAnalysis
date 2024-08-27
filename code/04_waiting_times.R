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
normalized_player_data <- readRDS("intermediate_data/normalized_player_data.rds")
averages <- readRDS("intermediate_data/averages.rds")
player_game_count <- readRDS("intermediate_data/player_game_count.rds")

load("~/HockeyDeterrenceAnalysis/intermediate_data/waiting_times.RData")
load("~/HockeyDeterrenceAnalysis/intermediate_data/most_common_num_pens.RData")
load("~/HockeyDeterrenceAnalysis/intermediate_data/fixednumber_penalties_season.RData")
load("~/HockeyDeterrenceAnalysis/intermediate_data/season_plays.RData")
load("~/HockeyDeterrenceAnalysis/intermediate_data/game_count_season.RData")
load("~/HockeyDeterrenceAnalysis/intermediate_data/player_games_season.RData")


#2019-2020
#waiting times for the top 3 most frequent amount of penalties for players to have

# calling the list of data frames created by the waiting_times function for the season and then going data frame by data frame in the list of waiting times
waitingtimes20192020 <- waiting_times(2019)
waitingtimes20192020_1 <- as.data.frame(waitingtimes20192020[1])

# the highest game count for a player in the given data frame, used to determine the axis of the plot and the largest possible waiting time to determine how many rows are needed in the waitingtimesdf
most_games <- max(waitingtimes20192020_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20192020_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
# if there is a game difference in the range that is not observed, it will come up as NA. since we want to look at all waiting times, this command changes the NA to 0
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games each player played each season to have penalties in 
# can set the amount of loops higher, will have to change the range of values below to the new upper bound + 2. ie in this case we are looking at i in 1:100, so the waitingtimes data frame will have 102 columns (the 100 random plus the observed and the player ids)
# list that will hold the results of all 100 iterations
randomwaitingtimes_list <- list()
for (i in 1:100) {
  # list that will hold the results of each player in each iteration
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20192020_1$player_id) {
    num_games <- waitingtimes20192020_1$game_count[waitingtimes20192020_1$player_id == player]
    num_penalties <- sum(waitingtimes20192020_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20192020_1, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }

  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20192020_1.png",p,bg="white")

#2019-2020
#waiting times for players with the second most common penalty amount

waitingtimes20192020_2 <- as.data.frame(waitingtimes20192020[2])

most_games <- max(waitingtimes20192020_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20192020_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20192020_2$player_id) {
    num_games <- waitingtimes20192020_2$game_count[waitingtimes20192020_2$player_id == player]
    num_penalties <- sum(waitingtimes20192020_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20192020_2, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2019-2020 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20192020_2.png",p,bg="white")

#2019-2020
#waiting times for players with the third most common penalty amount

waitingtimes20192020_3 <- as.data.frame(waitingtimes20192020[3])

most_games <- max(waitingtimes20192020_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20192020_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20192020_3$player_id) {
    num_games <- waitingtimes20192020_3$game_count[waitingtimes20192020_3$player_id == player]
    num_penalties <- sum(waitingtimes20192020_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20192020_3, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2019-2020 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20192020_3.png",p,bg="white")


#2018-2019
#waiting times for players with the most common penalty amount

waitingtimes20182019 <- waiting_times(2018)
waitingtimes20182019_1 <- as.data.frame(waitingtimes20182019[1])

most_games <- max(waitingtimes20182019_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20182019_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20182019_1$player_id) {
    num_games <- waitingtimes20182019_1$game_count[waitingtimes20182019_1$player_id == player]
    num_penalties <- sum(waitingtimes20182019_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20182019_1, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2018-2019 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20182019_1.png",p,bg="white")

#2018-2019
#waiting times for players with the second most common penalty amount

waitingtimes20182019_2 <- as.data.frame(waitingtimes20182019[2])

most_games <- max(waitingtimes20182019_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20182019_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20182019_2$player_id) {
    num_games <- waitingtimes20182019_2$game_count[waitingtimes20182019_2$player_id == player]
    num_penalties <- sum(waitingtimes20182019_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20182019_2, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2018-2019 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20182019_2.png",p,bg="white")

#2018-2019
#waiting times for players with the third most common penalty amount

waitingtimes20182019_3 <- as.data.frame(waitingtimes20182019[3])

most_games <- max(waitingtimes20182019_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20182019_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20182019_3$player_id) {
    num_games <- waitingtimes20182019_3$game_count[waitingtimes20182019_3$player_id == player]
    num_penalties <- sum(waitingtimes20182019_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20182019_3, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2018-2019 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20182019_3.png",p,bg="white")



#2017-2018
#waiting times for players with the most common penalty amount

waitingtimes20172018 <- waiting_times(2017)
waitingtimes20172018_1 <- as.data.frame(waitingtimes20172018[1])

most_games <- max(waitingtimes20172018_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20172018_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20172018_1$player_id) {
    num_games <- waitingtimes20172018_1$game_count[waitingtimes20172018_1$player_id == player]
    num_penalties <- sum(waitingtimes20172018_1$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20172018_1, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2017-2018 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20172018_1.png",p,bg="white")

#2017-2018
#waiting times for players with the second most common penalty amount

waitingtimes20172018_2 <- as.data.frame(waitingtimes20172018[2])

most_games <- max(waitingtimes20172018_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20172018_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20172018_2$player_id) {
    num_games <- waitingtimes20172018_2$game_count[waitingtimes20172018_2$player_id == player]
    num_penalties <- sum(waitingtimes20172018_2$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20172018_2, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2017-2018 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20172018_2.png",p,bg="white")

#2017-2018
#waiting times for players with the third most common penalty amount

waitingtimes20172018_3 <- as.data.frame(waitingtimes20172018[3])

most_games <- max(waitingtimes20172018_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20172018_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (player in waitingtimes20172018_3$player_id) {
    num_games <- waitingtimes20172018_3$game_count[waitingtimes20172018_3$player_id == player]
    num_penalties <- sum(waitingtimes20172018_3$player_id == player) + 1
    randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
    randomgames_ordered <- sort(randomgames)
    player_diffs <- filter(waitingtimes20172018_3, player_id == player)
    
    randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    for (game in 1:(num_penalties - 1)){
      penalty_game <- randomgames_ordered[game]
      randomwaitingtimes$penalty_game[game] <- penalty_game
      
      next_penalty_game <- randomgames_ordered[game + 1]
      randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- abs(next_penalty_game - penalty_game)
      randomwaitingtimes$game_diff[game] <- game_diff
      
      randomwaitingtimes$game_count[game] <- num_games
    }
    randomwaitingtimes1_list[[player]] <- randomwaitingtimes
  }
  
  
  randomwaitingtimes1 <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes1)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2017-2018 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20172018_3.png",p,bg="white")


#2016-2017
#waiting times for players with the most common penalty amount

waitingtimes20162017 <- waiting_times(2016)
waitingtimes20162017_1 <- as.data.frame(waitingtimes20162017[1])

most_games <- max(waitingtimes20162017_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20162017_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20162017_1$player_id)) {
    for (player in waitingtimes20162017_1$player_id) {
      num_games <- waitingtimes20162017_1$game_count[waitingtimes20162017_1$player_id == player]
      num_penalties <- sum(waitingtimes20162017_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20162017_1, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2016-2017 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20162017_1.png",p,bg="white")

#2016-2017
#waiting times for players with the second most common penalty amount

waitingtimes20162017_2 <- as.data.frame(waitingtimes20162017[2])

most_games <- max(waitingtimes20162017_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20162017_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20162017_2$player_id)) {
    for (player in waitingtimes20162017_2$player_id) {
      num_games <- waitingtimes20162017_2$game_count[waitingtimes20162017_2$player_id == player]
      num_penalties <- sum(waitingtimes20162017_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20162017_2, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2016-2017 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20162017_2.png",p,bg="white")

#2016-2017
#waiting times for players with the third most common penalty amount

waitingtimes20162017_3 <- as.data.frame(waitingtimes20162017[3])

most_games <- max(waitingtimes20162017_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20162017_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20162017_3$player_id)) {
    for (player in waitingtimes20162017_3$player_id) {
      num_games <- waitingtimes20162017_3$game_count[waitingtimes20162017_3$player_id == player]
      num_penalties <- sum(waitingtimes20162017_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20162017_3, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2016-2017 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20162017_3.png",p,bg="white")


#2015-2016
#waiting times for players with the most common penalty amount

waitingtimes20152016 <- waiting_times(2015)
waitingtimes20152016_1 <- as.data.frame(waitingtimes20152016[1])

most_games <- max(waitingtimes20152016_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20152016_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20152016_1$player_id)) {
    for (player in waitingtimes20152016_1$player_id) {
      num_games <- waitingtimes20152016_1$game_count[waitingtimes20152016_1$player_id == player]
      num_penalties <- sum(waitingtimes20152016_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20152016_1, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2015-2016 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20152016_1.png",p,bg="white")

#2015-2016
#waiting times for players with the second most common penalty amount

waitingtimes20152016_2 <- as.data.frame(waitingtimes20152016[2])

most_games <- max(waitingtimes20152016_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20152016_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20152016_2$player_id)) {
    for (player in waitingtimes20152016_2$player_id) {
      num_games <- waitingtimes20152016_2$game_count[waitingtimes20152016_2$player_id == player]
      num_penalties <- sum(waitingtimes20152016_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20152016_2, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2015-2016 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20152016_2.png",p,bg="white")

#2015-2016
#waiting times for players with the third most common penalty amount

waitingtimes20152016_3 <- as.data.frame(waitingtimes20152016[3])

most_games <- max(waitingtimes20152016_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20152016_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20152016_3$player_id)) {
    for (player in waitingtimes20152016_3$player_id) {
      num_games <- waitingtimes20152016_3$game_count[waitingtimes20152016_3$player_id == player]
      num_penalties <- sum(waitingtimes20152016_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20152016_3, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2015-2016 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20152016_3.png",p,bg="white")


#2014-2015
# waiting times for players with the most common penalty amount

waitingtimes20142015 <- waiting_times(2014)
waitingtimes20142015_1 <- as.data.frame(waitingtimes20142015[1])

most_games <- max(waitingtimes20142015_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20142015_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20142015_1$player_id)) {
    for (player in waitingtimes20142015_1$player_id) {
      num_games <- waitingtimes20142015_1$game_count[waitingtimes20142015_1$player_id == player]
      num_penalties <- sum(waitingtimes20142015_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20142015_1, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2014-2015 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20142015_1.png",p,bg="white")

#2014-2015
#waiting times for players with the second most common penalty amount

waitingtimes20142015_2 <- as.data.frame(waitingtimes20142015[2])

most_games <- max(waitingtimes20142015_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20142015_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20142015_2$player_id)) {
    for (player in waitingtimes20142015_2$player_id) {
      num_games <- waitingtimes20142015_2$game_count[waitingtimes20142015_2$player_id == player]
      num_penalties <- sum(waitingtimes20142015_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20142015_2, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2014-2015 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20142015_2.png",p,bg="white")

#2014-2015
#waiting times for players with the third most common penalty amount

waitingtimes20142015_3 <- as.data.frame(waitingtimes20142015[3])

most_games <- max(waitingtimes20142015_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes20142015_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randomwaitingtimes_list <- list()
for (i in 1:100) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20142015_3$player_id)) {
    for (player in waitingtimes20142015_3$player_id) {
      num_games <- waitingtimes20142015_3$game_count[waitingtimes20142015_3$player_id == player]
      num_penalties <- sum(waitingtimes20142015_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20142015_3, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randomwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randomwaitingtimes1_list[[p]] <- randomwaitingtimes
  }
  
  randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
  randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
  
  
  game_counts <- randomwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  waitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(waitingtimes)) {
  values <- as.numeric(waitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obswaitingtimes <- waitingtimes[, c("game_diff", "obs_count")]
waitingtimes_quantiles <- cbind(obswaitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:100)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2014-2015 Season"), x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20142015_3.png",p,bg="white")


# season20192020 <- readRDS("intermediate_data/season20192020.rds")
# twopenaltieswaitingtimes20192020 <- readRDS("intermediate_data/twopenaltieswaitingtimes20192020.rds")
# game_count2019 <- n_distinct(season20192020$game_id)
# penaltygames20192020 <- readRDS("intermediate_data/penaltygames20192020.rds")
# fixedwindow2pen_20192020 <- readRDS("intermediate_data/fixedwindow2pen_20192020.rds")
# fixedwindow3pen_20192020 <- readRDS("intermediate_data/fixedwindow3pen_20192020.rds") 
# fixedwindow4pen_20192020 <- readRDS("intermediate_data/fixedwindow4pen_20192020.rds")
# fourpenwaitingtimes20192020 <- readRDS("intermediate_data/fourpenwaitingtimes20192020.rds")
# threepenwaitingtimes20192020 <- readRDS("intermediate_data/threepenwaitingtimes20192020.rds")
# 
# #creating the null distribution line for the plot
# nulldist2 <- data.frame(c(0:31))
# names(nulldist2)[names(nulldist2) == "c.0.31."] <- "x"
# 
# # based on equation 9 from the burglary paper. expected distribution of order 2 games based on the number of games in the season.
# # distribution is based on the number of games in the season that has at least as many games as the given waiting time after it.
# nulldist2$y <- (2 * (game_count2019 - nulldist2$x)) / (game_count2019 * (game_count2019 + 1))
# 
# #graph of time between penalties vs. probability
# waitingtimes201920202penplot <- ggplot() +
#   geom_histogram(data = twopenaltieswaitingtimes20192020, aes(x = game_difference,
#                                                   y = distribution), stat = "identity", fill = "gray", alpha = 1.0, width = 0.4) +
#   geom_line(data = nulldist2, aes(x = x, y = y)) +
#   labs(x = "\u03C4 (Games)", y = "p(\u03C4)")
# 
# 
# waitingtimes201920202penplot
# 
# ggsave("results/2019twopenaltiesdistribution.png",waitingtimes201920202penplot)
# 
# #a list for storing the randomly generated waiting times in the loop
# waitingtimes_list <- list()
# 
# #loop picking random games out of the games they played each season to have a penalty in. Used based on the game count and picks 2 random games based on order and stores them in the list. 
# for (i in 1:1000) {
#   randompenalties <- data.frame(Player = fixedwindow2pen_20192020$player_id, first_penalty = NA, second_penalty = NA)
#   for (player in 1:nrow(randompenalties)) {
#     game_count <- fixedwindow2pen_20192020$game_count[player]
#     randomgames <- sample(1:game_count, 2, replace = TRUE)
#     randompenalties$first_penalty[player] <- randomgames[1]
#     randompenalties$second_penalty[player] <- randomgames[2]
#   }
#   waitingtimes <- randompenalties %>%
#   group_by(Player) %>%
#   summarise(game_difference = abs(first_penalty - second_penalty))
#   waitingtimes$distribution <- 1/nrow(waitingtimes)
#   
#   waitingtimes <- waitingtimes %>%
#     group_by(game_difference) %>%
#     summarise(cumDist = sum(distribution))
#   
#   waitingtimes_list[[i]] <- waitingtimes
# }
# 
# #combining all players random wait times
# all_waitingtimes <- do.call(rbind, waitingtimes_list)
# 
# # finding the quantiles for the confidence intervals
# quantiles_by_group <- all_waitingtimes %>%
#   group_by(game_difference) %>%
#   summarise(quantile_2.5 = quantile(cumDist, 0.025),
#             quantile_97.5 = quantile(cumDist, 0.975))
# 
# #graph that includes the observed distributions and the 2.5 percentile and 97.5 percentile of the random distributions
# everything2pen <- waitingtimes201920202penplot +
#   geom_histogram(data = quantiles_by_group, aes(x = game_difference, y = quantile_2.5), stat = "identity", fill = "red", alpha = 0.5) +
#   geom_histogram(data = quantiles_by_group, aes(x = game_difference, y = quantile_97.5), stat = "identity", fill = "blue", alpha = 0.5) 
# 
# # AK: rework histogram
# waitingtimes_2019_2020_count <- twopenaltieswaitingtimes20192020 %>% 
#   #mutate(distr_binned = cut(distribution,
#   #                          breaks = unique(quantile(distribution, probs = seq.int(0,1, by = 1 / max(game_difference)))), 
#   #                          include.lowest = TRUE)) %>% 
#   #mutate(distr_round = round(distribution,5)) %>% 
#   group_by(game_difference) %>% 
#   summarize(n = n()) %>% 
#   arrange(game_difference) %>% 
#   tidyr::complete(game_difference = min(game_difference):max(game_difference)) %>% 
#   mutate(n = ifelse(is.na(n),0,n)) %>% 
#   left_join(., nulldist2, by = c("game_difference" = "x")) %>% 
#   rename(distribution = y)
# 
# 
# p <- ggplot(data = waitingtimes_2019_2020_count,
#             aes(x = game_difference,
#                                         y = n,
#                                         group = 1)) +
#   geom_point() +
#   geom_line() +
#   scale_y_continuous(limits = c(0,NA)) +
#   scale_x_continuous(breaks = 1:31) +
#   labs(x = "Difference in Game Order for Player's Consecutive Penalties", y = "Count of players")+
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank())
# 
# ggsave("results/ak_waiting_times_2019_2020_count.png",p,bg="white")
# 
# # CL- adding expected counts to graph
# waitingtimes_2019_2020_expectedcount <- nulldist2 %>%
#   group_by(x) %>%
#   # Expected count of penalties for each game difference
#   summarize(n = (y*(game_count2019 * (game_count2019 + 1)))/2) %>%
#   arrange(x)
# 
# 
# g <- ggplot(data = waitingtimes_2019_2020_count,
#        aes(x = game_difference,
#                                         y = n,
#                                         group = 1)) +
#   geom_point() +
#   geom_line() +
#   scale_y_continuous(limits = c(0,NA)) +
#   scale_x_continuous(breaks = 1:31) +
#   geom_point(data = waitingtimes_2019_2020_expectedcount,
#              aes(x = x,
#                  y = n,
#                  group = 1)) +
#   geom_line(data = waitingtimes_2019_2020_expectedcount,
#             aes(x = x,
#                 y = n,
#                 group = 1)) +
#   scale_y_continuous(limits = c(0,NA)) +
#   scale_x_continuous(breaks = 1:31) +
#   labs(x = "Game difference", y = "Count of players")+
#   theme_minimal() +
#   theme(panel.grid.minor = element_blank())
# 
# ggsave("results/cl_waiting_times_2019_2020_count.png",g,bg="white")
# 
# # AK: testing if within CI:
# waitingtimes_2019_2020_count_ci <- left_join(waitingtimes_2019_2020_count,
#                                              quantiles_by_group) %>% 
#   mutate(is_within_ci = ifelse(distribution < quantile_2.5 & distribution < quantile_97.5,
#                                1,
#                                0))
# 
# head(waitingtimes_2019_2020_count_ci,10)
# 
# # 3 Penalties
# #creating the null distribution line for the plot
# nulldist3 <- data.frame(c(0:43))
# names(nulldist3)[names(nulldist3) == "c.0.43."] <- "x"
# nulldist3$y <- mapply(
#   function(game_diff) {
#     numerator <- prod((game_count2019 - game_diff + 0:1) / (game_count2019 + 0:1))
#     return(3 / (game_count2019 + 2) * numerator)
#   },
#   nulldist3$x
# )
# 
# #graph of time between penalties vs. probability (time between first and second, and then second and third)
# waitingtimes201920203penplot <- ggplot() +
#   geom_histogram(data = threepenwaitingtimes20192020, aes(x = game_difference, y = distribution), stat = "identity", alpha = 1.0, width = 2) +
#   geom_line(data = nulldist3, aes(x = x, y = y), fill = "blue") +
#   labs(x = "\u03C4 (Games)", y = "p(\u03C4)")
# 
# waitingtimes201920203penplot
# 
# ggsave("results/2019threepenaltiesdistribution.png",waitingtimes201920203penplot)
# 
# #a list for storing the randomly generated waiting times
# waitingtimes3_list <- list()
# 
# #loop picking random games out of the games they played each season to have a penalty in. Used based on the game count and picks 2 random games based on order and stores them in the list. 
# for (i in 1:1000) {
#   randompenalties3 <- data.frame(Player = fixedwindow3pen_20192020$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
#   for (player in 1:nrow(randompenalties3)) {
#     game_count <- fixedwindow3pen_20192020$game_count[player]
#     randomgames <- sort(sample(1:game_count, 3, replace = TRUE))
#     randompenalties3$first_penalty[player] <- randomgames[1]
#     randompenalties3$second_penalty[player] <- randomgames[2]
#     randompenalties3$third_penalty[player] <- randomgames[3]
#   }
#   #determining the amount of time between consecutive penalties 
#   waitingtimes3_1 <- randompenalties3 %>%
#     group_by(Player) %>%
#     summarise(game_difference = abs(first_penalty - second_penalty))
#   waitingtimes3_2 <- randompenalties3 %>%
#     group_by(Player) %>%
#     summarise(game_difference = abs(second_penalty - third_penalty))
#   waitingtimes3 <- bind_rows(waitingtimes3_1, waitingtimes3_2)
#   waitingtimes3$distribution <- mapply(
#     function(game_diff) {
#       numerator <- prod((game_count2019 - game_diff + 0:1) / (game_count2019 + 0:1))
#       return(3 / (game_count2019 + 2) * numerator)
#     },
#     waitingtimes3$game_difference
#   )
#   
#   #determining the observed prbability for each waiting time
#   waitingtimes3 <- waitingtimes3 %>%
#     group_by(game_difference) %>%
#     summarise(cumDist = sum(distribution))
#   
#   waitingtimes3_list[[i]] <- waitingtimes3
# }
# 
# all_waitingtimes3 <- do.call(rbind, waitingtimes3_list)
# 
# # determining the 2.5 and 97.5 percentiles for the confidence interval
# quantiles_by_group3 <- all_waitingtimes3 %>%
#   group_by(game_difference) %>%
#   summarise(quantile_2.5 = quantile(cumDist, 0.025),
#             quantile_97.5 = quantile(cumDist, 0.975))
# 
# #waiting times plot with confidence interval
# everything3pen <- waitingtimes201920203penplot +
#   geom_histogram(data = quantiles_by_group3, aes(x = game_difference, y = quantile_2.5), stat = "identity", fill = "red", alpha = 0.5) +
#   geom_histogram(data = quantiles_by_group3, aes(x = game_difference, y = quantile_97.5), stat = "identity", fill = "blue", alpha = 0.5) 
# 
# everything3pen
# ggsave("results/2019threepenaltiesdistribution_CI.png",everything3pen)
# 
# # 4 Penalties
# #creating the null distribution line for the plot
# nulldist4 <- data.frame(c(0:36))
# names(nulldist4)[names(nulldist4) == "c.0.36."] <- "x"
# nulldist4$y <- mapply(
#   function(game_diff) {
#     numerator <- prod((game_count2019 - game_diff + 0:2) / (game_count2019 + 0:2))
#     return(4 / (game_count2019 + 3) * numerator)
#   },
#   nulldist4$x
# )
# 
# #graph of time between penalties vs. probability (time between first and second, then second and third, then third and fourth)
# waitingtimes201920204penplot <- ggplot() +
#   geom_histogram(data = fourpenwaitingtimes20192020, aes(x = game_difference, y = distribution), stat = "identity", fill = "black", alpha = 1.0, width = 0.5) +
#   geom_line(data = nulldist4, aes(x = x, y = y)) +
#   labs(x = "\u03C4 (Games)", y = "p(\u03C4)")
# 
# waitingtimes201920204penplot
# 
# ggsave("results/2019fourpenaltiesdistribution.png",waitingtimes201920204penplot)
# 
# #a list for storing the randomly generated waiting times
# waitingtimes4_list <- list()
# 
# #loop picking random games out of the games they played each season to have a penalty in. Used based on the game count and picks 2 random games based on order and stores them in the list. 
# for (i in 1:1000) {
#   randompenalties4 <- data.frame(Player = fixedwindow4pen_20192020$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
#   for (player in 1:nrow(randompenalties4)) {
#     game_count <- fixedwindow4pen_20192020$game_count[player]
#     randomgames <- sort(sample(1:game_count, 4, replace = TRUE))
#     randompenalties4$first_penalty[player] <- randomgames[1]
#     randompenalties4$second_penalty[player] <- randomgames[2]
#     randompenalties4$third_penalty[player] <- randomgames[3]
#     randompenalties4$fourth_penalty[player] <- randomgames[4]
#   }
#   #determining the waiting times between consecutive penalties
#   waitingtimes4_1 <- randompenalties4 %>%
#     group_by(Player) %>%
#     summarise(game_difference = abs(first_penalty - second_penalty))
#   waitingtimes4_2 <- randompenalties4 %>%
#     group_by(Player) %>%
#     summarise(game_difference = abs(second_penalty - third_penalty))
#   waitingtimes4_3 <- randompenalties4 %>%
#     group_by(Player) %>%
#     summarise(game_difference = abs(third_penalty - fourth_penalty))
#   waitingtimes4 <- bind_rows(waitingtimes4_1, waitingtimes4_2, waitingtimes4_3)            
#   waitingtimes4$distribution <- mapply(
#     function(game_diff) {
#       numerator <- prod((game_count2019 - game_diff + 0:2) / (game_count2019 + 0:2))
#       return(4 / (game_count2019 + 3) * numerator)
#     },
#     waitingtimes4$game_difference
#   )
#   #determining the observed prbability for each waiting time
#   waitingtimes4 <- waitingtimes4 %>%
#     group_by(game_difference) %>%
#     summarise(cumDist = sum(distribution))
#   
#   waitingtimes4_list[[i]] <- waitingtimes4
# }
# 
# all_waitingtimes4 <- do.call(rbind, waitingtimes4_list)
# 
# 
# #determining the confidence interval
# quantiles_by_group4 <- all_waitingtimes4 %>%
#   group_by(game_difference) %>%
#   summarise(quantile_2.5 = quantile(cumDist, 0.025),
#             quantile_97.5 = quantile(cumDist, 0.975))
# 
# everything4pen <- waitingtimes201920204penplot +
#   geom_histogram(data = quantiles_by_group4, aes(x = game_difference, y = quantile_2.5), stat = "identity", fill = "red", alpha = 0.5) +
#   geom_histogram(data = quantiles_by_group4, aes(x = game_difference, y = quantile_97.5), stat = "identity", fill = "blue", alpha = 0.5) 
# 
# everything4pen
# ggsave("results/2019fourpenaltiesdistribution_CI.png",everything4pen)
