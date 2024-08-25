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

waiting_times <- load("~/HockeyDeterrenceAnalysis/intermediate_data/waiting_times.RData")
most_common_num_pens <- load("~/HockeyDeterrenceAnalysis/intermediate_data/most_common_num_pens.RData")
fixednumber_penalties_season <- load("~/HockeyDeterrenceAnalysis/intermediate_data/fixednumber_penalties_season.RData")
season_plays <- load("~/HockeyDeterrenceAnalysis/intermediate_data/season_plays.RData")
game_count_season <- load("~/HockeyDeterrenceAnalysis/intermediate_data/game_count_season.RData")
player_games_season <- load("~/HockeyDeterrenceAnalysis/intermediate_data/player_games_season.RData")

#2019-2020
#waiting times for the top 3 most frequent amount of penalties for players to have
majormajorwaitingtimes20192020 <- waiting_times(2019, "Major")
majormajorwaitingtimes20192020_1 <- as.data.frame(majormajorwaitingtimes20192020[1])

most_games <- max(majormajorwaitingtimes20192020_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majormajorwaitingtimes20192020_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majormajorwaitingtimes <- data.frame(game_diff = 0:most_games)
majormajorwaitingtimes <- merge(majormajorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majormajorwaitingtimes)) {
  if (is.na(majormajorwaitingtimes$obs_count[diff])) {
    majormajorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajormajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajormajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majormajorwaitingtimes20192020_1$player_id)) {
    for (player in majormajorwaitingtimes20192020_1$player_id) {
      num_games <- majormajorwaitingtimes20192020_1$game_count[majormajorwaitingtimes20192020_1$player_id == player]
      num_penalties <- sum(majormajorwaitingtimes20192020_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majormajorwaitingtimes20192020_1, player_id == player)
      
      randommajormajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajormajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajormajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajormajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajormajorwaitingtimes1_list[[p]] <- randommajormajorwaitingtimes
  }
  
  randommajormajorwaitingtimes <- do.call(rbind, randommajormajorwaitingtimes1_list)
  randommajormajorwaitingtimes_list[[i]] <- as.data.frame(randommajormajorwaitingtimes)
  
  
  game_counts <- randommajormajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majormajorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majormajorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majormajorwaitingtimes)) {
  values <- as.numeric(majormajorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajormajorwaitingtimes <- majormajorwaitingtimes[, c("game_diff", "obs_count")]
majormajorwaitingtimes_quantiles <- cbind(obsmajormajorwaitingtimes, quantiles_df)

majormajorwaitingtimes_quantiles <- majormajorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majormajorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majormajormajorwaitingtimes20192020_1.png",p,bg="white")

#2019-2020
#waiting times for players with the second most common penalty amount

majormajorwaitingtimes20192020_2 <- as.data.frame(majormajorwaitingtimes20192020[2])

most_games <- max(majormajorwaitingtimes20192020_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majormajorwaitingtimes20192020_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majormajorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20192020_2$player_id)) {
    for (player in majorwaitingtimes20192020_2$player_id) {
      num_games <- majorwaitingtimes20192020_2$game_count[majorwaitingtimes20192020_2$player_id == player]
      num_penalties <- sum(majorwaitingtimes20192020_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20192020_2, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20192020_2.png",p,bg="white")

#2019-2020
#waiting times for players with the third most common penalty amount

majorwaitingtimes20192020_3 <- as.data.frame(majorwaitingtimes20192020[3])

most_games <- max(majorwaitingtimes20192020_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20192020_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20192020_3$player_id)) {
    for (player in majorwaitingtimes20192020_3$player_id) {
      num_games <- majorwaitingtimes20192020_3$game_count[majorwaitingtimes20192020_3$player_id == player]
      num_penalties <- sum(majorwaitingtimes20192020_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20192020_3, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2019-2020 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20192020_3.png",p,bg="white")


#2018-2019
#waiting times for players with the most common penalty amount

majorwaitingtimes20182019 <- waiting_times(2018, "Major")
majorwaitingtimes20182019_1 <- as.data.frame(majorwaitingtimes20182019[1])

most_games <- max(majorwaitingtimes20182019_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20182019_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20182019_1$player_id)) {
    for (player in majorwaitingtimes20182019_1$player_id) {
      num_games <- majorwaitingtimes20182019_1$game_count[majorwaitingtimes20182019_1$player_id == player]
      num_penalties <- sum(majorwaitingtimes20182019_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20182019_1, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2018-2019 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20182019_1.png",p,bg="white")

#2018-2019
#waiting times for players with the second most common penalty amount

majorwaitingtimes20182019_2 <- as.data.frame(majorwaitingtimes20182019[2])

most_games <- max(majorwaitingtimes20182019_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20182019_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20182019_2$player_id)) {
    for (player in majorwaitingtimes20182019_2$player_id) {
      num_games <- majorwaitingtimes20182019_2$game_count[majorwaitingtimes20182019_2$player_id == player]
      num_penalties <- sum(majorwaitingtimes20182019_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20182019_2, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2018-2019 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20182019_2.png",p,bg="white")

#2018-2019
#waiting times for players with the third most common penalty amount

majorwaitingtimes20182019_3 <- as.data.frame(majorwaitingtimes20182019[3])

most_games <- max(majorwaitingtimes20182019_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20182019_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20182019_3$player_id)) {
    for (player in majorwaitingtimes20182019_3$player_id) {
      num_games <- majorwaitingtimes20182019_3$game_count[majorwaitingtimes20182019_3$player_id == player]
      num_penalties <- sum(majorwaitingtimes20182019_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20182019_3, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2018-2019 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20182019_3.png",p,bg="white")



#2017-2018
#waiting times for players with the most common penalty amount

majorwaitingtimes20172018 <- waiting_times(2017, "Major")
majorwaitingtimes20172018_1 <- as.data.frame(majorwaitingtimes20172018[1])

most_games <- max(majorwaitingtimes20172018_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20172018_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20172018_1$player_id)) {
    for (player in majorwaitingtimes20172018_1$player_id) {
      num_games <- majorwaitingtimes20172018_1$game_count[majorwaitingtimes20172018_1$player_id == player]
      num_penalties <- sum(majorwaitingtimes20172018_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20172018_1, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2017-2018 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20172018_1.png",p,bg="white")

#2017-2018
#waiting times for players with the second most common penalty amount

majorwaitingtimes20172018_2 <- as.data.frame(majorwaitingtimes20172018[2])

most_games <- max(majorwaitingtimes20172018_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20172018_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20172018_2$player_id)) {
    for (player in majorwaitingtimes20172018_2$player_id) {
      num_games <- majorwaitingtimes20172018_2$game_count[majorwaitingtimes20172018_2$player_id == player]
      num_penalties <- sum(majorwaitingtimes20172018_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20172018_2, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2017-2018 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20172018_2.png",p,bg="white")

#2017-2018
#waiting times for players with the third most common penalty amount

majorwaitingtimes20172018_3 <- as.data.frame(majorwaitingtimes20172018[3])

most_games <- max(majorwaitingtimes20172018_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20172018_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20172018_3$player_id)) {
    for (player in majorwaitingtimes20172018_3$player_id) {
      num_games <- majorwaitingtimes20172018_3$game_count[majorwaitingtimes20172018_3$player_id == player]
      num_penalties <- sum(majorwaitingtimes20172018_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20172018_3, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2017-2018 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20172018_3.png",p,bg="white")


#2016-2017
#waiting times for players with the most common penalty amount

majorwaitingtimes20162017 <- waiting_times(2016, "Major")
majorwaitingtimes20162017_1 <- as.data.frame(majorwaitingtimes20162017[1])

most_games <- max(majorwaitingtimes20162017_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20162017_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20162017_1$player_id)) {
    for (player in majorwaitingtimes20162017_1$player_id) {
      num_games <- majorwaitingtimes20162017_1$game_count[majorwaitingtimes20162017_1$player_id == player]
      num_penalties <- sum(majorwaitingtimes20162017_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20162017_1, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2016-2017 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20162017_1.png",p,bg="white")

#2016-2017
#waiting times for players with the second most common penalty amount

majorwaitingtimes20162017_2 <- as.data.frame(majorwaitingtimes20162017[2])

most_games <- max(majorwaitingtimes20162017_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20162017_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20162017_2$player_id)) {
    for (player in majorwaitingtimes20162017_2$player_id) {
      num_games <- majorwaitingtimes20162017_2$game_count[majorwaitingtimes20162017_2$player_id == player]
      num_penalties <- sum(majorwaitingtimes20162017_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20162017_2, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2016-2017 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20162017_2.png",p,bg="white")

#2016-2017
#waiting times for players with the third most common penalty amount

majorwaitingtimes20162017_3 <- as.data.frame(majorwaitingtimes20162017[3])

most_games <- max(majorwaitingtimes20162017_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20162017_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20162017_3$player_id)) {
    for (player in majorwaitingtimes20162017_3$player_id) {
      num_games <- majorwaitingtimes20162017_3$game_count[majorwaitingtimes20162017_3$player_id == player]
      num_penalties <- sum(majorwaitingtimes20162017_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20162017_3, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2016-2017 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20162017_3.png",p,bg="white")


#2015-2016
#waiting times for players with the most common penalty amount

majorwaitingtimes20152016 <- waiting_times(2015, "Major")
majorwaitingtimes20152016_1 <- as.data.frame(majorwaitingtimes20152016[1])

most_games <- max(majorwaitingtimes20152016_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20152016_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20152016_1$player_id)) {
    for (player in majorwaitingtimes20152016_1$player_id) {
      num_games <- majorwaitingtimes20152016_1$game_count[majorwaitingtimes20152016_1$player_id == player]
      num_penalties <- sum(majorwaitingtimes20152016_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20152016_1, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2015-2016 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20152016_1.png",p,bg="white")

#2015-2016
#waiting times for players with the second most common penalty amount

majorwaitingtimes20152016_2 <- as.data.frame(majorwaitingtimes20152016[2])

most_games <- max(majorwaitingtimes20152016_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20152016_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20152016_2$player_id)) {
    for (player in majorwaitingtimes20152016_2$player_id) {
      num_games <- majorwaitingtimes20152016_2$game_count[majorwaitingtimes20152016_2$player_id == player]
      num_penalties <- sum(majorwaitingtimes20152016_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20152016_2, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2015-2016 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20152016_2.png",p,bg="white")

#2015-2016
#waiting times for players with the third most common penalty amount

majorwaitingtimes20152016_3 <- as.data.frame(majorwaitingtimes20152016[3])

most_games <- max(majorwaitingtimes20152016_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20152016_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20152016_3$player_id)) {
    for (player in majorwaitingtimes20152016_3$player_id) {
      num_games <- majorwaitingtimes20152016_3$game_count[majorwaitingtimes20152016_3$player_id == player]
      num_penalties <- sum(majorwaitingtimes20152016_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20152016_3, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2015-2016 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20152016_3.png",p,bg="white")


#2014-2015
# waiting times for players with the most common penalty amount

majorwaitingtimes20142015 <- waiting_times(2014, "Major")
majorwaitingtimes20142015_1 <- as.data.frame(majorwaitingtimes20142015[1])

most_games <- max(majorwaitingtimes20142015_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20142015_1 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20142015_1$player_id)) {
    for (player in majorwaitingtimes20142015_1$player_id) {
      num_games <- majorwaitingtimes20142015_1$game_count[majorwaitingtimes20142015_1$player_id == player]
      num_penalties <- sum(majorwaitingtimes20142015_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20142015_1, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2014-2015 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20142015_1.png",p,bg="white")

#2014-2015
#waiting times for players with the second most common penalty amount

majorwaitingtimes20142015_2 <- as.data.frame(majorwaitingtimes20142015[2])

most_games <- max(majorwaitingtimes20142015_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20142015_2 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20142015_2$player_id)) {
    for (player in majorwaitingtimes20142015_2$player_id) {
      num_games <- majorwaitingtimes20142015_2$game_count[majorwaitingtimes20142015_2$player_id == player]
      num_penalties <- sum(majorwaitingtimes20142015_2$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20142015_2, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2014-2015 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20142015_2.png",p,bg="white")

#2014-2015
#waiting times for players with the third most common penalty amount

majorwaitingtimes20142015_3 <- as.data.frame(majorwaitingtimes20142015[3])

most_games <- max(majorwaitingtimes20142015_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- majorwaitingtimes20142015_3 %>%
  group_by(game_diff) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
majorwaitingtimes <- data.frame(game_diff = 0:most_games)
majorwaitingtimes <- merge(majorwaitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
for (diff in 1:nrow(majorwaitingtimes)) {
  if (is.na(majorwaitingtimes$obs_count[diff])) {
    majorwaitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
randommajorwaitingtimes_list <- list()
for (i in 1:100) {
  randommajorwaitingtimes1_list <- list()
  for (p in 1:n_distinct(majorwaitingtimes20142015_3$player_id)) {
    for (player in majorwaitingtimes20142015_3$player_id) {
      num_games <- majorwaitingtimes20142015_3$game_count[majorwaitingtimes20142015_3$player_id == player]
      num_penalties <- sum(majorwaitingtimes20142015_3$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(majorwaitingtimes20142015_3, player_id == player)
      
      randommajorwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames_ordered[game]
        randommajorwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames_ordered[game + 1]
        randommajorwaitingtimes$next_penalty_game[game] <- next_penalty_game
        
        game_diff <- abs(next_penalty_game - penalty_game)
        randommajorwaitingtimes$game_diff[game] <- game_diff
      }
    }
    randommajorwaitingtimes1_list[[p]] <- randommajorwaitingtimes
  }
  
  randommajorwaitingtimes <- do.call(rbind, randommajorwaitingtimes1_list)
  randommajorwaitingtimes_list[[i]] <- as.data.frame(randommajorwaitingtimes)
  
  
  game_counts <- randommajorwaitingtimes_list[[i]] %>%
    group_by(game_diff) %>%
    summarize(count = n())
  
  counts <- data.frame(game_diff = 0:most_games)
  counts <- merge(counts, game_counts, by = "game_diff", all.x = TRUE)
  for (diff in 1:nrow(majorwaitingtimes)) {
    if (is.na(counts$count[diff])) {
      counts$count[diff] <- 0
    }
  }
  colname <- paste0("count_simulation_", i)
  majorwaitingtimes[[colname]] <- counts$count
}

# loop for getting the upper and lower bounds
quantile_list <- list()

# pulling the values from each row (which are the counts for each waiting time from each iteration)
for (i in 1:nrow(majorwaitingtimes)) {
  values <- as.numeric(majorwaitingtimes[i, 3:102])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
obsmajorwaitingtimes <- majorwaitingtimes[, c("game_diff", "obs_count")]
majorwaitingtimes_quantiles <- cbind(obsmajorwaitingtimes, quantiles_df)

majorwaitingtimes_quantiles <- majorwaitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = majorwaitingtimes_quantiles) + 
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(title = paste0("Waiting Times of Players with ", num_penalties, " Penalties in the 2014-2015 Season"), x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/majorwaitingtimes20142015_3.png",p,bg="white")