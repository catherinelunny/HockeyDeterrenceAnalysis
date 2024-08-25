library(tidyverse)
library(patchwork)

#2019-2020
#waiting times for the top 3 most frequent amount of penalties for players to have
waitingtimes2019 <- waiting_times(2019)
waitingtimes2019_1 <- waitingtimes2019[1]


most_games <- max(waitingtimes2019_1$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2019_1 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2019_1$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2019_1$game_count[player]
    randomgames <- sample(1:num_games, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  game_diffs <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = abs(first_penalty - second_penalty))
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2019_1.png",p,bg="white")

#2019-2020
#players with 3 penalties

waitingtimes2019_2 <- waitingtimes2019[2]

most_games <- max(waitingtimes2019_2$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2019_2 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2019_2$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2019_2$game_count[player]
    randomgames <- sample(1:num_games, 3, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2019_2.png",p,bg="white")

#2019-2020
#players with 4 penalties

waitingtimes2019_3 <- waitingtimes2019[3]

most_games <- max(waitingtimes2019_3$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2019_3 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2019_3$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2019_3$game_count[player]
    randomgames <- sample(1:num_games, 4, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2019_3.png",p,bg="white")

#2019-2020
#players with 5 penalties

waitingtimes5pen2019 <- waiting_times(2019, 5)

most_games <- max(waitingtimes5pen2019$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes5pen2019 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes5pen2019$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA, fifth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes5pen2019$game_count[player]
    randomgames <- sample(1:num_games, 5, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
    randompenalties$fifth_penalty[player] <- randomgames[5]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  
  game_diffs4 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fifth_penalty - fourth_penalty,
              .groups = 'drop')
  
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3, game_diffs4)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes5pen20192020.png",p,bg="white")


#2018-2019
#players with 2 penalties

waitingtimes2pen2018 <- waiting_times(2018, 2)

most_games <- max(waitingtimes2pen2018$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2pen2018 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2pen2018$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2pen2018$game_count[player]
    randomgames <- sample(1:num_games, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  game_diffs <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = abs(first_penalty - second_penalty))
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2pen20182019.png",p,bg="white")

#2018-2019
#players with 3 penalties

waitingtimes3pen2018 <- waiting_times(2018, 3)

most_games <- max(waitingtimes3pen2018$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes3pen2018 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes3pen2018$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes3pen2018$game_count[player]
    randomgames <- sample(1:num_games, 3, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes3pen20182019.png",p,bg="white")

#2018-2019
#players with 4 penalties

waitingtimes4pen2018 <- waiting_times(2018, 4)

most_games <- max(waitingtimes4pen2018$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes4pen2018 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes4pen2018$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes4pen2018$game_count[player]
    randomgames <- sample(1:num_games, 4, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes4pen20182019.png",p,bg="white")

#2018-2019
#players with 5 penalties

waitingtimes5pen2018 <- waiting_times(2018, 5)

most_games <- max(waitingtimes5pen2018$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes5pen2018 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes5pen2018$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA, fifth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes5pen2018$game_count[player]
    randomgames <- sample(1:num_games, 5, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
    randompenalties$fifth_penalty[player] <- randomgames[5]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  
  game_diffs4 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fifth_penalty - fourth_penalty,
              .groups = 'drop')
  
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3, game_diffs4)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes5pen20182019.png",p,bg="white")

#2017-2018
#players with 2 penalties

waitingtimes2pen2017 <- waiting_times(2017, 2)

most_games <- max(waitingtimes2pen2017$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2pen2017 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2pen2017$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2pen2017$game_count[player]
    randomgames <- sample(1:num_games, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  game_diffs <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = abs(first_penalty - second_penalty))
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes20172018.png",p,bg="white")

#2017-2018
#players with 3 penalties

waitingtimes3pen2017 <- waiting_times(2017, 3)

most_games <- max(waitingtimes3pen2017$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes3pen2017 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes3pen2017$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes3pen2017$game_count[player]
    randomgames <- sample(1:num_games, 3, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes3pen20172018.png",p,bg="white")

#2017-2018
#players with 4 penalties

waitingtimes4pen2017 <- waiting_times(2017, 4)

most_games <- max(waitingtimes4pen2017$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes4pen2017 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes4pen2017$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes4pen2017$game_count[player]
    randomgames <- sample(1:num_games, 4, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes4pen20172018.png",p,bg="white")

#2017-2018
#players with 5 penalties

waitingtimes5pen2017 <- waiting_times(2017, 5)

most_games <- max(waitingtimes5pen2017$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes5pen2017 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes5pen2017$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA, fifth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes5pen2017$game_count[player]
    randomgames <- sample(1:num_games, 5, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
    randompenalties$fifth_penalty[player] <- randomgames[5]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  
  game_diffs4 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fifth_penalty - fourth_penalty,
              .groups = 'drop')
  
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3, game_diffs4)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes5pen20172018.png",p,bg="white")

#2016-2017
#players with 2 penalties

waitingtimes2pen2016 <- waiting_times(2016, 2)

most_games <- max(waitingtimes2pen2016$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2pen2016 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2pen2016$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2pen2016$game_count[player]
    randomgames <- sample(1:num_games, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  game_diffs <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = abs(first_penalty - second_penalty))
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2pen20162017.png",p,bg="white")

#2016-2017
#players with 3 penalties

waitingtimes3pen2016 <- waiting_times(2016, 3)

most_games <- max(waitingtimes3pen2016$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes3pen2016 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes3pen2016$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes3pen2016$game_count[player]
    randomgames <- sample(1:num_games, 3, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes3pen20162017.png",p,bg="white")

#2016-2017
#players with 4 penalties

waitingtimes4pen2016 <- waiting_times(2016, 4)

most_games <- max(waitingtimes4pen2016$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes4pen2016 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes4pen2016$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes4pen2016$game_count[player]
    randomgames <- sample(1:num_games, 4, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes4pen20162017.png",p,bg="white")

#2016-2017
#players with 5 penalties

waitingtimes5pen2016 <- waiting_times(2016, 5)

most_games <- max(waitingtimes5pen2016$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes5pen2016 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes5pen2016$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA, fifth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes5pen2016$game_count[player]
    randomgames <- sample(1:num_games, 5, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
    randompenalties$fifth_penalty[player] <- randomgames[5]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  
  game_diffs4 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fifth_penalty - fourth_penalty,
              .groups = 'drop')
  
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3, game_diffs4)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes5pen20162017.png",p,bg="white")

#2015-2016
#players with 2 penalties

waitingtimes2pen2015 <- waiting_times(2015, 2)

most_games <- max(waitingtimes2pen2015$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2pen2015 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2pen2015$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2pen2015$game_count[player]
    randomgames <- sample(1:num_games, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  game_diffs <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = abs(first_penalty - second_penalty))
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2pen20152016.png",p,bg="white")

#2015-2016
#players with 3 penalties

waitingtimes3pen2015 <- waiting_times(2015, 3)

most_games <- max(waitingtimes3pen2015$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes3pen2015 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes3pen2015$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes3pen2015$game_count[player]
    randomgames <- sample(1:num_games, 3, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes3pen20152016.png",p,bg="white")

#2015-2016
#players with 4 penalties

waitingtimes4pen2015 <- waiting_times(2015, 4)

most_games <- max(waitingtimes4pen2015$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes4pen2015 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes4pen2015$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes4pen2015$game_count[player]
    randomgames <- sample(1:num_games, 4, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes4pen20152016.png",p,bg="white")

#2015-2016
#players with 5 penalties

waitingtimes5pen2015 <- waiting_times(2015, 5)

most_games <- max(waitingtimes5pen2015$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes5pen2015 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes5pen2015$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA, fifth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes5pen2015$game_count[player]
    randomgames <- sample(1:num_games, 5, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
    randompenalties$fifth_penalty[player] <- randomgames[5]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  
  game_diffs4 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fifth_penalty - fourth_penalty,
              .groups = 'drop')
  
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3, game_diffs4)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes5pen20152016.png",p,bg="white")

#2014-2015
#players with 2 penalties

waitingtimes2pen2014 <- waiting_times(2014, 2)

most_games <- max(waitingtimes2pen2014$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes2pen2014 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes2pen2014$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes2pen2014$game_count[player]
    randomgames <- sample(1:num_games, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  game_diffs <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = abs(first_penalty - second_penalty))
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes2pen20142015.png",p,bg="white")

#2014-2015
#players with 3 penalties

waitingtimes3pen2014 <- waiting_times(2014, 3)

most_games <- max(waitingtimes3pen2014$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes3pen2014 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes3pen2014$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes3pen2014$game_count[player]
    randomgames <- sample(1:num_games, 3, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes3pen20142015.png",p,bg="white")

#2014-2015
#players with 4 penalties

waitingtimes4pen2014 <- waiting_times(2014, 4)

most_games <- max(waitingtimes4pen2014$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes4pen2014 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes4pen2014$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes4pen2014$game_count[player]
    randomgames <- sample(1:num_games, 4, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes4pen20142015.png",p,bg="white")

#2018-2019
#players with 5 penalties

waitingtimes5pen2014 <- waiting_times(2014, 5)

most_games <- max(waitingtimes5pen2014$game_count)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- waitingtimes5pen2014 %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(diff_time = 0:most_games)
waitingtimes <- merge(waitingtimes, counts_obs, by = "diff_time", all.x = TRUE)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(waitingtimes$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } 
}


#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = waitingtimes5pen2014$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA, fifth_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- waitingtimes5pen2014$game_count[player]
    randomgames <- sample(1:num_games, 5, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
    randompenalties$third_penalty[player] <- randomgames[3]
    randompenalties$fourth_penalty[player] <- randomgames[4]
    randompenalties$fifth_penalty[player] <- randomgames[5]
  }
  
  game_diffs1 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = second_penalty - first_penalty,
              .groups = 'drop')
  
  game_diffs2 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = third_penalty - second_penalty,
              .groups = 'drop')
  
  game_diffs3 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fourth_penalty - third_penalty,
              .groups = 'drop')
  
  game_diffs4 <- randompenalties %>%
    group_by(Player) %>%
    summarise(diff_time = fifth_penalty - fourth_penalty,
              .groups = 'drop')
  
  game_diffs <- rbind(game_diffs1, game_diffs2, game_diffs3, game_diffs4)
  
  game_counts <- game_diffs %>%
    group_by(diff_time) %>%
    summarize(count = n())
  
  counts <- data.frame(diff_time = 0:most_games)
  counts <- merge(counts, game_counts, by = "diff_time", all.x = TRUE)
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
  values <- as.numeric(waitingtimes[i, 3:1002])
  quantiles <- quantile(values, probs = c(0.025, 0.975))
  quantile_list[[i]] <- quantiles
}

quantiles_df <- do.call(rbind, quantile_list)
# making a data frame with all of the simulation values and quantiles
waitingtimes_quantiles <- cbind(waitingtimes, quantiles_df)

waitingtimes_quantiles <- waitingtimes_quantiles %>%
  rename(lower_bound = "2.5%",
         upper_bound = "97.5%")

p <- ggplot(data = waitingtimes_quantiles) + 
  geom_point(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = diff_time, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = diff_time, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = diff_time, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

ggsave("results/waitingtimes5pen20142015.png",p,bg="white")