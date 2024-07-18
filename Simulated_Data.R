library(tidyverse)
library(dplyr)
library(ggplot2)

synthetic_data <- read_csv("/Users/catherinelunny/Downloads/synthetic_hockey_data.csv")

# seeing the maximum number of games played by a player for x-axis range
most_games <- max(synthetic_data$num_games)

# dataframe counting the observed counts of each observed waiting time
counts_obs <- synthetic_data %>%
  group_by(diff_time) %>%
  summarize(obs_count = n())

# main dataframe that will have all possible waiting times and counts
waitingtimes <- data.frame(game_diff = 0:82, obs_count = NA)
for (diff in 1:nrow(waitingtimes)) {
  if (is.na(counts_obs$obs_count[diff])) {
    waitingtimes$obs_count[diff] <- 0
  } else {
    waitingtimes$obs_count[diff] <- counts_obs$obs_count[diff]
  }
}
  

#loop picking random games out of the games they played each season to have a penalty in 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = synthetic_data$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    num_games <- synthetic_data$num_games[player]
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
  
  counts <- data.frame(game_diff = 0:82, count = NA)
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(game_counts$count[diff])) {
      counts$count[diff] <- 0
    } else {
      counts$count[diff] <- game_counts$count[diff]
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
  geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
  geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
  geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
  scale_y_continuous(breaks = (0:35)) +
  scale_x_continuous(breaks = seq(0,84, by = 2)) +
  labs(x = "Game difference", y = "Count of players")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

p

