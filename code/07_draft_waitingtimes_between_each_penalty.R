library(tidyverse)
library(ggplot2)
library(patchwork)

#2018-2019

# This is the code that returns all of the waiting times on one plot, counting each player twice and not accounting for the fact
# that waiting times between first and second penalty could be different from waiting times between later consecutive penalties

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
for (i in 1:10) {
  randomwaitingtimes1_list <- list()
  for (p in 1:n_distinct(waitingtimes20182019_1$player_id)) {
    for (player in waitingtimes20182019_1$player_id) {
      num_games <- waitingtimes20182019_1$game_count[waitingtimes20182019_1$player_id == player]
      num_penalties <- sum(waitingtimes20182019_1$player_id == player) + 1
      randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
      randomgames_ordered <- sort(randomgames)
      player_diffs <- filter(waitingtimes20182019_1, player_id == player)
      
      randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
      for (game in 1:(num_penalties - 1)){
        penalty_game <- randomgames[game]
        randomwaitingtimes$penalty_game[game] <- penalty_game
        
        next_penalty_game <- randomgames[game + 1]
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
  values <- as.numeric(waitingtimes[i, 3:12])
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

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# This is the code for the analysis that makes separate plots for the waiting times between consecutive penalties 
# and makes it possible to compare the differences between waiting times for earlier and later consecutive penalties


#waiting times for the top 3 most frequent amount of penalties for players to have
waitingtimes20182019 <- waiting_times(2018)
# pulling the most frequent amount of penalties
waitingtimes20182019_1 <- as.data.frame(waitingtimes20182019[1])

# pulling the highest amount of games for a player
most_games <- max(waitingtimes20182019_1$game_count)

# determining the number of penalties the players in the given data frame had in a season 
for (player in waitingtimes20182019_1$player_id) {
  num_penalties <- sum(waitingtimes20182019_1$player_id == player) + 1
  break
}

# lists for storing the results from each iteration
waitingtimes_list <- list()
waitingtimesplot_list <- list()

# looping through each penalty
for (penalty in 1:(num_penalties - 1)) {
  # making data frames for 1st, 2nd, 3rd, etc. penalties for each of the players
  xth_penalty <- waitingtimes20182019_1 %>%
    group_by(player_id) %>%
    slice(penalty) %>%
    ungroup()
  
  # dataframe counting the observed counts of each observed waiting time for the xth penalty that occurred for each of the players
  counts_obs <- xth_penalty %>%
    group_by(game_diff) %>%
    summarize(obs_count = n())
  
  # main dataframe that will have all possible waiting times and counts. putting the observed counts in first 
  waitingtimes <- data.frame(game_diff = 0:most_games)
  # merging this empty data frame with the counts data frame
  waitingtimes <- merge(waitingtimes, counts_obs, by = "game_diff", all.x = TRUE)
  
  # for game differences that do not occur in the observed data, so that those values do not have NA but instead get 0 to represent 0 occurrences of that game difference
  for (diff in 1:nrow(waitingtimes)) {
    if (is.na(waitingtimes$obs_count[diff])) {
      waitingtimes$obs_count[diff] <- 0
    } 
  }
  #loop picking random games out of the games they played each season to have a penalty in 
  randomwaitingtimes_list <- list()
  for (i in 1:10) {
    randomwaitingtimes1_list <- list()
      # going through each player
      for (player in waitingtimes20182019_1$player_id) {
        num_games <- waitingtimes20182019_1$game_count[waitingtimes20182019_1$player_id == player]
        # choosing random penalty games
        randomgames <- sample(1:num_games, num_penalties, replace = TRUE)
        randomgames_ordered <- sort(randomgames)
        player_diffs <- filter(waitingtimes20182019_1, player_id == player)
        
        # making random waiting times data frame for each player
        randomwaitingtimes <- data.frame(player_id = player_diffs$player_id, penalty_game = NA, next_penalty_game = NA, game_diff = NA)
        for (game in 1:(length(randomgames_ordered) - 1)){
          penalty_game <- randomgames_ordered[game]
          randomwaitingtimes$penalty_game[game] <- penalty_game
          
          next_penalty_game <- randomgames_ordered[game + 1]
          randomwaitingtimes$next_penalty_game[game] <- next_penalty_game
          
          game_diff <- next_penalty_game - penalty_game
          randomwaitingtimes$game_diff[game] <- game_diff
        }
        # adding it to the list 
        randomwaitingtimes1_list[[player]] <- randomwaitingtimes
      }

    
    # combining all of the players random waiting times for this iteration into a data frame
    randomwaitingtimes <- do.call(rbind, randomwaitingtimes1_list)
    randomwaitingtimes_list[[i]] <- as.data.frame(randomwaitingtimes)
    xth_penalty <- randomwaitingtimes_list[[i]] %>%
      group_by(player_id) %>%
      slice(penalty) %>%
      ungroup()
    game_counts <- xth_penalty %>%
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
  waitingtimes_list[[penalty]] <- waitingtimes
  
  # loop for getting the upper and lower bounds
  quantile_list <- list()
  
  # pulling the values from each row (which are the counts for each waiting time from each iteration)
  for (i in 1:nrow(waitingtimes)) {
    values <- as.numeric(waitingtimes[i, 3:12])
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
  
  plot <- ggplot(data = waitingtimes_quantiles) + 
    geom_point(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
    geom_line(mapping = aes(x = game_diff, y = obs_count, group=1, color = "Observed")) +
    geom_point(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
    geom_line(mapping = aes(x = game_diff, y = lower_bound, group=1, color = "Lower Bound")) +
    geom_point(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
    geom_line(mapping = aes(x = game_diff, y = upper_bound, group=1, color = "Upper Bound")) +
    scale_y_continuous(breaks = (0:100)) +
    scale_x_continuous(breaks = seq(0, most_games, by = 2)) +
    labs(title = paste0("Penalty ", penalty, " to Penalty ", (penalty +1)), x = "Game difference", y = "Count of players")+
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  waitingtimesplot_list[[penalty]] <- plot

} #pen loop

waitingtimesplot <- wrap_plots(waitingtimesplot_list) + plot_annotation(title = "Waiting Times Between Each Penalty for Players with ", num_penalties, " Penalties in the 2018-2019 Season")
  
  

