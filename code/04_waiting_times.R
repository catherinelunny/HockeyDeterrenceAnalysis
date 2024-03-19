library(tidyverse)
library(patchwork)

season20192020 <- readRDS("intermediate_data/season20192020.rds")
waitingtimes20192020 <- readRDS("intermediate_data/waitingtimes20192020.rds")
game_count2019 <- n_distinct(season20192020$game_id)
penaltygames20192020 <- readRDS("intermediate_data/penaltygames20192020.rds")
fixedwindow_20192020 <- readRDS("intermediate_data/fixedwindow_20192020.rds")
fixedwindow3pen_20192020 <- readRDS("intermediate_data/fixedwindow3pen_20192020.rds") 
fixedwindow4pen_20192020 <- readRDS("intermediate_data/fixedwindow4pen_20192020.rds")
fourpenwaitingtimes20192020 <- readRDS("intermediate_data/fourpenwaitingtimes20192020.rds")
threepenwaitingtimes20192020 <- readRDS("intermediate_data/threepenwaitingtimes20192020.rds")

#creating the null distribution line for the plot
nulldist <- data.frame(c(0:31))
names(nulldist)[names(nulldist) == "c.0.31."] <- "x"
nulldist$y <- (2 * (game_count2019 - nulldist$x)) / (game_count2019 * (game_count2019 + 1))

#graph of time between penalties vs. probability
waitingtimes20192020plot <- ggplot() +
  geom_histogram(data = waitingtimes20192020, aes(x = game_difference,
                                                  y = distribution), stat = "identity", fill = "black", alpha = 1.0, width = 2) +
  geom_line(data = nulldist, aes(x = x, y = y)) +
  labs(x = "\u03C4 (Games)", y = "p(\u03C4)")

waitingtimes20192020plot

ggsave("results/2019twopenaltiesdistribution.png",waitingtimes20192020plot)

#a list for storing the randomly generated waiting times
waitingtimes_list <- list()

#loop picking random games out of the games they played each season to have a penalty in. Used based on the game count and picks 2 random games based on order and stores them in the list. 
for (i in 1:1000) {
  randompenalties <- data.frame(Player = fixedwindow_20192020$player_id, first_penalty = NA, second_penalty = NA)
  for (player in 1:nrow(randompenalties)) {
    game_count <- fixedwindow_20192020$game_count[player]
    randomgames <- sample(1:game_count, 2, replace = TRUE)
    randompenalties$first_penalty[player] <- randomgames[1]
    randompenalties$second_penalty[player] <- randomgames[2]
  }
  waitingtimes <- randompenalties %>%
  group_by(Player) %>%
  summarise(game_difference = abs(first_penalty - second_penalty))
  waitingtimes$distribution <- (2*(game_count2019 - waitingtimes$game_difference))/(game_count2019*(game_count2019 + 1))
  
  waitingtimes <- waitingtimes %>%
    group_by(game_difference) %>%
    summarise(cumDist = sum(distribution))
  
  waitingtimes_list[[i]] <- waitingtimes
}

#combining all players random wait times
all_waitingtimes <- do.call(rbind, waitingtimes_list)

# finding the quantiles for the confidence intervals
quantiles_by_group <- all_waitingtimes %>%
  group_by(game_difference) %>%
  summarise(quantile_2.5 = quantile(cumDist, 0.025),
            quantile_97.5 = quantile(cumDist, 0.975))

#graph that includes the observed distributions and the 2.5 percentile and 97.5 percentile of the random distributions
everything2pen <- waitingtimes20192020plot +
  geom_histogram(data = quantiles_by_group, aes(x = game_difference, y = quantile_2.5), stat = "identity", fill = "red", alpha = 0.5) +
  geom_histogram(data = quantiles_by_group, aes(x = game_difference, y = quantile_97.5), stat = "identity", fill = "blue", alpha = 0.5) 

# AK: rework histogram
waitingtimes_2019_2020_count <- waitingtimes20192020 %>% 
  #mutate(distr_binned = cut(distribution,
  #                          breaks = unique(quantile(distribution, probs = seq.int(0,1, by = 1 / max(game_difference)))), 
  #                          include.lowest = TRUE)) %>% 
  #mutate(distr_round = round(distribution,5)) %>% 
  group_by(game_difference) %>% 
  summarize(n = n()) %>% 
  arrange(game_difference) %>% 
  tidyr::complete(game_difference = min(game_difference):max(game_difference)) %>% 
  mutate(n = ifelse(is.na(n),0,n)) %>% 
  left_join(., nulldist, by = c("game_difference" = "x")) %>% 
  rename(distribution = y)
  

p <- ggplot(data = waitingtimes_2019_2020_count,
       aes(x = game_difference,
                                        y = n,
                                        group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0,NA)) +
  scale_x_continuous(breaks = 1:31) +
  labs(x = "Game difference", y = "Count of games")+
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

ggsave("results/ak_waiting_times_2019_2020_count.png",p,bg="white")

# AK: testing if within CI:
waitingtimes_2019_2020_count_ci <- left_join(waitingtimes_2019_2020_count,
                                             quantiles_by_group) %>% 
  mutate(is_within_ci = ifelse(distribution < quantile_2.5 & distribution < quantile_97.5,
                               1,
                               0))

head(waitingtimes_2019_2020_count_ci,10)

# 3 Penalties
#creating the null distribution line for the plot
nulldist3 <- data.frame(c(0:43))
names(nulldist3)[names(nulldist3) == "c.0.43."] <- "x"
nulldist3$y <- mapply(
  function(game_diff) {
    numerator <- prod((game_count2019 - game_diff + 0:1) / (game_count2019 + 0:1))
    return(3 / (game_count2019 + 2) * numerator)
  },
  nulldist3$x
)

#graph of time between penalties vs. probability (time between first and second, and then second and third)
waitingtimes201920203penplot <- ggplot() +
  geom_histogram(data = threepenwaitingtimes20192020, aes(x = game_difference, y = distribution), stat = "identity", alpha = 1.0, width = 2) +
  geom_line(data = nulldist3, aes(x = x, y = y), fill = "blue") +
  labs(x = "\u03C4 (Games)", y = "p(\u03C4)")

waitingtimes201920203penplot

ggsave("results/2019threepenaltiesdistribution.png",waitingtimes201920203penplot)

#a list for storing the randomly generated waiting times
waitingtimes3_list <- list()

#loop picking random games out of the games they played each season to have a penalty in. Used based on the game count and picks 2 random games based on order and stores them in the list. 
for (i in 1:1000) {
  randompenalties3 <- data.frame(Player = fixedwindow3pen_20192020$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA)
  for (player in 1:nrow(randompenalties3)) {
    game_count <- fixedwindow3pen_20192020$game_count[player]
    randomgames <- sort(sample(1:game_count, 3, replace = TRUE))
    randompenalties3$first_penalty[player] <- randomgames[1]
    randompenalties3$second_penalty[player] <- randomgames[2]
    randompenalties3$third_penalty[player] <- randomgames[3]
  }
  #determining the amount of time between consecutive penalties 
  waitingtimes3_1 <- randompenalties3 %>%
    group_by(Player) %>%
    summarise(game_difference = abs(first_penalty - second_penalty))
  waitingtimes3_2 <- randompenalties3 %>%
    group_by(Player) %>%
    summarise(game_difference = abs(second_penalty - third_penalty))
  waitingtimes3 <- bind_rows(waitingtimes3_1, waitingtimes3_2)
  waitingtimes3$distribution <- mapply(
    function(game_diff) {
      numerator <- prod((game_count2019 - game_diff + 0:1) / (game_count2019 + 0:1))
      return(3 / (game_count2019 + 2) * numerator)
    },
    waitingtimes3$game_difference
  )
  
  #determining the observed prbability for each waiting time
  waitingtimes3 <- waitingtimes3 %>%
    group_by(game_difference) %>%
    summarise(cumDist = sum(distribution))
  
  waitingtimes3_list[[i]] <- waitingtimes3
}

all_waitingtimes3 <- do.call(rbind, waitingtimes3_list)

# determining the 2.5 and 97.5 percentiles for the confidence interval
quantiles_by_group3 <- all_waitingtimes3 %>%
  group_by(game_difference) %>%
  summarise(quantile_2.5 = quantile(cumDist, 0.025),
            quantile_97.5 = quantile(cumDist, 0.975))

#waiting times plot with confidence interval
everything3pen <- waitingtimes201920203penplot +
  geom_histogram(data = quantiles_by_group3, aes(x = game_difference, y = quantile_2.5), stat = "identity", fill = "red", alpha = 0.5) +
  geom_histogram(data = quantiles_by_group3, aes(x = game_difference, y = quantile_97.5), stat = "identity", fill = "blue", alpha = 0.5) 

everything3pen
ggsave("results/2019threepenaltiesdistribution_CI.png",everything3pen)

# 4 Penalties
#creating the null distribution line for the plot
nulldist4 <- data.frame(c(0:36))
names(nulldist4)[names(nulldist4) == "c.0.36."] <- "x"
nulldist4$y <- mapply(
  function(game_diff) {
    numerator <- prod((game_count2019 - game_diff + 0:2) / (game_count2019 + 0:2))
    return(4 / (game_count2019 + 3) * numerator)
  },
  nulldist4$x
)

#graph of time between penalties vs. probability (time between first and second, then second and third, then third and fourth)
waitingtimes201920204penplot <- ggplot() +
  geom_histogram(data = fourpenwaitingtimes20192020, aes(x = game_difference, y = distribution), stat = "identity", fill = "black", alpha = 1.0, width = 0.5) +
  geom_line(data = nulldist4, aes(x = x, y = y)) +
  labs(x = "\u03C4 (Games)", y = "p(\u03C4)")

waitingtimes201920204penplot

ggsave("results/2019fourpenaltiesdistribution.png",waitingtimes201920204penplot)

#a list for storing the randomly generated waiting times
waitingtimes4_list <- list()

#loop picking random games out of the games they played each season to have a penalty in. Used based on the game count and picks 2 random games based on order and stores them in the list. 
for (i in 1:1000) {
  randompenalties4 <- data.frame(Player = fixedwindow4pen_20192020$player_id, first_penalty = NA, second_penalty = NA, third_penalty = NA, fourth_penalty = NA)
  for (player in 1:nrow(randompenalties4)) {
    game_count <- fixedwindow4pen_20192020$game_count[player]
    randomgames <- sort(sample(1:game_count, 4, replace = TRUE))
    randompenalties4$first_penalty[player] <- randomgames[1]
    randompenalties4$second_penalty[player] <- randomgames[2]
    randompenalties4$third_penalty[player] <- randomgames[3]
    randompenalties4$fourth_penalty[player] <- randomgames[4]
  }
  #determining the waiting times between consecutive penalties
  waitingtimes4_1 <- randompenalties4 %>%
    group_by(Player) %>%
    summarise(game_difference = abs(first_penalty - second_penalty))
  waitingtimes4_2 <- randompenalties4 %>%
    group_by(Player) %>%
    summarise(game_difference = abs(second_penalty - third_penalty))
  waitingtimes4_3 <- randompenalties4 %>%
    group_by(Player) %>%
    summarise(game_difference = abs(third_penalty - fourth_penalty))
  waitingtimes4 <- bind_rows(waitingtimes4_1, waitingtimes4_2, waitingtimes4_3)            
  waitingtimes4$distribution <- mapply(
    function(game_diff) {
      numerator <- prod((game_count2019 - game_diff + 0:2) / (game_count2019 + 0:2))
      return(4 / (game_count2019 + 3) * numerator)
    },
    waitingtimes4$game_difference
  )
  #determining the observed prbability for each waiting time
  waitingtimes4 <- waitingtimes4 %>%
    group_by(game_difference) %>%
    summarise(cumDist = sum(distribution))
  
  waitingtimes4_list[[i]] <- waitingtimes4
}

all_waitingtimes4 <- do.call(rbind, waitingtimes4_list)


#determining the confidence interval
quantiles_by_group4 <- all_waitingtimes4 %>%
  group_by(game_difference) %>%
  summarise(quantile_2.5 = quantile(cumDist, 0.025),
            quantile_97.5 = quantile(cumDist, 0.975))

everything4pen <- waitingtimes201920204penplot +
  geom_histogram(data = quantiles_by_group4, aes(x = game_difference, y = quantile_2.5), stat = "identity", fill = "red", alpha = 0.5) +
  geom_histogram(data = quantiles_by_group4, aes(x = game_difference, y = quantile_97.5), stat = "identity", fill = "blue", alpha = 0.5) 

everything4pen
ggsave("results/2019fourpenaltiesdistribution_CI.png",everything4pen)
