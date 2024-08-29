library(tidyverse)
library(patchwork)

# defining all of the functions and intermediate data

# note: there are 3602 players in the data

# Open data
# contains the play, game, and the player the penalty was on 
player_penalties <- read.csv("data/plays_players.csv")

# all penalties, the play id, penalty severity, and the minutes for the penalty
penalties <- read.csv("data/game_penalties.csv")

# information about all of the players
players <- read.csv("data/player_info.csv")

# all of the plays each player was a part of
plays_players1 <- readRDS("data/plays_players1.rds")

# information about each game
games <- read.csv("data/games.csv")

#filtering the penalties to only include plays that is matched with a player in the player data frames 
filtered_penalties <- penalties %>%
  filter(play_id %in% player_penalties$'play_id')
saveRDS(filtered_penalties,"intermediate_data/filtered_penalties.rds")

# filtering out all of the penalty plays so that this data frame only has non-penalty plays
player_plays_nonpenalty <- anti_join(plays_players1, filtered_penalties, by = "play_id")
player_plays_nonpenalty$penaltySeverity <- NA
player_plays_nonpenalty$penaltyMinutes <- 0
saveRDS(player_plays_nonpenalty, "intermediate_data/player_plays_nonpenalty.rds")

# removing the time from the date column (first 10 characters have the day, month, and year of the game)
games$date_time_GMT <- substr(games$date_time_GMT, 1, 10)
games$date_time_GMT <- as.Date(games$date_time_GMT)
saveRDS(games,"intermediate_data/games.rds")

# games in chronological order
games_ordered <- games %>% arrange(date_time_GMT)
saveRDS(games_ordered,"data/games_ordered.rds")


#Creating a data frame containing all information on penalty plays with the player_ids of the players the penalty was on 
complete_penalty_info <- cbind(player_penalties, filtered_penalties)
complete_penalty_info <- complete_penalty_info[, c("play_id", "game_id", "player_id", "playerType", "penaltySeverity", "penaltyMinutes")]
saveRDS(complete_penalty_info, "intermediate_data/complete_penalty_info.rds")

# bringing the penalty and non-penalty plays together- now all plays have the players involved and game info 
all_player_plays <- rbind(complete_penalty_info, player_plays_nonpenalty)
all_player_plays <- merge(all_player_plays, games_ordered, by = "game_id", all.x = TRUE)
saveRDS(all_player_plays,"intermediate_data/all_player_plays.rds")


# looking at player careers 
n_of_bins <- 10
cut_breaks <- seq(0,1,length.out = n_of_bins+1)
cut_breaks_labels <- sprintf(cut_breaks, fmt = "%.2f")
cut_breaks_labels <- paste(cut_breaks_labels[-length(cut_breaks_labels)],
                           cut_breaks_labels[-1],
                           sep = "-")

# Calculate the grouping of penalties within each player career, how many penalties in each percentile of their career:
careers <- all_player_plays %>% 
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
saveRDS(careers,"intermediate_data/careers.rds")

# shows what percent of players' penalties occurred in each portion of players career 
career_data <- careers %>% 
  group_by(player_id,game_percentile) %>% 
  reframe(penalty_minutes_share = sum(penaltyMinutes) / dplyr::first(total_penalty_minutes)) %>% 
  ungroup() %>% 
  complete(player_id,game_percentile,
           fill = list(penalty_minutes_share = 0),explicit = F) %>% 
  na.omit() %>% 
  ungroup()
saveRDS(career_data,"intermediate_data/career_data.rds")

# displays the average fraction of penalties that occured in each phase of a the players' career
mean_share_table <- career_data %>% 
  group_by(game_percentile) %>% 
  summarise(mean_share_of_total_penalty_minutes = round(mean(penalty_minutes_share),3))
saveRDS(mean_share_table,"intermediate_data/mean_share_table.rds")

# data frame that shows players total game count and total penalty count over their career
player_games <- all_player_plays %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
  )
saveRDS(player_games,"intermediate_data/player_games.rds")

# Shows how many games each player played in
tab_games_by_players <- all_player_plays %>% 
  group_by(player_id) %>% 
  summarise(unique_games = length(unique(game_id)))
saveRDS(tab_games_by_players,"intermediate_data/tab_games_by_players.rds")

# shows how many seasons each player played in
tab_seasons_by_players <- all_player_plays %>%
  group_by(player_id) %>%
  summarise(unique_seasons = length(unique(season)))
saveRDS(tab_seasons_by_players,"intermediate_data/tab_seasons_by_players.rds")

# shows how many penalties each player had
tab_penalties_by_players <- all_player_plays %>%
  group_by(player_id) %>%
  summarise(unique_penalties = n_distinct(unique(play_id[playerType == "PenaltyOn"])))
saveRDS(tab_penalties_by_players,"intermediate_data/tab_penalties_by_players.rds")

# data set that has the number of games a player played in each season and the percent of the total games in their career that accounts for
normalized_player_data <- all_player_plays %>%
  group_by(player_id, season) %>%
  summarise(total_plays = length(unique(play_id))) %>%
  group_by(player_id) %>%
  mutate(normalized_plays = total_plays / sum(total_plays))
saveRDS(normalized_player_data,"intermediate_data/normalized_player_data.rds")

# data frame with the average amount of games played per season for players. divides the number of seasons played in by the total number of penalties
averages <- normalized_player_data %>%
  group_by(player_id) %>%
  summarise(seasons_played = length(unique(season)),
            total_plays = sum(total_plays)) %>%
  mutate(avg_plays_season = total_plays / seasons_played)
saveRDS(averages,"intermediate_data/averages.rds")

# dataset that shows the total number of games and penalties in each player's career
player_game_count <- all_player_plays %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
  )
saveRDS(player_game_count,"intermediate_data/player_game_count.rds")

# makes a data frame of the plays in a specific season - input the year that is the first half of the desired season (for example for the 2019-2020 season input 2019)
season_plays <- function(year) {
  specific_season <- paste0(year, year+1)
  seasondf <- filter(all_player_plays, season == specific_season)
  
  return(seasondf)
}


#variable counting the number of games in the season - input the year that is the first half of the desired season (for example for the 2019-2020 season input 2019)
game_count_season <- function(year) {
  # uses the season_plays function to make the data frame
  game_count <- n_distinct(season_plays(year)$game_id)
  return(game_count)
}


# Data frame that shows the number of games and penalties for players during a specific season. can specify penalty severity minor or major or with all together
# input the year that is the first half of the desired season (for the 2019-2020 season input 2019)
 player_games_season <- function(year, penalty_severity = NULL) {
   # for all penalties together
   if (is.null(penalty_severity)){
     player_games <- season_plays(year) %>%
       # counting by player
       group_by(player_id) %>%
       summarize(game_count = n_distinct(unique(game_id)),
                 penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"])))
     } else if (penalty_severity == "Major") {
      player_games <- season_plays(year) %>%
        group_by(player_id) %>%
        summarize(game_count = n_distinct(unique(game_id)),
                  penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn" & penaltySeverity == penalty_severity])))
     } else if (penalty_severity == "Minor") {
       player_games <- season_plays(year) %>%
         group_by(player_id) %>%
         summarize(game_count = n_distinct(unique(game_id)),
                   penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn" & penaltySeverity == penalty_severity])))
     }
   
   return(player_games)
 }
 

# function that returns the most common frequencies of penalties of the desired type in a season. 
most_common_num_pens <- function(year, penalty_severity = NULL) {
  if (is.null(penalty_severity)){
    most_common <- player_games_season(year) %>%
      group_by(penalty_count) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      # need the penalty count to be > 1 so we can look at the waiting times
      filter(penalty_count > 1)
    numbers <- most_common$penalty_count[1:3]
    return(numbers)
  } else {
    most_common <- player_games_season(year, penalty_severity) %>%
      group_by(penalty_count) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      filter(penalty_count > 1)
    numbers <- most_common$penalty_count[1:3]
    return(numbers)
  }
}


#function that returns a data frame with players with a specific number of penalties in a season by filtering by the number of penalties
fixednumber_penalties_season <- function(year, penalties, penalty_severity = NULL) {
  if (is.null(penalty_severity)){
    playerswith_number_penalties <- filter(player_games_season(year, penalty_severity), penalty_count == penalties)
    return(playerswith_number_penalties)
  } else if (penalty_severity == "Major"){
    playerswith_number_penalties <- filter(player_games_season(year, penalty_severity), penalty_count == penalties)
    return(playerswith_number_penalties)
  } else if (penalty_severity == "Minor"){
    playerswith_number_penalties <- filter(player_games_season(year, penalty_severity), penalty_count == penalties)
    return(playerswith_number_penalties)
  } 
    
}



# function that returns a data frame with player penalty games and the game differences between them
# the three data frames are returned in a list
# enter specific penalty numbers if you want, but if left null the function will use the top 3 most frequent penalty counts from the season_highest_pen_freq function for the year
waiting_times <- function(year, penalty_severity = NULL, penalties1 = NULL, penalties2 = NULL , penalties3 = NULL){
  season_highest_pen_freq <- most_common_num_pens(year, penalty_severity)
  # the season_highest_pen_freq list has the frequencies in order, so the first entry will be the most frequent and so on 
  if (is.null(penalties1)) {
    penalties1 <- season_highest_pen_freq[1]
  }
  if (is.null(penalties2)) {
    penalties2 <- season_highest_pen_freq[2]
  }
  if (is.null(penalties3)) {
    penalties3 <- season_highest_pen_freq[3]
  }
  
  # pulling up all of the play information from the season 
  season <- season_plays(year)
  
  # data frame that has the season game count of all of the players that had x penalties in the season 
  x_pens_season1 <- subset(fixednumber_penalties_season(year, penalties1, penalty_severity), select = -penalty_count)
  
  # extracting the column so that we have all of the players that had x penalties in the season
  players_with_x_pens1 <- fixednumber_penalties_season(year, penalties1, penalty_severity)$player_id
  
  # full details on the games and plays for each player that had x penalties in the season 
  x_pen_playergames1 <- filter(season, player_id %in% players_with_x_pens1)
  
  # each row occurs twice in the data frame, this way it only appears once
  noduplicates1 <- distinct(x_pen_playergames1, play_id, .keep_all = TRUE)

  # ranking all of the season games for the players
  ranking_games1 <- noduplicates1 %>%
    group_by(player_id) %>%
    summarise(game_order = dense_rank(date_time_GMT),
              # keeping the playerType column so that we can filter out the penalty games
              playerType = playerType,
              penaltySeverity = penaltySeverity)
  
  # data frame that just has players and the ranking of their penalty games
  penalties_only1 <- filter(ranking_games1, playerType == "PenaltyOn", penaltySeverity == penalty_severity)
  
  # the same thing but for the second penalty number
  x_pens_season2 <- subset(fixednumber_penalties_season(year, penalties2, penalty_severity), select = -penalty_count)
  players_with_x_pens2 <- fixednumber_penalties_season(year, penalties2, penalty_severity)$player_id
  
  x_pen_playergames2 <- filter(season, player_id %in% players_with_x_pens2)
  
  noduplicates2 <- distinct(x_pen_playergames2, play_id, .keep_all = TRUE)
  
  ranking_games2 <- noduplicates2 %>%
    group_by(player_id) %>%
    summarise(game_order = dense_rank(date_time_GMT),
              playerType = playerType,
              penaltySeverity = penaltySeverity)
  
  penalties_only2 <- filter(ranking_games2, playerType == "PenaltyOn", penaltySeverity == penalty_severity)
  
  # the same thing but for the third penalty number
  x_pens_season3 <- subset(fixednumber_penalties_season(year, penalties3, penalty_severity), select = -penalty_count)
  players_with_x_pens3 <- fixednumber_penalties_season(year, penalties3, penalty_severity)$player_id
  
  x_pen_playergames3 <- filter(season, player_id %in% players_with_x_pens3)
  
  noduplicates3 <- distinct(x_pen_playergames3, play_id, .keep_all = TRUE)
  
  ranking_games3 <- noduplicates3 %>%
    group_by(player_id) %>%
    summarise(game_order = dense_rank(date_time_GMT),
              playerType = playerType,
              penaltySeverity = penaltySeverity)
  
  penalties_only3 <- filter(ranking_games3, playerType == "PenaltyOn", penaltySeverity == penalty_severity)
  
  # main list where all of the waiting times data frames will be stored in
  waitingtimes_list <- list()
  
  #first df
  # list to store each player's waiting times
  waitingtimes1_list <- list()
  for (player in penalties_only1$player_id) {
    # data frame that has an individual player's penalty games ranked
    player_diffs <- filter(penalties_only1, player_id == player)
    # empty data frame for the waiting times of each player. player_id column is the player being looked at in each loop, and one less row than game_diffs because if there are x penalties there are x-1 waiting times
    waitingtimes1 <- data.frame(player_id = player_diffs$player_id[1:(nrow(player_diffs)-1)], penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
    # goes through each penalty game and finds the difference in games between it and the next one
    for (game in 1:(nrow(player_diffs) - 1)){
      penalty_game <- player_diffs$game_order[game]
      waitingtimes1$penalty_game[game] <- penalty_game
      
      next_penalty_game <- player_diffs$game_order[game + 1]
      waitingtimes1$next_penalty_game[game] <- next_penalty_game
      
      game_diff <- next_penalty_game - penalty_game
      waitingtimes1$game_diff[game] <- game_diff
      
      # adding the game count column to have for drawing random penalty games for the null distributions for the plots in 04_waiting_times
      game_count <- x_pens_season1$game_count[x_pens_season1$player_id == player]
      waitingtimes1$game_count[game] <- game_count
    }
    # adds each players data frame to the list created above
    waitingtimes1_list[[player]] <- waitingtimes1
  }
  
# combines all of the players waiting times into one data frame
 waitingtimes1 <- do.call(rbind, waitingtimes1_list)
 
 # adds the waiting times for players with the most frequent number of penalty occurences to the list
 waitingtimes_list[[1]] <- waitingtimes1
 
 # same exact thing for the second and third data frames
 # second df
 waitingtimes2_list <- list()
 for (player in penalties_only2$player_id) {
   player_diffs <- filter(penalties_only2, player_id == player)
   waitingtimes2 <- data.frame(player_id = player_diffs$player_id[1:(nrow(player_diffs)-1)], penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
   for (game in 1:(nrow(player_diffs) - 1)){
     penalty_game <- player_diffs$game_order[game]
     waitingtimes2$penalty_game[game] <- penalty_game
     
     next_penalty_game <- player_diffs$game_order[game + 1]
     waitingtimes2$next_penalty_game[game] <- next_penalty_game
     
     game_diff <- next_penalty_game - penalty_game
     waitingtimes2$game_diff[game] <- game_diff
     
     game_count <- x_pens_season2$game_count[x_pens_season2$player_id == player]
     waitingtimes2$game_count[game] <- game_count
   }
   waitingtimes2_list[[player]] <- waitingtimes2
 }
 
 waitingtimes2 <- do.call(rbind, waitingtimes2_list)
 waitingtimes_list[[2]] <- waitingtimes2
 
 #third df
 waitingtimes3_list <- list()
 #for (i in 1:n_distinct(penalties_only1$player_id)) {
 for (player in penalties_only3$player_id) {
   player_diffs <- filter(penalties_only3, player_id == player)
   waitingtimes3 <- data.frame(player_id = player_diffs$player_id[1:(nrow(player_diffs)-1)], penalty_game = NA, next_penalty_game = NA, game_diff = NA, game_count = NA)
   for (game in 1:(nrow(player_diffs) - 1)){
     penalty_game <- player_diffs$game_order[game]
     waitingtimes3$penalty_game[game] <- penalty_game
     
     next_penalty_game <- player_diffs$game_order[game + 1]
     waitingtimes3$next_penalty_game[game] <- next_penalty_game
     
     game_diff <- next_penalty_game - penalty_game
     waitingtimes3$game_diff[game] <- game_diff
     
     game_count <- x_pens_season3$game_count[x_pens_season3$player_id == player]
     waitingtimes3$game_count[game] <- game_count
   }
   waitingtimes3_list[[player]] <- waitingtimes3
 }
 
 waitingtimes3 <- do.call(rbind, waitingtimes3_list)
 waitingtimes_list[[3]] <- waitingtimes3
 
 return(waitingtimes_list)
 
}



# old waiting times function that did not use a loop and only worked for specfic numbers of penalty frequencies, disregard

  # player_penalty_games <- penalties_only1 %>%
  #   group_by(player_id) %>%
  #   for (diff in 1:nrow(player_penalty_games){
  #     summarise(first_penalty = game_order[1],
  #             second_penalty = game_order[2],
  #             diff_time = game_order[2] - game_order[1])
  # waiting_times <- merge(player_penalty_games, x_pens_season)
  # return(waiting_times)
#   
#   if (penalties == 2){
#     player_penalty_games <- penalties_only %>%
#     group_by(player_id) %>%
#     summarise(first_penalty = game_order[1],
#               second_penalty = game_order[2],
#               diff_time = game_order[2] - game_order[1])
#     waiting_times <- merge(player_penalty_games, x_pens_season)
#     return(waiting_times)
#   } else if (penalties == 3){
#     player_penalty_games1 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 diff_time = game_order[2] - game_order[1])
#     player_penalty_games2 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 diff_time = game_order[3] - game_order[2])
#     player_penalty_games <- rbind(player_penalty_games1, player_penalty_games2)
#     waiting_times <- merge(player_penalty_games, x_pens_season)
#     return(waiting_times)
#   } else if (penalties == 4){
#     player_penalty_games1 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4], 
#                 diff_time = game_order[2] - game_order[1])
#     player_penalty_games2 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4],
#                 diff_time = game_order[3] - game_order[2])
#     player_penalty_games3 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4],
#                 diff_time = game_order[4] - game_order[3])
#     player_penalty_games <- rbind(player_penalty_games1, player_penalty_games2, player_penalty_games3)
#     waiting_times <- merge(player_penalty_games, x_pens_season)
#     return(waiting_times)
#   } else if (penalties == 5){
#     player_penalty_games1 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4], 
#                 fifth_penalty = game_order[5],
#                 diff_time = game_order[2] - game_order[1])
#     player_penalty_games2 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4],
#                 fifth_penalty = game_order[5],
#                 diff_time = game_order[3] - game_order[2])
#     player_penalty_games3 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4],
#                 fifth_penalty = game_order[5],
#                 diff_time = game_order[4] - game_order[3])
#     player_penalty_games4 <- penalties_only %>%
#       group_by(player_id) %>%
#       summarise(first_penalty = game_order[1],
#                 second_penalty = game_order[2],
#                 third_penalty = game_order[3],
#                 fourth_penalty = game_order[4],
#                 fifth_penalty = game_order[5],
#                 diff_time = game_order[5] - game_order[4])
#     player_penalty_games <- rbind(player_penalty_games1, player_penalty_games2, player_penalty_games3, player_penalty_games4)
#     waiting_times <- merge(player_penalty_games, x_pens_season)
#     return(waiting_times)
#   } 
# }



  # waiting_times <- ranking_games %>%
  #   group_by(player_id) %>%
  #   summarise(num_games = max(game_order)) %>%
  #   filter(playerType == "PenaltyOn") %>%
  #   summarise(first_penalty = penalties_only[1, 2],
  #             second_penalty = penalties_only[2, 2],
  #             diff_time = second_penalty - first_penalty)
  
  # return(penalties_only)



# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
# twopenalties20192020 <- filter(season20192020, player_id %in% fixedwindow2pen_20192020$player_id)
# twopenalties20192020 <- distinct(twopenalties20192020, play_id, .keep_all = TRUE)
# twopenalties20192020 <- twopenalties20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_order = dense_rank(date_time_GMT),
#             playerType = playerType)
# twopenalties20192020 <- filter(twopenalties20192020, playerType == "PenaltyOn")
# saveRDS(twopenalties20192020,"intermediate_data/twopenalties20192020.rds")
# 
# # data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
# twopenaltieswaitingtimes20192020 <- twopenalties20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_difference = diff(game_order))



# twopenaltieswaitingtimes20192020$distribution <- 1/nrow(twopenaltieswaitingtimes20192020)
# saveRDS(twopenaltieswaitingtimes20192020,"intermediate_data/twopenaltieswaitingtimes20192020.rds")


# saveRDS(fixedwindow3pen_20192020,"intermediate_data/fixedwindow3pen_20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
# threepenalties20192020 <- filter(season20192020, player_id %in% fixedwindow3pen_20192020$player_id)
# threepenalties20192020 <- distinct(threepenalties20192020, play_id, .keep_all = TRUE)
# threepenalties20192020 <- threepenalties20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_order = dense_rank(date_time_GMT),
#             playerType = playerType)
# threepenalties20192020 <- filter(threepenalties20192020, playerType == "PenaltyOn")
# 
# saveRDS(threepenalties20192020,"intermediate_data/threepenalties20192020.rds")
# 
# # data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
# threepenwaitingtimes20192020 <- threepenalties20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_difference = diff(game_order))
# 
# #applying formula (10) from the paper to determine the distribution
# threepenwaitingtimes20192020$distribution <- 1/nrow(threepenwaitingtimes20192020)
# saveRDS(threepenwaitingtimes20192020,"intermediate_data/threepenwaitingtimes20192020.rds")
# 
# 
# 
# #the same analysis, but now for 4 penalties. isolating the players who had 4 penalties in the 2019-2020 season
# fixedwindow4pen_20192020 <- filter(player_games20192020, penalty_count == 4)
# saveRDS(fixedwindow4pen_20192020,"intermediate_data/fixedwindow4pen_20192020.rds")
# 
# # ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
# fourpenalties20192020 <- filter(season20192020, player_id %in% fixedwindow4pen_20192020$player_id)
# fourpenalties20192020 <- distinct(fourpenalties20192020, play_id, .keep_all = TRUE)
# fourpenalties20192020 <- fourpenalties20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_order = dense_rank(date_time_GMT),
#             playerType = playerType)
# fourpenalties20192020 <- filter(fourpenalties20192020, playerType == "PenaltyOn")
# saveRDS(fourpenalties20192020,"intermediate_data/fourpenalties20192020.rds")
# 
# # data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
# fourpenwaitingtimes20192020 <- fourpenalties20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_difference = diff(game_order))
# 
# #applying formula (10) from the paper to determine the distribution
# fourpenwaitingtimes20192020$distribution <- 1/nrow(fourpenwaitingtimes20192020)
# saveRDS(fourpenwaitingtimes20192020,"intermediate_data/fourpenwaitingtimes20192020.rds")
# 
# #CREATING DATAFRAMES FOR THE SAME ANALYSIS, BUT WITH MAJOR PENALTIES
# #data frame showing the number of major penalties each offending player had in the 2019-2020 season 
# majorpenalties20192020 <- season20192020 %>%
#   group_by(player_id) %>%
#   summarize(game_count = n_distinct(unique(game_id)),
#             major_penalty_count = n_distinct(unique(play_id[penaltySeverity == "Major"]))
#   )
# saveRDS(majorpenalties20192020,"intermediate_data/majorpenalties20192020.rds")
# 
# two_major_players20192020 <- filter(majorpenalties20192020, major_penalty_count == 2)
# saveRDS(two_major_players20192020,"intermediate_data/two_major_players20192020.rds")
# 
# # ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
# two_majors20192020 <- filter(season20192020, player_id %in% two_major_players20192020$player_id)
# two_majors20192020 <- distinct(two_majors20192020, play_id, .keep_all = TRUE)
# two_majors20192020 <- two_majors20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_order = dense_rank(date_time_GMT),
#             penaltySeverity = penaltySeverity)
# two_majors20192020 <- filter(two_majors20192020, penaltySeverity == "Major")
# saveRDS(two_majors20192020,"intermediate_data/two_majors20192020.rds")
# 
# # data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
# twomajor_waitingtimes20192020 <- two_majors20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_difference = diff(game_order))
# 
# twomajor_waitingtimes20192020$distribution <- 1/nrow(twomajor_waitingtimes20192020)
# saveRDS(twomajor_waitingtimes20192020,"intermediate_data/twomajor_waitingtimes20192020.rds")
# 
# 
# #the same analysis, but now for 3 penalties. isolating the players who had 3 penalties in the 2019-2020 season
# three_major_players20192020 <- filter(majorpenalties20192020, major_penalty_count == 3)
# saveRDS(three_major_players20192020,"intermediate_data/three_major_players20192020.rds")
# 
# # ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
# three_majors20192020 <- filter(season20192020, player_id %in% three_major_players20192020$player_id)
# three_majors20192020 <- distinct(three_majors20192020, play_id, .keep_all = TRUE)
# three_majors20192020 <- three_majors20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_order = dense_rank(date_time_GMT),
#             penaltySeverity = penaltySeverity)
# three_majors20192020 <- filter(three_majors20192020, penaltySeverity == "Major")
# 
# saveRDS(three_majors20192020,"intermediate_data/three_majors20192020.rds")
# 
# # data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
# threemajor_waitingtimes20192020 <- three_majors20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_difference = diff(game_order))
# 
# threemajor_waitingtimes20192020$distribution <- 1/nrow(threemajor_waitingtimes20192020)
# saveRDS(threemajor_waitingtimes20192020,"intermediate_data/threemajor_waitingtimes20192020.rds")
# 
# 
# 

# 
# # data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
# fourmajor_waitingtimes20192020 <- four_majors20192020 %>%
#   group_by(player_id) %>%
#   summarise(game_difference = diff(game_order))
# 
# #applying formula (10) from the paper to determine the distribution
# fourmajor_waitingtimes20192020$distribution <- 1/nrow(fourmajor_waitingtimes20192020)
# saveRDS(fourmajor_waitingtimes20192020,"intermediate_data/fourmajor_waitingtimes20192020.rds")

# # probability distribution of waiting times
# waitingtimes20192020_dist <- data.frame(prop.table(table(waitingtimes20192020$game_difference)))
# names(waitingtimes20192020_proportions)[names(waitingtimes20192020_proportions) == "Var1"] <- "game_difference"
# names(waitingtimes20192020_proportions)[names(waitingtimes20192020_proportions) == "Freq"] <- "percentage"
# saveRDS(waitingtimes20192020_proportions,"intermediate_data/waitingtimes20192020_proportions.rds")






