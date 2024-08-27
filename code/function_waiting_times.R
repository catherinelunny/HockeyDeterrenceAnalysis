# function that returns a data frame with player penalty games and the game differences between them
# the three data frames are returned in a list
# enter specific penalty numbers if you want, but if left null the function will use the top 3 most frequent penalty counts from the season_highest_pen_freq function for the year

waiting_times <- function (year, penalty_severity = NULL, penalties1 = NULL, 
          penalties2 = NULL, penalties3 = NULL) 
{
  season_highest_pen_freq <- most_common_num_pens(year, penalty_severity)
  if (is.null(penalties1)) {
    penalties1 <- season_highest_pen_freq[1]
  }
  if (is.null(penalties2)) {
    penalties2 <- season_highest_pen_freq[2]
  }
  if (is.null(penalties3)) {
    penalties3 <- season_highest_pen_freq[3]
  }
  season <- season_plays(year)
  if (is.null(penalty_severity)) {
    x_pens_season1 <- subset(fixednumber_penalties_season(year, 
                                                          penalties1, penalty_severity), select = -penalty_count)
    players_with_x_pens1 <- fixednumber_penalties_season(year, 
                                                         penalties1, penalty_severity)$player_id
    x_pen_playergames1 <- filter(season, player_id %in% players_with_x_pens1)
    noduplicates1 <- distinct(x_pen_playergames1, play_id, .keep_all = TRUE)
    ranking_games1 <- noduplicates1 %>% group_by(player_id) %>% 
      summarise(game_order = dense_rank(date_time_GMT), playerType = playerType)
    penalties_only1 <- filter(ranking_games1, playerType == 
                                "PenaltyOn")
    x_pens_season2 <- subset(fixednumber_penalties_season(year, 
                                                          penalties2, penalty_severity), select = -penalty_count)
    players_with_x_pens2 <- fixednumber_penalties_season(year, 
                                                         penalties2, penalty_severity)$player_id
    x_pen_playergames2 <- filter(season, player_id %in% players_with_x_pens2)
    noduplicates2 <- distinct(x_pen_playergames2, play_id, .keep_all = TRUE)
    ranking_games2 <- noduplicates2 %>% group_by(player_id) %>% 
      summarise(game_order = dense_rank(date_time_GMT), playerType = playerType)
    penalties_only2 <- filter(ranking_games2, playerType == 
                                "PenaltyOn")
    x_pens_season3 <- subset(fixednumber_penalties_season(year, 
                                                          penalties3, penalty_severity), select = -penalty_count)
    players_with_x_pens3 <- fixednumber_penalties_season(year, 
                                                         penalties3, penalty_severity)$player_id
    x_pen_playergames3 <- filter(season, player_id %in% players_with_x_pens3)
    noduplicates3 <- distinct(x_pen_playergames3, play_id, .keep_all = TRUE)
    ranking_games3 <- noduplicates3 %>% group_by(player_id) %>% 
      summarise(game_order = dense_rank(date_time_GMT), playerType = playerType)
    penalties_only3 <- filter(ranking_games3, playerType == 
                                "PenaltyOn")
  } else {
    x_pens_season1 <- subset(fixednumber_penalties_season(year, 
                                                          penalties1, penalty_severity), select = -penalty_count)
    players_with_x_pens1 <- fixednumber_penalties_season(year, 
                                                         penalties1, penalty_severity)$player_id
    x_pen_playergames1 <- filter(season, player_id %in% players_with_x_pens1)
    noduplicates1 <- distinct(x_pen_playergames1, play_id, .keep_all = TRUE)
    ranking_games1 <- noduplicates1 %>% group_by(player_id) %>% 
      summarise(game_order = dense_rank(date_time_GMT), playerType = playerType, 
                penaltySeverity = penaltySeverity)
    penalties_only1 <- filter(ranking_games1, playerType == 
                                "PenaltyOn", penaltySeverity == penalty_severity)
    x_pens_season2 <- subset(fixednumber_penalties_season(year, 
                                                          penalties2, penalty_severity), select = -penalty_count)
    players_with_x_pens2 <- fixednumber_penalties_season(year, 
                                                         penalties2, penalty_severity)$player_id
    x_pen_playergames2 <- filter(season, player_id %in% players_with_x_pens2)
    noduplicates2 <- distinct(x_pen_playergames2, play_id, .keep_all = TRUE)
    ranking_games2 <- noduplicates2 %>% group_by(player_id) %>% 
      summarise(game_order = dense_rank(date_time_GMT), playerType = playerType, 
                penaltySeverity = penaltySeverity)
    penalties_only2 <- filter(ranking_games2, playerType == 
                                "PenaltyOn", penaltySeverity == penalty_severity)
    x_pens_season3 <- subset(fixednumber_penalties_season(year, 
                                                          penalties3, penalty_severity), select = -penalty_count)
    players_with_x_pens3 <- fixednumber_penalties_season(year, 
                                                         penalties3, penalty_severity)$player_id
    x_pen_playergames3 <- filter(season, player_id %in% players_with_x_pens3)
    noduplicates3 <- distinct(x_pen_playergames3, play_id, .keep_all = TRUE)
    ranking_games3 <- noduplicates3 %>% group_by(player_id) %>% 
      summarise(game_order = dense_rank(date_time_GMT), playerType = playerType, 
                penaltySeverity = penaltySeverity)
    penalties_only3 <- filter(ranking_games3, playerType == 
                                "PenaltyOn", penaltySeverity == penalty_severity)
  }
  
  waitingtimes_list <- list()
  waitingtimes1_list <- list()
  for (player in penalties_only1$player_id) {
    player_diffs <- filter(penalties_only1, player_id == 
                             player)
    waitingtimes1 <- data.frame(player_id = player_diffs$player_id[1:(nrow(player_diffs) - 
                                                                        1)], penalty_game = NA, next_penalty_game = NA, 
                                game_diff = NA, game_count = NA)
    for (game in 1:(nrow(player_diffs) - 1)) {
      penalty_game <- player_diffs$game_order[game]
      waitingtimes1$penalty_game[game] <- penalty_game
      next_penalty_game <- player_diffs$game_order[game + 
                                                     1]
      waitingtimes1$next_penalty_game[game] <- next_penalty_game
      game_diff <- next_penalty_game - penalty_game
      waitingtimes1$game_diff[game] <- game_diff
      game_count <- x_pens_season1$game_count[x_pens_season1$player_id == 
                                                player]
      waitingtimes1$game_count[game] <- game_count
    }
    waitingtimes1_list[[player]] <- waitingtimes1
  }
  waitingtimes1 <- do.call(rbind, waitingtimes1_list)
  waitingtimes_list[[1]] <- waitingtimes1
  waitingtimes2_list <- list()
  for (player in penalties_only2$player_id) {
    player_diffs <- filter(penalties_only2, player_id == 
                             player)
    waitingtimes2 <- data.frame(player_id = player_diffs$player_id[1:(nrow(player_diffs) - 
                                                                        1)], penalty_game = NA, next_penalty_game = NA, 
                                game_diff = NA, game_count = NA)
    for (game in 1:(nrow(player_diffs) - 1)) {
      penalty_game <- player_diffs$game_order[game]
      waitingtimes2$penalty_game[game] <- penalty_game
      next_penalty_game <- player_diffs$game_order[game + 
                                                     1]
      waitingtimes2$next_penalty_game[game] <- next_penalty_game
      game_diff <- next_penalty_game - penalty_game
      waitingtimes2$game_diff[game] <- game_diff
      game_count <- x_pens_season2$game_count[x_pens_season2$player_id == 
                                                player]
      waitingtimes2$game_count[game] <- game_count
    }
    waitingtimes2_list[[player]] <- waitingtimes2
  }
  waitingtimes2 <- do.call(rbind, waitingtimes2_list)
  waitingtimes_list[[2]] <- waitingtimes2
  waitingtimes3_list <- list()
  for (player in penalties_only3$player_id) {
    player_diffs <- filter(penalties_only3, player_id == 
                             player)
    waitingtimes3 <- data.frame(player_id = player_diffs$player_id[1:(nrow(player_diffs) - 
                                                                        1)], penalty_game = NA, next_penalty_game = NA, 
                                game_diff = NA, game_count = NA)
    for (game in 1:(nrow(player_diffs) - 1)) {
      penalty_game <- player_diffs$game_order[game]
      waitingtimes3$penalty_game[game] <- penalty_game
      next_penalty_game <- player_diffs$game_order[game + 
                                                     1]
      waitingtimes3$next_penalty_game[game] <- next_penalty_game
      game_diff <- next_penalty_game - penalty_game
      waitingtimes3$game_diff[game] <- game_diff
      game_count <- x_pens_season3$game_count[x_pens_season3$player_id == 
                                                player]
      waitingtimes3$game_count[game] <- game_count
    }
    waitingtimes3_list[[player]] <- waitingtimes3
  }
  waitingtimes3 <- do.call(rbind, waitingtimes3_list)
  waitingtimes_list[[3]] <- waitingtimes3
  return(waitingtimes_list)
}