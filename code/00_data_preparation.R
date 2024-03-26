library(tidyverse)
library(patchwork)

# Open data
player_penalties <- read.csv("data/plays_players.csv")
penalties <- read.csv("data/game_penalties.csv")
players <- read.csv("data/player_info.csv")
all_player_plays <- readRDS("intermediate_data/plays_players1.rds")

#filtering the penalties to only include plays from the player data frames 
filtered_penalties <- penalties %>%
  filter(play_id %in% player_penalties$'play_id')
saveRDS(filtered_penalties,"intermediate_data/filtered_penalties.rds")

# filtering out all penalties so that this data frame only has non-penalty plays
player_plays_nonpenalty <- anti_join(all_player_plays, filtered_penalties, by = "play_id")
player_plays_nonpenalty$penaltySeverity <- NA
player_plays_nonpenalty$penaltyMinutes <- 0
saveRDS(all_player_plays, "intermediate_data/all_player_plays.rds")

games <- read.csv("data/games.csv")
#write.csv(games, file = "data/games.csv", row.names = FALSE)

games$date_time_GMT <- substr(games$date_time_GMT, 1, 10)
games$date_time_GMT <- as.Date(games$date_time_GMT)
saveRDS(games,"intermediate_data/games.rds")

games_ordered <- games %>% arrange(date_time_GMT)
saveRDS(games_ordered,"data/games_ordered.rds")


#Creating a data frame containing all information on penalty plays with the player_ids
complete_penalty_info <- cbind(player_penalties, filtered_penalties)
complete_penalty_info <- complete_penalty_info[, c("play_id", "game_id", "player_id", "playerType", "penaltySeverity", "penaltyMinutes")]
saveRDS(complete_penalty_info, "intermediate_data/complete_penalty_info.rds")

all_player_plays <- rbind(complete_penalty_info, player_plays_nonpenalty)

all_player_plays <- merge(all_player_plays, games_ordered, by = "game_id", all.x = TRUE)

# # Creating a data frame that contains all information on all plays, including non-penalties
# all_plays <- merge(all_player_plays, penalties, by = "play_id", all = TRUE)

# unique_elements <- unique(complete_penalty_info$player_id)
# print(unique_elements)


# Calculating the total number of penalty minutes
all_player_plays <- all_player_plays %>%
  mutate(accumulated_column = cumsum(penaltyMinutes))

# complete_penalty_info <- complete_penalty_info %>%
#   group_by(penaltySeverity) %>%
#   mutate(category_counts = n())

# making a column with each player's total penalty minutes
all_player_plays <- all_player_plays %>%
  group_by(player_id) %>%
  mutate(totalPenaltyMinutes = sum(penaltyMinutes))

saveRDS(all_player_plays,"intermediate_data/all_player_plays.rds")

# Making only one entry for each player that still contains their total number of penalties
unique_player_play_info <- all_player_plays %>%
  filter(!duplicated(player_id))


# creating a column that shows what percent of total penalties each player accounts for
unique_player_play_info$penaltyPercentages <- unique_player_play_info$'totalPenaltyMinutes' / sum(unique_player_play_info$'totalPenaltyMinutes')


#sorting players total penalty minutes in descending order
unique_player_play_info <- unique_player_play_info[order(-unique_player_play_info$totalPenaltyMinutes), ]

#creating a column ranking players from most penalty minutes to least
unique_player_play_info$ranked_players <- rank(-unique_player_play_info$totalPenaltyMinutes, ties.method = "first")

# a column creating player percentiles based on penalty minutes rankings
unique_player_play_info$playerPercentages <- (unique_player_play_info$ranked_players) / nrow(unique_player_play_info)

# calculating cumulative percentages of penalties in order
unique_player_play_info$cumPlays <- cumsum(unique_player_play_info$penaltyPercentages)

#sorting players total penalty minutes in descending order
unique_player_play_info <- unique_player_play_info[order(-unique_player_play_info$totalPenaltyMinutes), ]

saveRDS(unique_player_play_info, "intermediate_data/unique_player_play_info.rds")

n_of_bins <- 10
cut_breaks <- seq(0,1,length.out = n_of_bins+1)
cut_breaks_labels <- sprintf(cut_breaks, fmt = "%.2f")
cut_breaks_labels <- paste(cut_breaks_labels[-length(cut_breaks_labels)],
                           cut_breaks_labels[-1],
                           sep = "-")

# Calculate the grouping of penalties within each player career:
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

career_data <- careers %>% 
  group_by(player_id,game_percentile) %>% 
  reframe(penalty_minutes_share = sum(penaltyMinutes) / dplyr::first(total_penalty_minutes)) %>% 
  ungroup() %>% 
  complete(player_id,game_percentile,
           fill = list(penalty_minutes_share = 0),explicit = F) %>% 
  na.omit() %>% 
  ungroup()
saveRDS(career_data,"intermediate_data/career_data.rds")

mean_share_table <- career_data %>% 
  group_by(game_percentile) %>% 
  summarise(mean_share_of_total_penalty_minutes = round(mean(penalty_minutes_share),3))
saveRDS(mean_share_table,"intermediate_data/mean_share_table.rds")

player_games <- all_player_plays %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
  )
saveRDS(player_games,"intermediate_data/player_games.rds")

# Summary table
tab_games_by_players <- all_player_plays %>% 
  group_by(player_id) %>% 
  summarise(unique_games = length(unique(game_id)))
saveRDS(tab_games_by_players,"intermediate_data/tab_games_by_players.rds")

tab_seasons_by_players <- all_player_plays %>%
  group_by(player_id) %>%
  summarise(unique_seasons = length(unique(season)))
saveRDS(tab_seasons_by_players,"intermediate_data/tab_seasons_by_players.rds")

# data set that has the number of games a player played in each season and the percent of the total games in their career that accounts for
normalized_player_data <- all_player_plays %>%
  group_by(player_id, season) %>%
  summarise(total_plays = length(unique(play_id))) %>%
  group_by(player_id) %>%
  mutate(normalized_plays = total_plays / sum(total_plays))
saveRDS(normalized_player_data,"intermediate_data/normalized_player_data.rds")

# data frame with the average amount of games played per season for players
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

# isolating the penalty games in the 2019-2020 season
penaltygames20192020 <- filter(all_player_plays, season == "20192020")
penaltygames20192020 <- filter(penaltygames20192020, playerType == "PenaltyOn")
penaltygames20192020 <- penaltygames20192020 %>%
  group_by(game_id) %>%
  summarize_all(first)
penaltygames20192020 <- subset(penaltygames20192020, select = game_id)
saveRDS(penaltygames20192020, "intermediate_data/penaltygames20192020.rds")

# making a separate dataset for the 2019-2020 season
season20192020 <- filter(all_player_plays, season == "20192020")
saveRDS(season20192020,"intermediate_data/season20192020.rds")

#variable counting the number of games in the season
game_count2019 <- n_distinct(season20192020$game_id)

# Trial Analysis for the 2019-2020 season
# Data frame that shows the number of games and penalties for players during the 2019-2020 season 
 player_games20192020 <- season20192020 %>%
   group_by(player_id) %>%
   summarize(game_count = n_distinct(unique(game_id)),
             penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
   )
 saveRDS(player_games20192020,"intermediate_data/player_games20192020.rds") 

# attempt at using the method outlined in the paper, a probability distribution of the amount of games in between each penalty for players
 # isolating the players who had 2 penalties in the 2019-2020 season
fixedwindow2pen_20192020 <- filter(player_games20192020, penalty_count == 2)
saveRDS(fixedwindow2pen_20192020,"intermediate_data/fixedwindow2pen_20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
twopenalties20192020 <- filter(season20192020, player_id %in% fixedwindow2pen_20192020$player_id)
twopenalties20192020 <- distinct(twopenalties20192020, play_id, .keep_all = TRUE)
twopenalties20192020 <- twopenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_order = dense_rank(date_time_GMT),
            playerType = playerType)
twopenalties20192020 <- filter(twopenalties20192020, playerType == "PenaltyOn")
saveRDS(twopenalties20192020,"intermediate_data/twopenalties20192020.rds")

# data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
twopenaltieswaitingtimes20192020 <- twopenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

#applying formula (9) from the paper to determine the distribution
# twopenaltieswaitingtimes20192020$distribution <- (2*(game_count2019 - twopenaltieswaitingtimes20192020$game_difference))/(game_count2019*(game_count2019 + 1))
# saveRDS(twopenaltieswaitingtimes20192020,"intermediate_data/twopenaltieswaitingtimes20192020.rds")

twopenaltieswaitingtimes20192020$distribution <- 1/nrow(twopenaltieswaitingtimes20192020)
saveRDS(twopenaltieswaitingtimes20192020,"intermediate_data/twopenaltieswaitingtimes20192020.rds")

#the same analysis, but now for 3 penalties. isolating the players who had 3 penalties in the 2019-2020 season
fixedwindow3pen_20192020 <- filter(player_games20192020, penalty_count == 3)
saveRDS(fixedwindow3pen_20192020,"intermediate_data/fixedwindow3pen_20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
threepenalties20192020 <- filter(season20192020, player_id %in% fixedwindow3pen_20192020$player_id)
threepenalties20192020 <- distinct(threepenalties20192020, play_id, .keep_all = TRUE)
threepenalties20192020 <- threepenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_order = dense_rank(date_time_GMT),
            playerType = playerType)
threepenalties20192020 <- filter(threepenalties20192020, playerType == "PenaltyOn")

saveRDS(threepenalties20192020,"intermediate_data/threepenalties20192020.rds")

# data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
threepenwaitingtimes20192020 <- threepenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

#applying formula (10) from the paper to determine the distribution
threepenwaitingtimes20192020$distribution <- 1/nrow(threepenwaitingtimes20192020)
saveRDS(threepenwaitingtimes20192020,"intermediate_data/threepenwaitingtimes20192020.rds")



#the same analysis, but now for 4 penalties. isolating the players who had 4 penalties in the 2019-2020 season
fixedwindow4pen_20192020 <- filter(player_games20192020, penalty_count == 4)
saveRDS(fixedwindow4pen_20192020,"intermediate_data/fixedwindow4pen_20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
fourpenalties20192020 <- filter(season20192020, player_id %in% fixedwindow4pen_20192020$player_id)
fourpenalties20192020 <- distinct(fourpenalties20192020, play_id, .keep_all = TRUE)
fourpenalties20192020 <- fourpenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_order = dense_rank(date_time_GMT),
            playerType = playerType)
fourpenalties20192020 <- filter(fourpenalties20192020, playerType == "PenaltyOn")
saveRDS(fourpenalties20192020,"intermediate_data/fourpenalties20192020.rds")

# data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
fourpenwaitingtimes20192020 <- fourpenalties20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

#applying formula (10) from the paper to determine the distribution
fourpenwaitingtimes20192020$distribution <- 1/nrow(fourpenwaitingtimes20192020)
saveRDS(fourpenwaitingtimes20192020,"intermediate_data/fourpenwaitingtimes20192020.rds")

#CREATING DATAFRAMES FOR THE SAME ANALYSIS, BUT WITH MAJOR PENALTIES
#data frame showing the number of major penalties each offending player had in the 2019-2020 season 
majorpenalties20192020 <- season20192020 %>%
  group_by(player_id) %>%
  summarize(game_count = n_distinct(unique(game_id)),
            major_penalty_count = n_distinct(unique(play_id[penaltySeverity == "Major"]))
  )
saveRDS(majorpenalties20192020,"intermediate_data/majorpenalties20192020.rds")

two_major_players20192020 <- filter(majorpenalties20192020, major_penalty_count == 2)
saveRDS(two_major_players20192020,"intermediate_data/two_major_players20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
two_majors20192020 <- filter(season20192020, player_id %in% two_major_players20192020$player_id)
two_majors20192020 <- distinct(two_majors20192020, play_id, .keep_all = TRUE)
two_majors20192020 <- two_majors20192020 %>%
  group_by(player_id) %>%
  summarise(game_order = dense_rank(date_time_GMT),
            penaltySeverity = penaltySeverity)
two_majors20192020 <- filter(two_majors20192020, penaltySeverity == "Major")
saveRDS(two_majors20192020,"intermediate_data/two_majors20192020.rds")

# data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
twomajor_waitingtimes20192020 <- two_majors20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

twomajor_waitingtimes20192020$distribution <- 1/nrow(twomajor_waitingtimes20192020)
saveRDS(twomajor_waitingtimes20192020,"intermediate_data/twomajor_waitingtimes20192020.rds")


#the same analysis, but now for 3 penalties. isolating the players who had 3 penalties in the 2019-2020 season
three_major_players20192020 <- filter(majorpenalties20192020, major_penalty_count == 3)
saveRDS(three_major_players20192020,"intermediate_data/three_major_players20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
three_majors20192020 <- filter(season20192020, player_id %in% three_major_players20192020$player_id)
three_majors20192020 <- distinct(three_majors20192020, play_id, .keep_all = TRUE)
three_majors20192020 <- three_majors20192020 %>%
  group_by(player_id) %>%
  summarise(game_order = dense_rank(date_time_GMT),
            penaltySeverity = penaltySeverity)
three_majors20192020 <- filter(three_majors20192020, penaltySeverity == "Major")

saveRDS(three_majors20192020,"intermediate_data/three_majors20192020.rds")

# data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
threemajor_waitingtimes20192020 <- three_majors20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

threemajor_waitingtimes20192020$distribution <- 1/nrow(threemajor_waitingtimes20192020)
saveRDS(threemajor_waitingtimes20192020,"intermediate_data/threemajor_waitingtimes20192020.rds")



#the same analysis, but now for 4 penalties. isolating the players who had 4 penalties in the 2019-2020 season
four_major_players20192020 <- filter(majorpenalties20192020, major_penalty_count == 4)
saveRDS(four_major_players20192020,"intermediate_data/four_major_players20192020.rds")

# ranking the games where players had penalties through ranking the games that players played in in 2019-2020 season
four_majors20192020 <- filter(season20192020, player_id %in% four_major_players20192020$player_id)
four_majors20192020 <- distinct(four_majors20192020, play_id, .keep_all = TRUE)
four_majors20192020 <- four_majors20192020 %>%
  group_by(player_id) %>%
  summarise(game_order = dense_rank(date_time_GMT),
            penaltySeverity = penaltySeverity)
four_majors20192020 <- filter(four_majors20192020, penaltySeverity == "Major")
saveRDS(four_majors20192020,"intermediate_data/four_majors20192020.rds")

# data frame with waiting times between player's two penalties in 2019-2020 season- subtracts the rankings of games from each other
fourmajor_waitingtimes20192020 <- four_majors20192020 %>%
  group_by(player_id) %>%
  summarise(game_difference = diff(game_order))

#applying formula (10) from the paper to determine the distribution
fourmajor_waitingtimes20192020$distribution <- 1/nrow(fourmajor_waitingtimes20192020)
saveRDS(fourmajor_waitingtimes20192020,"intermediate_data/fourmajor_waitingtimes20192020.rds")

# # probability distribution of waiting times
# waitingtimes20192020_dist <- data.frame(prop.table(table(waitingtimes20192020$game_difference)))
# names(waitingtimes20192020_proportions)[names(waitingtimes20192020_proportions) == "Var1"] <- "game_difference"
# names(waitingtimes20192020_proportions)[names(waitingtimes20192020_proportions) == "Freq"] <- "percentage"
# saveRDS(waitingtimes20192020_proportions,"intermediate_data/waitingtimes20192020_proportions.rds")

# IGNORE

#making a separate data frame for each season
# season20002001 <- filter(all_plays, season == "20002001")
# season20012002 <- filter(all_plays, season == "20012002")
# season20022003 <- filter(all_plays, season == "20022003")
# season20032004 <- filter(all_plays, season == "20032004")
# season20042005 <- filter(all_plays, season == "20042005")
# season20052006 <- filter(all_plays, season == "20052006")
# season20062007 <- filter(all_plays, season == "20062007")
# season20072008 <- filter(all_plays, season == "20072008")
# season20082009 <- filter(all_plays, season == "20082009")
# season20092010 <- filter(all_plays, season == "20092010")
# season20102011 <- filter(all_plays, season == "20102011")
# season20112012 <- filter(all_plays, season == "20112012")
# season20122013 <- filter(all_plays, season == "20122013")
# season20132014 <- filter(all_plays, season == "20132014")
# season20142015 <- filter(all_plays, season == "20142015")
# season20152016 <- filter(all_plays, season == "20152016")
# season20162017 <- filter(all_plays, season == "20162017")
# season20172018 <- filter(all_plays, season == "20172018")
# season20182019 <- filter(all_plays, season == "20182019")
# season20202021 <- filter(all_plays, season == "20202021")
# season20212022 <- filter(all_plays, season == "20212022")


#  player_games20002001 <- season20002001 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  fixedwindow_2000 <- filter(player_games2000, penalty_count == 2)
# 
# 
# # Bar graph showing the distribution of all of the games played by players who had 2 penalties in 2000
# fixedwindow2000 <- ggplot(fixedwindow_2000, aes(x = player_id, y = game_count) ) +
#   geom_bar(stat = "identity")
# print(fixedwindow2000)
# 
#  player_games20012002 <- season20012002 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20022003 <- season20022003 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20032004 <- season20032004 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20042005 <- season20042005 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20052006 <- season20052006 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20062007 <- season20062007 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20072008 <- season20072008 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#                           penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20082009 <- season20082009 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20092010 <- season20092010 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20102011 <- season20102011 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20112012 <- season20112012 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20122013 <- season20122013 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20132014 <- season20132014 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20142015 <- season20142015 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20152016 <- season20152016 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
#  player_games20162017 <- season20162017 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
#  
#  player_games20172018 <- season20172018 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )
# 
# 
#  player_games20182019 <- season20182019 %>%
#    group_by(player_id) %>%
#    summarize(game_count = n_distinct(unique(game_id)),
#              penalty_count = n_distinct(unique(play_id[playerType == "PenaltyOn"]))
#    )



