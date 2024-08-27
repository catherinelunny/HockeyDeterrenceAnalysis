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