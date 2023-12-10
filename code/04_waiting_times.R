waitingtimes20192020_proportions <- readRDS("intermediate_data/waitingtimes20192020_proportions.rds")

#graph of time between penalties vs. probability
waitingtimes20192020 <- ggplot(waitingtimes20192020_proportions, aes(x = game_difference, y = percentage)) +
  geom_bar(stat = "identity", width = 5) +
  labs(x = " \U03C4 (Games)", y= "p(\U03C4)")

ggsave("results/2019twopenaltiesdistribution.png",waitingtimes20192020)