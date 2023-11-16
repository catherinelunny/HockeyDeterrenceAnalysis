#graph of time between penalties vs. probability
fixedwindow20192020 <- ggplot(result20192020, aes(x = game_difference, y = percent)) +
  geom_bar(stat = "identity", width = 5) +
  labs(x = " \U03C4 (Games)", y= "p(\U03C4)")

ggsave("results/2019twopenaltiesdistribution.png",fixedwindow20192020)