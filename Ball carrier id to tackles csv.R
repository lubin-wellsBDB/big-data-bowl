library(dplyr)
plays <- read.csv("data/plays.csv")
tackles <- read.csv("data/tackles.csv")
tracking_w1 <- read.csv("data/tracking_week_1.csv")
play_56 <- tracking_w1 %>%
  filter(playId == "56", gameId == "2022090800")
table(play_56$displayName)
table(tracking_w1$event)

ball_carrier_id_info <- plays %>%
  select(gameId, playId, ballCarrierId) %>%
  mutate(whole_id = paste0(gameId, playId)) %>%
  select(whole_id, ballCarrierId)

tackles$whole_id <- paste0(tackles$gameId, tackles$playId)

tackles_with_bc_id <- merge(tackles, ball_carrier_id_info, by = ("whole_id"))

write.csv(tackles_with_bc_id, "data/tackles_bc_id1.csv")

