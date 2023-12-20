tackler_stats <- tackles_with_bc_id %>%
  group_by(nflId) %>%
  summarise(total_tackles1 = sum(tackle),
            total_assists = sum(assist),
            total_tackles = total_tackles1 + total_assists,
            total_missed_tackles = sum(pff_missedTackle)) %>%
  mutate(missed_tackle_percentage = total_missed_tackles / (total_tackles + total_missed_tackles),
         made_tackle_percentage = 1-missed_tackle_percentage)

bc_stats <- tackles_with_bc_id %>%
  group_by(ballCarrierId) %>%
  summarise(total_tackles1 = sum(tackle),
            total_assists = sum(assist),
            total_tackles = total_tackles1 + total_assists,
            total_missed_tackles = sum(pff_missedTackle)) %>%
  mutate(missed_tackle_percentage = total_missed_tackles / (total_tackles + total_missed_tackles))

made_tackle_percentage <- tackler_stats %>%
  select(nflId, made_tackle_percentage)

missed_tackle_percentage <- bc_stats %>%
  select(ballCarrierId, missed_tackle_percentage)

updated_tackles_df <- merge(tackles_with_bc_id, made_tackle_percentage, by = "nflId")
updated_tackles_df <- merge(updated_tackles_df, missed_tackle_percentage, by = "ballCarrierId")

tackle_value_df <- updated_tackles_df %>%
  mutate(any_tackle = tackle + assist,
         tackle_value = any_tackle + missed_tackle_percentage - 1) %>%
  group_by(nflId) %>%
  summarise(mean_tackle_value = mean(tackle_value),
            total_tackle_value = sum(tackle_value),
            appearances = n())

write.csv(tackle_value_df, "tackle_value.csv")
  