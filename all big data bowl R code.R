# Load necessary libraries
library(tidyr)
library(dplyr)
library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gt)
library(gtExtras)


##To get tackles with ballcarrier id

## Load in the data sets: plays and tackles
plays <- read.csv("plays.csv")
tackles <- read.csv("tackles.csv")
players <- read.csv("players.csv")

#Get ball carrier info
ball_carrier_id_info <- plays %>%
  select(gameId, playId, ballCarrierId) %>%
  mutate(whole_id = paste0(gameId, playId)) %>%
  select(whole_id, ballCarrierId)

#Get specific play id
tackles$whole_id <- paste0(tackles$gameId, tackles$playId)

#merge the tackles data set with the ball carrier info data set
tackles_with_bc_id <- merge(tackles, ball_carrier_id_info, by = ("whole_id"))

write.csv(tackles_with_bc_id, "tackles_bc_id.csv")


## Making tackle value attribute

#Get basic defender tackling statistics
tackler_stats <- tackles_with_bc_id %>%
  group_by(nflId) %>%
  summarise(total_tackles1 = sum(tackle),
            total_assists = sum(assist),
            total_tackles = total_tackles1 + total_assists,
            total_missed_tackles = sum(pff_missedTackle)) %>%
  mutate(missed_tackle_percentage = total_missed_tackles / (total_tackles + total_missed_tackles),
         made_tackle_percentage = 1-missed_tackle_percentage)

#Get basic ball carrier statistics
bc_stats <- tackles_with_bc_id %>%
  group_by(ballCarrierId) %>%
  summarise(total_tackles1 = sum(tackle),
            total_assists = sum(assist),
            total_tackles = total_tackles1 + total_assists,
            total_missed_tackles = sum(pff_missedTackle)) %>%
  mutate(missed_tackle_percentage = total_missed_tackles / (total_tackles + total_missed_tackles))

#Get how often a tackler makes a tackle
made_tackle_percentage <- tackler_stats %>%
  select(nflId, made_tackle_percentage)

#Gets how often a ball carrier gets a missed tackle
missed_tackle_percentage <- bc_stats %>%
  select(ballCarrierId, missed_tackle_percentage)

#For every play/tackle, have the tackler's made tackle percentage and the ball carrier's missed tackle percentage
updated_tackles_df <- merge(tackles_with_bc_id, made_tackle_percentage, by = "nflId")
updated_tackles_df <- merge(updated_tackles_df, missed_tackle_percentage, by = "ballCarrierId")

#From that data set, calculate the tackle value
tackle_value_df <- updated_tackles_df %>%
  mutate(any_tackle = tackle + assist,
         tackle_value = any_tackle + missed_tackle_percentage - 1) %>%
  group_by(nflId) %>%
  summarise(mean_tackle_value = mean(tackle_value),
            total_tackle_value = sum(tackle_value),
            appearances = n())

#Convert them into csv's
write.csv(tackle_value_df, "tackle_value.csv")
write.csv(tackler_stats, "tackler_stats.csv")


###

#Expected tackle opportunity:

#Look at data set
table(plays$offenseFormation)
table(plays$defendersInTheBox)
table(plays$passResult)

#Get only designed runs
designed_runs <- plays %>%
  mutate(unique_play_id = paste0(gameId, playId)) %>%
  filter(passResult=="")

#Read in tracking data
tracking_w1 <- read.csv("tracking_week_1.csv")
tracking_w2 <- read.csv("tracking_week_2.csv")
tracking_w3 <- read.csv("tracking_week_3.csv")
tracking_w4 <- read.csv("tracking_week_4.csv")
tracking_w5 <- read.csv("tracking_week_5.csv")
tracking_w6 <- read.csv("tracking_week_6.csv")
tracking_w7 <- read.csv("tracking_week_7.csv")
tracking_w8 <- read.csv("tracking_week_8.csv")
tracking_w9 <- read.csv("tracking_week_9.csv")

tracking_all <- rbind(tracking_w1, tracking_w2, tracking_w3, tracking_w4, tracking_w5, 
                      tracking_w6, tracking_w7, tracking_w8, tracking_w9)

tracking_all <- tracking_all %>%
  mutate(unique_play_id = paste0(gameId, playId))

#write.csv(tracking_all, "tracking_all.csv")


##SECOND FRAME AFTER RUN

#Getting all the frames in which a run, handoff, or a caught pass occurred
run_frames <- tracking_all %>%
  mutate(unique_play_id = paste0(gameId, playId),
         unique_frame_id = paste0(unique_play_id, frameId)) %>%
  filter(event %in% c("run", "handoff", "pass_outcome_caught"))

#Taking that frame and then adding 2 to it to get the data from .2 seconds after
run_plus_2_unique_frame_ids <- as.numeric(run_frames$unique_frame_id) + 2

#Getting the tracking data from those frames
post_run_frame_all <- tracking_all %>%
  mutate(unique_play_id = paste0(gameId, playId),
         unique_frame_id = paste0(unique_play_id, frameId)) %>%
  filter(unique_frame_id %in% run_plus_2_unique_frame_ids)

#Check where the football is at those frames
football_location_all <- post_run_frame_all %>%
  filter(club=="football") %>%
  select(unique_play_id,x,y) %>%
  rename("football_x" = "x",
         "football_y" = "y")

#Add the football location data to entire tracking data
post_run_frame_all <- merge(post_run_frame_all, football_location_all, by = "unique_play_id")

#Look at only designed runs
designed_runs_and_tracking_all <- merge(post_run_frame_all, designed_runs, by = "unique_play_id") %>%
  filter(club!="football")

#Differ between offense and defense
designed_runs_and_tracking_all$role <- ifelse(
  designed_runs_and_tracking_all$club != designed_runs_and_tracking_all$possessionTeam,
  "Defense",
  "Offense"
)


#Only look at defenders
designed_runs_and_tracking_all_defensive_players <- designed_runs_and_tracking_all %>%
  filter(role=="Defense")

#Depending on which way the offense is going (left or right), adjust the x coordinates
designed_runs_and_tracking_all_defensive_players$new_x <- ifelse(
  designed_runs_and_tracking_all_defensive_players$playDirection == "left",
  designed_runs_and_tracking_all_defensive_players$football_x - designed_runs_and_tracking_all_defensive_players$x,
  designed_runs_and_tracking_all_defensive_players$x - designed_runs_and_tracking_all_defensive_players$football_x
)
designed_runs_and_tracking_all_defensive_players$new_y <- ifelse(
  designed_runs_and_tracking_all_defensive_players$playDirection == "left",
  designed_runs_and_tracking_all_defensive_players$football_y - designed_runs_and_tracking_all_defensive_players$y,
  designed_runs_and_tracking_all_defensive_players$y - designed_runs_and_tracking_all_defensive_players$football_y
)
designed_runs_and_tracking_all_defensive_players <- designed_runs_and_tracking_all_defensive_players %>%
  group_by(unique_play_id) %>%
  filter(all(new_x > -1))

#Plot of all player's locations (very messy and indistinguishable)
plot(designed_runs_and_tracking_all_defensive_players$new_y, designed_runs_and_tracking_all_defensive_players$new_x)

#Sort the x and y values into ones values to give more clarity
designed_runs_and_tracking_all_defensive_players_heatmap <- designed_runs_and_tracking_all_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 1), labels = seq(1, 30, 1))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 1), labels = seq(-29.5, 29.5, 1)))

designed_runs_and_tracking_all_defensive_players_heatmap$new_x_rounded <- as.numeric(as.character(designed_runs_and_tracking_all_defensive_players_heatmap$new_x_rounded))

designed_runs_and_tracking_all_defensive_players_heatmap$new_y_rounded <- as.numeric(as.character(designed_runs_and_tracking_all_defensive_players_heatmap$new_y_rounded))

#Making a table of all locations and the amount of times a player has been there
count_table <- table(designed_runs_and_tracking_all_defensive_players_heatmap$new_y_rounded, designed_runs_and_tracking_all_defensive_players_heatmap$new_x_rounded)
count_df <- as.data.frame(count_table)
colnames(count_df) <- c("new_y_rounded", "new_x_rounded", "count")
count_df$new_x_rounded <- as.numeric(as.character(count_df$new_x_rounded))
count_df$new_y_rounded <- as.numeric(as.character(count_df$new_y_rounded))

#Making a heat map showing player's locations
post_run_frame_defensive_player_position_heat_map_all_plot <- ggplot(count_df, aes(x = new_y_rounded, y = new_x_rounded, fill = log(count + 1))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(title = "Post Run Frame Defensive Player Position Heat Map",
       subtitle = "All Weeks",
       x = "Line of Scrimmage",
       y = "") +
  theme_clean() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(-20, 20, by = 5), limits = c(-30, 30)) +
  scale_y_continuous(breaks = seq(min(count_df$new_x_rounded), max(count_df$new_x_rounded), by = 5))

ggsave("post_run_frame_defensive_player_position_heat_map_all_plot.png", post_run_frame_defensive_player_position_heat_map_all_plot)

#Getting total distance from ball using distance formula
#Also get adjusted y value that takes into account how far a player is left and right using absolute value to get actual distance
position_ranges <- designed_runs_and_tracking_all_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 2), labels = seq(1, 29, 2))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 2), labels = seq(-29, 29, 2))) %>%
  mutate(unique_and_nfl_id = paste0(unique_play_id, nflId) ) %>%
  mutate(y_dist_from_ball = abs(as.numeric(new_y))) %>%
  mutate(total_dist_from_ball = sqrt(new_x^2 + y_dist_from_ball^2))

#New table
table(position_ranges$new_x_rounded, position_ranges$new_y_rounded)

#Instead of relying on when the tackle happened for the response variable, we used when a tackle opportunity occurred, which is either a tackle or missed tackle
first_tackle_opportunity <- tackles_with_bc_id %>%
  mutate(unique_play_id = paste0(gameId, playId)) %>%
  group_by(unique_play_id) %>%
  summarise(nflId = first(nflId)) %>%
  filter(unique_play_id %in% designed_runs$unique_play_id) %>%
  mutate(tackle_opp_id = paste0(unique_play_id, nflId))

position_ranges$tackle_opp <- ifelse(
  position_ranges$unique_and_nfl_id %in% first_tackle_opportunity$tackle_opp_id,
  1,
  0)
position_ranges$tackle_opp <- as.factor(position_ranges$tackle_opp)

#Classify offensive formations into where the QB is presnap, which would be either shotgun or under center
position_ranges$qb_position <- ifelse(
  position_ranges$offenseFormation == "SHOTGUN",
  "Shotgun",
  "Under Center"
)


#Select all logistic model variables
tackle_opp <- position_ranges %>%
  ungroup() %>%
  select(unique_play_id, qb_position, new_x, y_dist_from_ball, tackle_opp, total_dist_from_ball, passProbability)

tackle_opp <- na.omit(tackle_opp)

#Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(tackle_opp$tackle_opp, p = 0.7, list = FALSE)
train_data <- tackle_opp[train_index, ]
test_data <- tackle_opp[-train_index, ]

#Train a logistic regression model
model <- glm(tackle_opp ~ . - unique_play_id, data = train_data, family = "binomial")

summary(model)

library("jtools")
summ(model)

# plot_coefs(summ(model))
# 
# coefficients <- coef(model)
# standard_errors <- summary(model)$coefficients[, "Std. Error"]
# 
# # Calculate z-scores
# z_scores <- coefficients / standard_errors
# 
# # Create a coefficient plot
# barplot(z_scores, names.arg = names(coefficients), col = "skyblue", main = "Standardized Coefficients")

# library(MASS)
# step_model <- stepAIC(model, direction = "both", 
#                       trace = FALSE)
# 
# summary(step_model)

#Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")
print(summary(predictions))

#If any player has more than a 10% chance of making the tackle, have them predicted to make tackle
#Arbitrary number to tests confidence matrix
predicted_classes <- ifelse(predictions > (.1), 1, 0)
conf_matrix <- table(predicted_classes, test_data$tackle_opp)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste0("Accuracy:", accuracy))
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
roc_curve <- pROC::roc(test_data$tackle_opp, predictions)
auc_value <- pROC::auc(roc_curve)
plot(roc_curve, main = paste0("ROC Curve - AUC =", round(auc_value, 3)))
dev.copy(png, "model_roc_curve_plot.png")
dev.off()

#getting all the play ids from the tackle opp data set so we can maintain exact output
good_ids <- tackle_opp$unique_play_id

#only getting data from those plays
position_ranges <- position_ranges %>%
  ungroup() %>%
  filter(unique_play_id %in% good_ids)

#Predict using the model on the entire data set instead of just the test data
all_predictions <- predict(model, newdata = tackle_opp, type = "response")

#Combining with the entire data set
all_data <- cbind(position_ranges, all_predictions)

#For every play, get the percent chance the defender makes the tackle relative to the other defenders
#This is because not all plays add up to 1 in terms of tackle responsibility
#Then calculate tackle over expectation metric
selected_data_probabilities <- all_data %>%
  select(unique_frame_id, nflId, tackle_opp, all_predictions) %>%
  group_by(unique_frame_id) %>%
  mutate(sum_predictions = sum(all_predictions),
         responsibility = all_predictions / sum_predictions) %>%
  ungroup() %>%
  mutate(tackle_oe = ifelse(
    tackle_opp==1,
    1-responsibility,
    0-responsibility
  )) 

#Get the total tackle over expectation and the average tackle over expectation
tackle_over_expected <- selected_data_probabilities %>%
  group_by(nflId) %>%
  summarise(total_tackle_oe = sum(tackle_oe),
            avg_tackle_oe = mean(tackle_oe),
            opportunities = n()) %>%
  arrange(desc(total_tackle_oe))

#Combine in with the players data set to get a good data set / csv
tackle_over_expected_with_names <- merge(tackle_over_expected, players, by = "nflId") %>%
  select(nflId, total_tackle_oe, avg_tackle_oe, opportunities, displayName) %>%
  arrange(desc(total_tackle_oe))

write.csv(tackle_over_expected_with_names, "tackle_over_expected_with_names.csv")

########

##########


#For clarity, we used player load, work load, and intensity interchangeably in this code, as we did not settle on the term at the initial state

#Get the difference in accelerations for every frame
tracking_all_agg <- tracking_all %>%
  filter(!is.na(nflId)) %>%
  group_by(nflId, unique_play_id) %>%
  mutate(
    acceleration_x = c(0, diff(x) / 0.1),
    acceleration_y = c(0, diff(y) / 0.1)
  )

#Calculate Workload using 2D Player Load formula
#Also add which play of the game (for each respective player) each play was on
tracking_all_workload <- tracking_all_agg %>%
  group_by(nflId, unique_play_id) %>%
  summarise(
    workload = sum(sqrt((c(0, diff(acceleration_x))^2) + (c(0, diff(acceleration_y))^2))),
    gameId = first(gameId),
    playId = first(playId)
  )  %>%
  arrange(gameId, nflId, playId) %>%
  group_by(gameId, nflId) %>%
  mutate(num_play = row_number()) %>%
  ungroup() %>%
  select(nflId, workload, num_play)

#We wanted to see if there was a drop off in intensity throughout the game, so we looked at all players at a whole
#Filtered out any play numbers that had 150 or less plays in the data for more accurate results
workload_by_play <- tracking_all_workload %>%
  group_by(num_play) %>%
  summarise(avg_workload = mean(workload),
            plays_ran = n()) %>%
  filter(plays_ran>150) %>%
  ungroup() %>%
  select(num_play, avg_workload)

#Plotted the drop off of intensity throughout the game
workload_by_play_number_plot <- ggplot(workload_by_play, aes(x = factor(num_play))) +
  geom_line(aes(y = avg_workload, group = 1, color = "red"), size = 1.2) +
  geom_point(aes(y = avg_workload), size = 3, color = "red") +
  labs(title = "Average Player Load By Play Number",
       x = "Number Play",
       y = "Average Player Load",
       color = "") +
  theme_light() +
  theme(legend.position = "none") +
  scale_x_discrete(breaks = seq(1, max(workload_by_play$num_play), by = 5))

ggsave("workload_by_play_number_plot.png", workload_by_play_number_plot)  

#Grouped by player and number play to see their mean, median, and standard deviation of player load.
#Standard deviation could only be calculated on plays with more than 1 play, so we filtered that out
workload_by_player_play <- tracking_all_workload %>%
  group_by(nflId, num_play) %>%
  summarise(mean_playerload = mean(workload, na.rm = T),
            sd_playerload = sd(workload, na.rm = T),
            median_playerload = median(workload, na.rm=T),
            occurences = n()) %>%
  filter(occurences>1)

#After getting how much they differ between different plays of the same play number, we wanted to see how much they differ based on play number
playerload_median_sd <- workload_by_player_play %>%
  group_by(nflId) %>%
  summarise(sd_playerload = sd(median_playerload, na.rm=T),
            first_and_last_play_dif = first(median_playerload) - last(median_playerload))%>%
  filter(!is.na(sd_playerload))

#After seeing there was good data, we ran a linear regression model to see the slope of their intensities
models <- workload_by_player_play %>%
  group_by(nflId) %>%
  do(model = lm(mean_playerload ~ num_play, data = .))
coefficients <- models %>%
  summarize(nflId = nflId[1], 
            Intercept = coef(model)[1], 
            Slope = coef(model)[2])

#Getting only player names
player_names <- players %>%
  select(nflId, displayName)

#Getting only a player's intensity slope and ID
players_player_load <- merge(coefficients, player_names, by = "nflId")
player_load <- players_player_load %>%
  select(nflId, Slope) %>%
  rename("fatigue" = "Slope") %>%
  filter(!is.na(fatigue))

write.csv(player_load, "player_load.csv")


###########
#Athleticism work

#Getting important metrics from the tracking data, such as max and average speed and acceleration
athleticism_work <- tracking_all %>%
  group_by(nflId) %>%
  filter(!is.na(nflId),
         gameId!=2022091809) %>%
  summarise(max_speed = max(s),
            max_acceleration = max(a),
            avg_speed = mean(s, na.rm = T),
            avg_acceleration = mean(a, na.rm = T))

###Seems like the tracking data is messed up in the game with id 2022091809. There are several very clear outliers, so I decided to remove that game from tracking data
anthony_brown <- tracking_all %>%
  filter(nflId == 43478)

#Getting important metrics from the players data, such as height and weight, and then convert height into inches
player_height_weight_info <- players %>%
  select(nflId, height, weight) %>%
  mutate(height_feet = as.numeric(sub("-.*", "", height)),
         height_inches = as.numeric(sub(".*-", "", height)),
         height_inches = height_feet * 12 + height_inches) %>%
  select(nflId, height_inches, weight)

#Getting a complete athleticism data set
athleticism_work <- merge(player_height_weight_info, athleticism_work, by = "nflId")
athleticism_work$nflId <- as.character(athleticism_work$nflId)

#Taking all columns that are numeric and then scaling it using Z scores
numeric_columns <- athleticism_work[, sapply(athleticism_work, is.numeric)]
normalized_data <- as.data.frame(lapply(numeric_columns, scale))

#Combine it back to the other data so we know the players each row belongs to
athleticism_work_normalized <- cbind(athleticism_work[, !sapply(athleticism_work, is.numeric)], normalized_data) %>%
  rename("nflId" = "athleticism_work[, !sapply(athleticism_work, is.numeric)]")

#Get total athleticism score by adding up all athletic attribute Z scores
athleticism_work_normalized$athleticism_score <-  athleticism_work_normalized$height_inches + 
  athleticism_work_normalized$weight + athleticism_work_normalized$max_speed + 
  athleticism_work_normalized$max_acceleration + athleticism_work_normalized$avg_speed + 
  athleticism_work_normalized$avg_acceleration

write.csv(athleticism_work_normalized, "athleticism_work_normalized.csv")

#Getting players IDs, names, and positions for future work
player_name_position <- players %>%
  select(nflId, displayName, position)

#Combine it with names for reference
athleticism_w_names <- merge(athleticism_work_normalized, player_name_position, by = "nflId")

#Create GT for good visualization
athleticism_gt <- athleticism_w_names %>%
  arrange(desc(athleticism_score)) %>%
  filter(athleticism_score>4.9) %>%
  select(displayName, position, athleticism_score, height_inches, weight, max_speed, 
         max_acceleration, avg_speed, avg_acceleration) %>%
  mutate(athleticism_score=round(athleticism_score,2),
         height_inches=round(height_inches,2),
         weight=round(weight,2),
         max_speed = round(max_speed, 2),
         max_acceleration=round(max_acceleration,2),
         avg_speed=round(avg_speed,2),
         avg_acceleration=round(avg_acceleration,2)) %>%
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(displayName = "Player",
             position = "Position",
             athleticism_score = "Total Athleticism Score",
             height_inches = "Height Score",
             weight = "Weight Score",
             max_speed = "Max Speed Score",
             max_acceleration = "Max Acceleration Score",
             avg_speed = "Average Speed Score",
             avg_acceleration = "Average Acceleration Score") %>%
  data_color(
    columns = c(athleticism_score, height_inches, weight, max_speed, 
                max_acceleration, avg_speed, avg_acceleration),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      )
      %>%
        as.character(),
      domain= NULL,
      reverse = FALSE
    )
  ) %>%
  gt_theme_espn()  %>%
  tab_header("Top 15 Athleticism Scores") %>%
  tab_caption("2022 NFL Season | Weeks 1-9 | Jonah Lubin & Charles Wells")

gtsave(athleticism_gt, "athleticism_gt.png")



############

#Load in all attribute CSVs, and select only the NFL ID and attribute values
momentum_csv <- read.csv("momentum_with_play.csv")
momentum_csv_selected <- momentum_csv %>%
  select(tacklerId, avgImpulse) %>%
  rename("nflId" = tacklerId)

tackle_over_expectation_csv <- read.csv("tackle_over_expected_with_names.csv")
tackle_over_expectation_csv_selected <- tackle_over_expectation_csv %>%
  select(nflId, avg_tackle_oe)

tackle_range_csv <- read.csv("tackle_radius.csv")
tackle_range_csv$tackle_range_metric <- tackle_range_csv$maxTackleRadius * tackle_range_csv$avgTackleRadius
tackle_range_csv_selected <- tackle_range_csv %>%
  select(tacklerId, tackle_range_metric) %>%
  rename("nflId" = "tacklerId")

strain_csv <- read.csv("strain.csv")
strain_csv_selected <- strain_csv %>%
  select(tacklerId, avgAvgStrain) %>%
  rename("nflId" = "tacklerId", "avgSTRAIN" = "avgAvgStrain")

tackle_value_csv <- read.csv("tackle_value.csv")
tackle_value_csv_selected <- tackle_value_csv %>%
  select(nflId, mean_tackle_value)

athleticism_score_csv <- read.csv("athleticism_work_normalized.csv")
athleticism_score_csv_selected <- athleticism_score_csv %>%
  select(nflId, athleticism_score)

allowed_yards_after_contact_csv <- read.csv("ayac.csv")
allowed_yards_after_contact_csv_selected <- allowed_yards_after_contact_csv %>%
  select(tacklerId, avgAyac) %>%
  rename("nflId" = "tacklerId")

player_load_fatigue_csv <- read.csv("player_load.csv")
player_load_fatigue_csv_selected <- player_load_fatigue_csv %>%
  select(nflId, fatigue)


#We want to only get players that are in all data frames
merged_data <- merge(merge(merge(merge(merge(merge(merge(momentum_csv_selected, tackle_over_expectation_csv_selected, by = "nflId")
                                                   , strain_csv_selected, by = "nflId"), tackle_value_csv_selected, by = "nflId"), athleticism_score_csv_selected, by = "nflId"), 
                                 allowed_yards_after_contact_csv_selected, by = "nflId"), player_load_fatigue_csv_selected, by = "nflId"), tackle_range_csv_selected, by = "nflId")

players_ids <- merged_data$nflId

#Get only players that have 100 or more plays in the tracking data
tracking_all <- read.csv("tracking_all.csv")

tracking_all$unique_play_id <- paste(tracking_all$gameId, tracking_all$playId)

tracking_all_players <- tracking_all %>%
  filter(!is.na(nflId)) %>%
  group_by(nflId, unique_play_id) %>%
  summarise(n = n()) %>%
  group_by(nflId) %>%
  summarise(plays = n()) %>%
  filter(plays>=100)

players_ids2 <- tracking_all_players$nflId

#Filter out those players in all attribute data sets
momentum <- momentum_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

tackle_oe <- tackle_over_expectation_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

range <- tackle_range_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

strain <- strain_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

tackle_value <- tackle_value_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

athleticism <- athleticism_score_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

ayac <- allowed_yards_after_contact_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)

fatigue <- player_load_fatigue_csv_selected %>%
  filter(nflId %in% players_ids) %>%
  filter(nflId %in% players_ids2)


#Normalize values for every data set using percentile normalization
percentile_normalize <- function(x) {
  rank_x <- rank(x)
  normalized_x <- rank_x / length(rank_x)
  return(normalized_x)
}

##Normalization work for the Adjustable Tackle Metric Hub
#Apply to all attributes and center it at 0 instead of .5
momentum_normalized <- momentum
momentum_normalized$avgImpulse <- percentile_normalize(momentum$avgImpulse) - .5

tackle_oe_normalized <- tackle_oe
tackle_oe_normalized$avg_tackle_oe <- percentile_normalize(tackle_oe$avg_tackle_oe) - .5

range_normalized <- range
range_normalized$tackle_range_metric <- percentile_normalize(range$tackle_range_metric) - .5

strain_normalized <- strain
strain_normalized$avgSTRAIN <- percentile_normalize(strain$avgSTRAIN) - .5

tackle_value_normalized <- tackle_value
tackle_value_normalized$mean_tackle_value <- percentile_normalize(tackle_value$mean_tackle_value) - .5

athleticism_normalized <- athleticism
athleticism_normalized$athleticism_score <- percentile_normalize(athleticism$athleticism_score) - .5

ayac_normalized <- ayac
ayac_normalized$avgAyac <- (percentile_normalize(ayac$avgAyac) - .5)*(-1)

fatigue_normalized <- fatigue
fatigue_normalized$fatigue <- percentile_normalize(fatigue$fatigue) - .5


#Merge the new normalized data sets

all_normalized_data <- merge(merge(merge(merge(merge(merge(merge(momentum_normalized, tackle_oe_normalized, by = "nflId")
                                                           , strain_normalized, by = "nflId"), tackle_value_normalized, by = "nflId"), athleticism_normalized, by = "nflId"), 
                                         ayac_normalized, by = "nflId"), fatigue_normalized, by = "nflId"), range_normalized, by = "nflId")

#Metric testing to see how it looked if we weighed all attributes evenly
all_normalized_data_metric <- all_normalized_data %>%
  mutate(final_metric = tackle_range_metric*(1/8)+avgImpulse*(1/8) + avg_tackle_oe*(1/8) + avgSTRAIN*(1/8) + mean_tackle_value*(1/8) + athleticism_score*(1/8) + avgAyac*(1/8) + fatigue*(1/8))

player_name_position <- players %>%
  select(nflId, displayName, position)

all_normalized_data2 <- merge(all_normalized_data_metric, player_name_position, by = "nflId")

write.csv(all_normalized_data2, "all_normalized_data2.csv", row.names = F)

#Split up all players by their position so they can select which position to sort by if wanted to
unique(all_normalized_data2$position)

dt_final <- all_normalized_data2 %>%
  filter(position %in% c("NT", "DT"))
write.csv(dt_final, "dt_data.csv", row.names = F)

de_final <- all_normalized_data2 %>%
  filter(position == "DE")
write.csv(de_final, "de_data.csv", row.names = F)

olb_final <- all_normalized_data2 %>%
  filter(position == "OLB")
write.csv(olb_final, "olb_data.csv", row.names = F)

ilb_final <- all_normalized_data2 %>%
  filter(position %in% c("ILB", "MLB"))
write.csv(ilb_final, "ilb_data.csv", row.names = F)

cb_final <- all_normalized_data2 %>%
  filter(position == "CB")
write.csv(cb_final, "cb_data.csv", row.names = F)

safety_final <- all_normalized_data2 %>%
  filter(position %in% c("SS", "FS", "DB"))
write.csv(safety_final, "safety_data.csv", row.names = F)



##Normalization work for the Adjustable Tackle Metric Hub
#Get the percentile of every player from 0 to 100

momentum_normalized2 <- momentum
momentum_normalized2$avgImpulse <- percentile_normalize(momentum$avgImpulse) * 100

tackle_oe_normalized2 <- tackle_oe
tackle_oe_normalized2$avg_tackle_oe <- percentile_normalize(tackle_oe$avg_tackle_oe) * 100

range_normalized2 <- range
range_normalized2$tackle_range_metric <- percentile_normalize(range$tackle_range_metric) * 100

strain_normalized2 <- strain
strain_normalized2$avgSTRAIN <- percentile_normalize(strain$avgSTRAIN) * 100

tackle_value_normalized2 <- tackle_value
tackle_value_normalized2$mean_tackle_value <- percentile_normalize(tackle_value$mean_tackle_value) * 100

athleticism_normalized2 <- athleticism
athleticism_normalized2$athleticism_score <- percentile_normalize(athleticism$athleticism_score) * 100

ayac_normalized2 <- ayac
ayac_normalized2$avgAyac <- (percentile_normalize(ayac$avgAyac) * 100)*(-1)+100

fatigue_normalized2 <- fatigue
fatigue_normalized2$fatigue <- percentile_normalize(fatigue$fatigue) * 100


#Merge normalized data sets

all_normalized_data_perc <- merge(merge(merge(merge(merge(merge(merge(momentum_normalized2, tackle_oe_normalized2, by = "nflId")
                                                                , strain_normalized2, by = "nflId"), tackle_value_normalized2, by = "nflId"), athleticism_normalized2, by = "nflId"), 
                                              ayac_normalized2, by = "nflId"), fatigue_normalized2, by = "nflId"), range_normalized2, by = "nflId")

#Metric testing to see how it looked if we weighed all attributes evenly
all_normalized_data_metric_perc <- all_normalized_data_perc %>%
  mutate(final_metric = tackle_range_metric*(1/8)+avgImpulse*(1/8) + avg_tackle_oe*(1/8) + avgSTRAIN*(1/8) + mean_tackle_value*(1/8) + athleticism_score*(1/8) + avgAyac*(1/8) + fatigue*(1/8))

#Merging so we can see attribute values, nflId, player name, and position
all_normalized_data2_perc <- merge(all_normalized_data_metric_perc, player_name_position, by = "nflId")

write.csv(all_normalized_data2_perc, "player_percentile_data.csv", row.names = F)

#Split up by position to allow for user to sort based on position
dt_final_2 <- all_normalized_data2_perc %>%
  filter(position %in% c("NT", "DT"))
write.csv(dt_final_2, "dt_data_2.csv", row.names = F)

de_final_2 <- all_normalized_data2_perc %>%
  filter(position == "DE")
write.csv(de_final_2, "de_data_2.csv", row.names = F)

olb_final_2 <- all_normalized_data2_perc %>%
  filter(position == "OLB")
write.csv(olb_final_2, "olb_data_2.csv", row.names = F)

ilb_final_2 <- all_normalized_data2_perc %>%
  filter(position %in% c("ILB", "MLB"))
write.csv(ilb_final_2, "ilb_data_2.csv", row.names = F)

cb_final_2 <- all_normalized_data2_perc %>%
  filter(position == "CB")
write.csv(cb_final_2, "cb_data_2.csv", row.names = F)

safety_final_2 <- all_normalized_data2_perc %>%
  filter(position %in% c("SS", "FS", "DB"))
write.csv(safety_final_2, "safety_data_2.csv", row.names = F)


