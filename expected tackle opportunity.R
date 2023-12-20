#Take the approach that a good tackler can include attributes that are coachable and attributes that are not coachable
#Not coachable is athleticism, range, force
#Coachable is current success, play recognition

#Tackle expectation can be position on the field relative to ball carrier.
#Should include factors such as offensive formation, run likelihood, have to use player position within a certain radius (1 yard all around)


#Expected tackle opportunity:
table(plays$offenseFormation)
table(plays$defendersInTheBox)
table(plays$passResult)

library(dplyr)
designed_runs <- plays %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(passResult=="")

first_frame_w1 <- tracking_w1 %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  filter(frameId==1)


football_location_w1 <- first_frame_w1 %>%
  filter(club=="football") %>%
  select(unique_id,x,y) %>%
  rename("football_x" = "x",
         "football_y" = "y")



##FIRST FRAME MODEL

first_frame_w1 <- merge(first_frame_w1, football_location_w1, by = "unique_id")

designed_runs_and_tracking_w1 <- merge(first_frame_w1, designed_runs, by = "unique_id") %>%
  filter(club!="football")

designed_runs_and_tracking_w1$role <- ifelse(
  designed_runs_and_tracking_w1$club != designed_runs_and_tracking_w1$possessionTeam,
  "Defense",
  "Offense"
)


designed_runs_and_tracking_w1_defensive_players <- designed_runs_and_tracking_w1 %>%
  filter(role=="Defense")

designed_runs_and_tracking_w1_defensive_players$new_x <- ifelse(
  designed_runs_and_tracking_w1_defensive_players$playDirection == "left",
  designed_runs_and_tracking_w1_defensive_players$football_x - designed_runs_and_tracking_w1_defensive_players$x,
  designed_runs_and_tracking_w1_defensive_players$x - designed_runs_and_tracking_w1_defensive_players$football_x
)

designed_runs_and_tracking_w1_defensive_players$new_y <- ifelse(
  designed_runs_and_tracking_w1_defensive_players$playDirection == "left",
  designed_runs_and_tracking_w1_defensive_players$football_y - designed_runs_and_tracking_w1_defensive_players$y,
  designed_runs_and_tracking_w1_defensive_players$y - designed_runs_and_tracking_w1_defensive_players$football_y
)

designed_runs_and_tracking_w1_defensive_players <- designed_runs_and_tracking_w1_defensive_players %>%
  group_by(unique_id) %>%
  filter(all(new_x > -1))

plot(designed_runs_and_tracking_w1_defensive_players$new_y, designed_runs_and_tracking_w1_defensive_players$new_x)


library(ggplot2)
library(ggplot2)

library(dplyr)



designed_runs_and_tracking_w1_defensive_players_heatmap <- designed_runs_and_tracking_w1_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 1), labels = seq(1, 30, 1))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 1), labels = seq(-29.5, 29.5, 1)))

designed_runs_and_tracking_w1_defensive_players_heatmap$new_x_rounded <- as.numeric(as.character(designed_runs_and_tracking_w1_defensive_players_heatmap$new_x_rounded))

designed_runs_and_tracking_w1_defensive_players_heatmap$new_y_rounded <- as.numeric(as.character(designed_runs_and_tracking_w1_defensive_players_heatmap$new_y_rounded))

count_table <- table(designed_runs_and_tracking_w1_defensive_players_heatmap$new_y_rounded, designed_runs_and_tracking_w1_defensive_players_heatmap$new_x_rounded)

library(ggplot2)
library(ggthemes)

count_df <- as.data.frame(count_table)

colnames(count_df) <- c("new_y_rounded", "new_x_rounded", "count")
count_df$new_x_rounded <- as.numeric(as.character(count_df$new_x_rounded))

count_df$new_y_rounded <- as.numeric(as.character(count_df$new_y_rounded))

first_frame_defensive_player_position_heat_map_w1_plot <- ggplot(count_df, aes(x = new_y_rounded, y = new_x_rounded, fill = log(count + 1))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(title = "First Frame Defensive Player Position Heat Map",
       subtitle = "Week 1",
       x = "Line of Scrimmage",
       y = "") +
  theme_clean() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(-20, 20, by = 5), limits = c(-22.5, 22.5)) +
  scale_y_continuous(breaks = seq(min(count_df$new_x_rounded), max(count_df$new_x_rounded), by = 5))

ggsave("first_frame_defensive_player_position_heat_map_w1_plot.png", first_frame_defensive_player_position_heat_map_w1_plot)



position_ranges <- designed_runs_and_tracking_w1_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 2), labels = seq(1, 29, 2))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 2), labels = seq(-29, 29, 2))) %>%
  mutate(unique_and_nfl_id = paste0(unique_id, nflId) ) %>%
  mutate(y_dist_from_ball = abs(as.numeric(new_y))) %>%
  mutate(total_dist_from_ball = sqrt(new_x^2 + y_dist_from_ball^2))

table(position_ranges$new_x_rounded, position_ranges$new_y_rounded)

first_tackle_opportunity <- tackles_with_bc_id %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  group_by(unique_id) %>%
  summarise(nflId = first(nflId)) %>%
  filter(unique_id %in% designed_runs$unique_id) %>%
  mutate(tackle_opp_id = paste0(unique_id, nflId))

position_ranges$tackle_opp <- ifelse(
  position_ranges$unique_and_nfl_id %in% first_tackle_opportunity$tackle_opp_id,
  1,
  0)
position_ranges$tackle_opp <- as.factor(position_ranges$tackle_opp)
position_ranges$qb_position <- ifelse(
  position_ranges$offenseFormation == "SHOTGUN",
  "Shotgun",
  "Under Center"
)

# Assuming you have a dataframe named "your_data" with the relevant columns

# Load necessary libraries
library(tidyr)
library(dplyr)
library(caret)
library(glmnet)
tackle_opp <- position_ranges %>%
  ungroup() %>%
  select(unique_id, qb_position, new_x, y_dist_from_ball, tackle_opp, total_dist_from_ball)

tackle_opp <- na.omit(tackle_opp)

# Step 2: Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(tackle_opp$tackle_opp, p = 0.7, list = FALSE)
train_data <- tackle_opp[train_index, ]
test_data <- tackle_opp[-train_index, ]

# Step 3: Train a logistic regression model
model <- glm(tackle_opp ~ . - unique_id, data = train_data, family = "binomial")

summary(model)

# Step 4: Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")

predicted_classes <- ifelse(predictions > (1/11), 1, 0)
conf_matrix <- table(predicted_classes, test_data$tackle_opp)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste0("Accuracy:", accuracy))

precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste0("Precision:", precision))

# Recall
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste0("Recall:", recall))

# F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste0("F1 Score:", f1_score))

# ROC Curve and AUC
roc_curve <- pROC::roc(test_data$tackle_opp, predictions)
auc_value <- pROC::auc(roc_curve)
plot(roc_curve, main = paste0("ROC Curve - AUC =", round(auc_value, 3)))



# print(predictions)
# 
# test_data <- cbind(test_data, predictions)
# print(1/11)
# test_data$probability_to_tackle <- (test_data$predictions - (1/11)) * 100


# Combine predictions with actual values for comparison
result_df <- data.frame(Actual = test_data$tackle_opp, Predicted = predicted_classes)

# Display the first few rows of the result
head(result_df)


table(tracking_w1$event)





##SECOND FRAME AFTER RUN W1

run_frames <- tracking_w1 %>%
  mutate(unique_id = paste0(gameId, playId),
         unique_frame_id = paste0(unique_id, frameId)) %>%
  filter(event %in% c("run", "handoff", "pass_outcome_caught"))
run_plus_2_unique_frame_ids <- as.numeric(run_frames$unique_frame_id) + 2

post_run_frame_w1 <- tracking_w1 %>%
  mutate(unique_id = paste0(gameId, playId),
         unique_frame_id = paste0(unique_id, frameId)) %>%
  filter(unique_frame_id %in% run_plus_2_unique_frame_ids)


football_location_w1 <- post_run_frame_w1 %>%
  filter(club=="football") %>%
  select(unique_id,x,y) %>%
  rename("football_x" = "x",
         "football_y" = "y")

post_run_frame_w1 <- merge(post_run_frame_w1, football_location_w1, by = "unique_id")

designed_runs_and_tracking_w1 <- merge(post_run_frame_w1, designed_runs, by = "unique_id") %>%
  filter(club!="football")

designed_runs_and_tracking_w1$role <- ifelse(
  designed_runs_and_tracking_w1$club != designed_runs_and_tracking_w1$possessionTeam,
  "Defense",
  "Offense"
)


designed_runs_and_tracking_w1_defensive_players <- designed_runs_and_tracking_w1 %>%
  filter(role=="Defense")

designed_runs_and_tracking_w1_defensive_players$new_x <- ifelse(
  designed_runs_and_tracking_w1_defensive_players$playDirection == "left",
  designed_runs_and_tracking_w1_defensive_players$football_x - designed_runs_and_tracking_w1_defensive_players$x,
  designed_runs_and_tracking_w1_defensive_players$x - designed_runs_and_tracking_w1_defensive_players$football_x
)

designed_runs_and_tracking_w1_defensive_players$new_y <- ifelse(
  designed_runs_and_tracking_w1_defensive_players$playDirection == "left",
  designed_runs_and_tracking_w1_defensive_players$football_y - designed_runs_and_tracking_w1_defensive_players$y,
  designed_runs_and_tracking_w1_defensive_players$y - designed_runs_and_tracking_w1_defensive_players$football_y
)

designed_runs_and_tracking_w1_defensive_players <- designed_runs_and_tracking_w1_defensive_players %>%
  group_by(unique_id) %>%
  filter(all(new_x > -1))

plot(designed_runs_and_tracking_w1_defensive_players$new_y, designed_runs_and_tracking_w1_defensive_players$new_x)


library(ggplot2)
library(ggplot2)

library(dplyr)



designed_runs_and_tracking_w1_defensive_players_heatmap <- designed_runs_and_tracking_w1_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 1), labels = seq(1, 30, 1))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 1), labels = seq(-29.5, 29.5, 1)))

designed_runs_and_tracking_w1_defensive_players_heatmap$new_x_rounded <- as.numeric(as.character(designed_runs_and_tracking_w1_defensive_players_heatmap$new_x_rounded))

designed_runs_and_tracking_w1_defensive_players_heatmap$new_y_rounded <- as.numeric(as.character(designed_runs_and_tracking_w1_defensive_players_heatmap$new_y_rounded))

count_table <- table(designed_runs_and_tracking_w1_defensive_players_heatmap$new_y_rounded, designed_runs_and_tracking_w1_defensive_players_heatmap$new_x_rounded)

library(ggplot2)
library(ggthemes)

count_df <- as.data.frame(count_table)

colnames(count_df) <- c("new_y_rounded", "new_x_rounded", "count")
count_df$new_x_rounded <- as.numeric(as.character(count_df$new_x_rounded))

count_df$new_y_rounded <- as.numeric(as.character(count_df$new_y_rounded))

post_run_frame_defensive_player_position_heat_map_w1_plot <- ggplot(count_df, aes(x = new_y_rounded, y = new_x_rounded, fill = log(count + 1))) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(title = "Post Run Frame Defensive Player Position Heat Map",
       subtitle = "Week 1",
       x = "Line of Scrimmage",
       y = "") +
  theme_clean() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(-20, 20, by = 5), limits = c(-30, 30)) +
  scale_y_continuous(breaks = seq(min(count_df$new_x_rounded), max(count_df$new_x_rounded), by = 5))

ggsave("post_run_frame_defensive_player_position_heat_map_w1_plot.png", post_run_frame_defensive_player_position_heat_map_w1_plot)



position_ranges <- designed_runs_and_tracking_w1_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 2), labels = seq(1, 29, 2))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 2), labels = seq(-29, 29, 2))) %>%
  mutate(unique_and_nfl_id = paste0(unique_id, nflId) ) %>%
  mutate(y_dist_from_ball = abs(as.numeric(new_y))) %>%
  mutate(total_dist_from_ball = sqrt(new_x^2 + y_dist_from_ball^2))

table(position_ranges$new_x_rounded, position_ranges$new_y_rounded)

first_tackle_opportunity <- tackles_with_bc_id %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  group_by(unique_id) %>%
  summarise(nflId = first(nflId)) %>%
  filter(unique_id %in% designed_runs$unique_id) %>%
  mutate(tackle_opp_id = paste0(unique_id, nflId))

position_ranges$tackle_opp <- ifelse(
  position_ranges$unique_and_nfl_id %in% first_tackle_opportunity$tackle_opp_id,
  1,
  0)
position_ranges$tackle_opp <- as.factor(position_ranges$tackle_opp)
position_ranges$qb_position <- ifelse(
  position_ranges$offenseFormation == "SHOTGUN",
  "Shotgun",
  "Under Center"
)

# Assuming you have a dataframe named "your_data" with the relevant columns

# Load necessary libraries
library(tidyr)
library(dplyr)
library(caret)
library(glmnet)
tackle_opp <- position_ranges %>%
  ungroup() %>%
  select(unique_id, qb_position, new_x, y_dist_from_ball, tackle_opp, total_dist_from_ball, passProbability)

tackle_opp <- na.omit(tackle_opp)

# Step 2: Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(tackle_opp$tackle_opp, p = 0.7, list = FALSE)
train_data <- tackle_opp[train_index, ]
test_data <- tackle_opp[-train_index, ]

# Step 3: Train a logistic regression model
model <- glm(tackle_opp ~ . - unique_id, data = train_data, family = "binomial")

summary(model)

# library(MASS)
# step_model <- stepAIC(model, direction = "both", 
#                       trace = FALSE)
# 
# summary(step_model)

# Step 4: Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")
print(summary(predictions))

predicted_classes <- ifelse(predictions > (.1), 1, 0)
conf_matrix <- table(predicted_classes, test_data$tackle_opp)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste0("Accuracy:", accuracy))

precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste0("Precision:", precision))

# Recall
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste0("Recall:", recall))

# F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste0("F1 Score:", f1_score))

# ROC Curve and AUC
roc_curve <- pROC::roc(test_data$tackle_opp, predictions)
auc_value <- pROC::auc(roc_curve)
plot(roc_curve, main = paste0("ROC Curve - AUC =", round(auc_value, 3)))

good_ids <- tackle_opp$unique_id

position_ranges <- position_ranges %>%
  ungroup() %>%
  filter(unique_id %in% good_ids)

all_predictions <- predict(model, newdata = tackle_opp, type = "response")

all_data <- cbind(position_ranges, all_predictions)

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

tackle_over_expected <- selected_data_probabilities %>%
  group_by(nflId) %>%
  summarise(total_tackle_oe = sum(tackle_oe),
            avg_tackle_oe = mean(tackle_oe)) %>%
  arrange(desc(total_tackle_oe))

tackle_over_expected_with_names_w1 <- merge(tackle_over_expected, players, by = "nflId") %>%
  select(nflId, total_tackle_oe, avg_tackle_oe, displayName) %>%
  arrange(desc(total_tackle_oe))

write.csv(tackle_over_expected_with_names_w1, "tackle_over_expected_with_names_w1.csv")




############
tracking_w1 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_1.csv")
tracking_w2 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_2.csv")
tracking_w3 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_3.csv")
tracking_w4 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_4.csv")
tracking_w5 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_5.csv")
tracking_w6 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_6.csv")
tracking_w7 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_7.csv")
tracking_w8 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_8.csv")
tracking_w9 <- read.csv("/Users/jonahlubin/Desktop/nfl-big-data-bowl-2024/tracking_week_9.csv")

tracking_all <- rbind(tracking_w1, tracking_w2, tracking_w3, tracking_w4, tracking_w5, 
                      tracking_w6, tracking_w7, tracking_w8, tracking_w9)


##SECOND FRAME AFTER RUN ALL WEEKS

run_frames <- tracking_all %>%
  mutate(unique_id = paste0(gameId, playId),
         unique_frame_id = paste0(unique_id, frameId)) %>%
  filter(event %in% c("run", "handoff", "pass_outcome_caught"))
run_plus_2_unique_frame_ids <- as.numeric(run_frames$unique_frame_id) + 2

post_run_frame_all <- tracking_all %>%
  mutate(unique_id = paste0(gameId, playId),
         unique_frame_id = paste0(unique_id, frameId)) %>%
  filter(unique_frame_id %in% run_plus_2_unique_frame_ids)


football_location_all <- post_run_frame_all %>%
  filter(club=="football") %>%
  select(unique_id,x,y) %>%
  rename("football_x" = "x",
         "football_y" = "y")

post_run_frame_all <- merge(post_run_frame_all, football_location_all, by = "unique_id")

designed_runs_and_tracking_all <- merge(post_run_frame_all, designed_runs, by = "unique_id") %>%
  filter(club!="football")

designed_runs_and_tracking_all$role <- ifelse(
  designed_runs_and_tracking_all$club != designed_runs_and_tracking_all$possessionTeam,
  "Defense",
  "Offense"
)


designed_runs_and_tracking_all_defensive_players <- designed_runs_and_tracking_all %>%
  filter(role=="Defense")

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
  group_by(unique_id) %>%
  filter(all(new_x > -1))

plot(designed_runs_and_tracking_all_defensive_players$new_y, designed_runs_and_tracking_all_defensive_players$new_x)


library(ggplot2)
library(ggplot2)

library(dplyr)



designed_runs_and_tracking_all_defensive_players_heatmap <- designed_runs_and_tracking_all_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 1), labels = seq(1, 30, 1))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 1), labels = seq(-29.5, 29.5, 1)))

designed_runs_and_tracking_all_defensive_players_heatmap$new_x_rounded <- as.numeric(as.character(designed_runs_and_tracking_all_defensive_players_heatmap$new_x_rounded))

designed_runs_and_tracking_all_defensive_players_heatmap$new_y_rounded <- as.numeric(as.character(designed_runs_and_tracking_all_defensive_players_heatmap$new_y_rounded))

count_table <- table(designed_runs_and_tracking_all_defensive_players_heatmap$new_y_rounded, designed_runs_and_tracking_all_defensive_players_heatmap$new_x_rounded)

library(ggplot2)
library(ggthemes)

count_df <- as.data.frame(count_table)

colnames(count_df) <- c("new_y_rounded", "new_x_rounded", "count")
count_df$new_x_rounded <- as.numeric(as.character(count_df$new_x_rounded))

count_df$new_y_rounded <- as.numeric(as.character(count_df$new_y_rounded))

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



position_ranges <- designed_runs_and_tracking_all_defensive_players %>%
  mutate(new_x_rounded = cut(new_x, breaks = seq(0, 30, 2), labels = seq(1, 29, 2))) %>%
  mutate(new_y_rounded = cut(new_y, breaks = seq(-30, 30, 2), labels = seq(-29, 29, 2))) %>%
  mutate(unique_and_nfl_id = paste0(unique_id, nflId) ) %>%
  mutate(y_dist_from_ball = abs(as.numeric(new_y))) %>%
  mutate(total_dist_from_ball = sqrt(new_x^2 + y_dist_from_ball^2))

table(position_ranges$new_x_rounded, position_ranges$new_y_rounded)

first_tackle_opportunity <- tackles_with_bc_id %>%
  mutate(unique_id = paste0(gameId, playId)) %>%
  group_by(unique_id) %>%
  summarise(nflId = first(nflId)) %>%
  filter(unique_id %in% designed_runs$unique_id) %>%
  mutate(tackle_opp_id = paste0(unique_id, nflId))

position_ranges$tackle_opp <- ifelse(
  position_ranges$unique_and_nfl_id %in% first_tackle_opportunity$tackle_opp_id,
  1,
  0)
position_ranges$tackle_opp <- as.factor(position_ranges$tackle_opp)
position_ranges$qb_position <- ifelse(
  position_ranges$offenseFormation == "SHOTGUN",
  "Shotgun",
  "Under Center"
)

# Assuming you have a dataframe named "your_data" with the relevant columns

# Load necessary libraries
library(tidyr)
library(dplyr)
library(caret)
library(glmnet)
tackle_opp <- position_ranges %>%
  ungroup() %>%
  select(unique_id, qb_position, new_x, y_dist_from_ball, tackle_opp, total_dist_from_ball, passProbability)

tackle_opp <- na.omit(tackle_opp)

# Step 2: Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(tackle_opp$tackle_opp, p = 0.7, list = FALSE)
train_data <- tackle_opp[train_index, ]
test_data <- tackle_opp[-train_index, ]

# Step 3: Train a logistic regression model
model <- glm(tackle_opp ~ . - unique_id, data = train_data, family = "binomial")

summary(model)

# library(MASS)
# step_model <- stepAIC(model, direction = "both", 
#                       trace = FALSE)
# 
# summary(step_model)

# Step 4: Make predictions on the test set
predictions <- predict(model, newdata = test_data, type = "response")
print(summary(predictions))

predicted_classes <- ifelse(predictions > (.1), 1, 0)
conf_matrix <- table(predicted_classes, test_data$tackle_opp)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste0("Accuracy:", accuracy))

precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(paste0("Precision:", precision))

# Recall
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(paste0("Recall:", recall))

# F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste0("F1 Score:", f1_score))

# ROC Curve and AUC
roc_curve <- pROC::roc(test_data$tackle_opp, predictions)
auc_value <- pROC::auc(roc_curve)
plot(roc_curve, main = paste0("ROC Curve - AUC =", round(auc_value, 3)))
dev.copy(png, "model_roc_curve_plot.png")
dev.off()

good_ids <- tackle_opp$unique_id

position_ranges <- position_ranges %>%
  ungroup() %>%
  filter(unique_id %in% good_ids)

all_predictions <- predict(model, newdata = tackle_opp, type = "response")

all_data <- cbind(position_ranges, all_predictions)

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

tackle_over_expected <- selected_data_probabilities %>%
  group_by(nflId) %>%
  summarise(total_tackle_oe = sum(tackle_oe),
            avg_tackle_oe = mean(tackle_oe),
            opportunities = n()) %>%
  arrange(desc(total_tackle_oe))

tackle_over_expected_with_names <- merge(tackle_over_expected, players, by = "nflId") %>%
  select(nflId, total_tackle_oe, avg_tackle_oe, opportunities, displayName) %>%
  arrange(desc(total_tackle_oe))

write.csv(tackle_over_expected_with_names, "tackle_over_expected_with_names.csv")

