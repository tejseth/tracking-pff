library(rdtools)
library(tidyverse)
library(ggthemes)
library(nflfastR)
library(mltools)
library(data.table)
library(xgboost)
library(caret)
library(vip)
library(SHAPforxgboost)
library(ggimage)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

trace(pull_ngs,edit=TRUE)
rushing_data <<- pull_s3(paste0("analytics/projections/by_facet/", 'nfl', "/%i/rushing.csv.gz"), season_start = 2017, season_end = 2020)
tracking_run_2020_1 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 1, wk_end = 1, run_pass_all = "r")
tracking_run_2020_2 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 2, wk_end = 2, run_pass_all = "r")
tracking_run_2020_3 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 3, wk_end = 3, run_pass_all = "r")
tracking_run_2020_4 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 4, wk_end = 4, run_pass_all = "r")
tracking_run_2020_5 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 5, wk_end = 5, run_pass_all = "r")
tracking_run_2020_6 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 6, wk_end = 6, run_pass_all = "r")
tracking_run_2020_7 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 7, wk_end = 7, run_pass_all = "r")
tracking_run_2020_8 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 8, wk_end = 8, run_pass_all = "r")
tracking_run_2020_9 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 9, wk_end = 9, run_pass_all = "r")
tracking_run_2020_10 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 10, wk_end = 10, run_pass_all = "r")
tracking_run_2020_11 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 11, wk_end = 11, run_pass_all = "r")
tracking_run_2020_12 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 12, wk_end = 12, run_pass_all = "r")
tracking_run_2020_13 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 13, wk_end = 13, run_pass_all = "r")
tracking_run_2020_14 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 14, wk_end = 14, run_pass_all = "r")
tracking_run_2020_15 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 15, wk_end = 15, run_pass_all = "r")
tracking_run_2020_16 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 16, wk_end = 16, run_pass_all = "r")
tracking_run_2020_17 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 17, wk_end = 17, run_pass_all = "r")

# tracking_run_2020 <- do.call("rbind", list(tracking_run_2020_1, tracking_run_2020_2, tracking_run_2020_3,
#                                            tracking_run_2020_4, tracking_run_2020_5, tracking_run_2020_6,
#                                            tracking_run_2020_7, tracking_run_2020_8, tracking_run_2020_9,
#                                            tracking_run_2020_10, tracking_run_2020_11, tracking_run_2020_12,
#                                            tracking_run_2020_13, tracking_run_2020_14, tracking_run_2020_15, 
#                                            tracking_run_2020_16, tracking_run_2020_17))

rushing_data <- rushing_data %>%
  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>%
  ungroup()

rushing_data <- rushing_data %>%
  mutate(defense = case_when(
    defense == "SD" ~ "LAC",
    defense == "BLT" ~ "BAL",
    defense == "OAK" ~ "LV",
    defense == "HST" ~ "HOU",
    defense == "SL" ~ "LA",
    defense == "CLV" ~ "CLE", 
    defense == "ARZ" ~ "ARI",
    TRUE ~ defense
  )) %>%
  ungroup()

rushing_data <- rushing_data %>%
  mutate(yards_before_contact = yards - yards_after_contact)

rushers <- rushing_data %>%
  dplyr::select(game_id, play_id, player, yards_before_contact)

play_speed_all %>%
  ggplot(aes(x = avg_speed, y = ybc)) +
  geom_jitter(alpha = 0.5, size = 2) +
  geom_smooth(size = 2, color = "darkorange") +
  theme_reach() +
  labs(x = "Average Speed",
       y = "Yards Before Contact",
       title = "Higher Speed Before Contact Leads to Higher Yards Before Contact On a Play",
       subtitle = "2020, Weeks 1-17") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  annotate("text", x = 3, y = 50, label = "R^2 = 0.15", size = 5)

play_speed_all %>%
  ggplot(aes(x = avg_speed, y = ybc)) +
  geom_jitter(alpha = 0.5, size = 2) +
  geom_smooth(size = 2, color = "darkgreen") +
  theme_reach() +
  labs(x = "Seconds Before Contact",
       y = "Yards Before Contact",
       title = "Higher Seconds Before Contact Leads to Higher Yards Before Contact On a Play",
       subtitle = "2020, Weeks 1-17") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  annotate("text", x = 3, y = 50, label = "R^2 = 0.37", size = 5)

summary(lm(ybc ~ avg_speed, data = play_speed_all))$r.squared #0.15
summary(lm(ybc ~ seconds_before_contact, data = play_speed_all))$r.squared #0.37
summary(lm(ybc ~ (avg_speed + seconds_before_contact)^2, data = play_speed_all))

rushing_data_speed <- rushing_data %>%
  dplyr::select(game_id, play_id, offense, defense, season, week, down, distance, player_id,
                position, quarter, seconds_left_in_quarter, concept_1, concept_2, run_position,
                intended_run_position, run_direction, box_players, rpo, yards,
                yards_after_contact, avoided_tackles, yards_to_go) %>%
  left_join(play_speed_all, by = c("game_id" = "gameid", "play_id" = "playid")) %>%
  filter(!is.na(avg_speed)) %>%
  dplyr::filter(grepl("HB",position))

rusher_speed <- rushing_data_speed %>%
  group_by(player, offense) %>%
  summarize(rushes = n(),
            avg_speed = mean(avg_speed, na.rm = T),
            avg_seconds_bc = mean(seconds_before_contact, na.rm = T),
            avg_ybc = mean(ybc, na.rm = T)) %>%
  filter(rushes >= 100) %>% 
  arrange(-avg_speed) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

rusher_speed %>%
  ggplot(aes(x = avg_speed, y = avg_ybc)) +
  geom_smooth(method = "lm", size = 1.5, color = "black") +
  geom_hline(yintercept = mean(rusher_speed$avg_ybc), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(rusher_speed$avg_speed), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(fill = team_color, color = team_color2, size = rushes), shape = 21, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = player), size = 5, box.padding = 0.3) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Average Speed Before Contact",
       y = "Yards Before Contact",
       title = "Average Speed and Yards Before Contact, 2020",
       subtitle = "Average speed before contract determined by speed from handoff to first contact") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
ggsave('2020-speed.png', width = 15, height = 10, dpi = "retina")

rushing_data_speed$avoided_tackles[is.na(rushing_data_speed$avoided_tackles)] <- 0

rushing_data_speed <- rushing_data_speed %>%
  dplyr::select(-...1)

colSums(is.na(rushing_data_speed))

speed_down_distance <- rushing_data_speed %>%
  filter(distance < 15) %>%
  filter(down %in% c(1, 2, 3)) %>%
  group_by(down, distance) %>%
  summarize(rushes = n(),
            avg_speed = mean(avg_speed))

speed_down_distance %>%
  filter(avg_speed > 4 & avg_speed < 6) %>%
  mutate(Down = as.factor(down)) %>%
  ggplot(aes(x = distance, y = avg_speed, color = Down)) +
  geom_point(aes(size = rushes), alpha = 0.4) +
  geom_smooth(aes(color = Down), se = FALSE, size = 2) +
  scale_color_viridis_d() +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(legend.position = "bottom") +
  labs(x = "Yards to Sticks",
       y = "Average Speed",
       title = "How Down and Distance Impacts Running Back Speed",
       subtitle = "2020, speed is determined by handoff until first contact") +
  guides(size = FALSE)

speed_yardline_box <- rushing_data_speed %>%
  filter(box_players %in% c(5, 6, 7, 8)) %>%
  mutate(yards_bin = round(yards_to_go / 5) * 5) %>%
  group_by(yards_bin, box_players) %>%
  summarize(rushes = n(),
            avg_speed = mean(avg_speed))

speed_yardline_box$box_players <- factor(speed_yardline_box$box_players, levels = c(5, 6, 7, 8))

speed_yardline_box %>%
  filter(avg_speed < 5.8) %>%
  mutate(`Box Defenders` = as.factor(box_players)) %>%
  ggplot(aes(x = yards_bin, y = avg_speed, color = `Box Defenders`)) +
  geom_point(aes(size = rushes), alpha = 0.4) +
  geom_smooth(aes(color = `Box Defenders`), se = FALSE, size = 2) +
  scale_color_viridis_d() +
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(legend.position = "bottom") +
  labs(x = "Yards From Endzone",
       y = "Average Speed",
       title = "How Yardline and Box Defenders Impacts Running Back Speed",
       subtitle = "2020, speed is determined by handoff until first contact") +
  guides(size = FALSE)

rushing_data_speed %>%
  filter(concept_1 %in% c("Outside Zone", "Inside Zone", "Man", "Power", "Pull Lead", "Counter", "Draw", "Trap")) %>%
  group_by(concept_1) %>%
  mutate(speed = mean(avg_speed)) %>%
  ungroup() %>%
  mutate(concept_1 = fct_reorder(concept_1, speed)) %>%
  ggplot(aes(x = concept_1, y = avg_speed)) +
  geom_violin(aes(color = concept_1)) +
  geom_boxplot(aes(fill = concept_1)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_reach() +
  labs(x = "Run Concept",
       y = "Speed Before Contact",
       title = "How Run Concept Affects a Running Back's Speed Through the Hole",
       subtitle = "2020, eight most common run types selected") 
ggsave('speed-run-concept.png', width = 15, height = 10, dpi = "retina")

rushing_data_speed <- rushing_data_speed %>%
  mutate(seconds_left_in_half = case_when(
    quarter == 1 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 2 ~ as.integer(seconds_left_in_quarter),
    quarter == 3 ~ as.integer(seconds_left_in_quarter + 900),
    quarter == 4 ~ as.integer(seconds_left_in_quarter),
    TRUE ~ as.integer(0)
  )) %>%
  dplyr::select(-seconds_left_in_quarter)

colSums(is.na(rushing_data_speed))

rushing_data_speed %>% group_by(concept_1) %>% tally(sort = T)

others <- c("FB Run", "Undefined", "Trick")

rushing_data_speed <- rushing_data_speed %>%
  mutate(concept_1 = ifelse(concept_1 %in% others, "Other", concept_1))

speed_data_select <- rushing_data_speed %>%
  dplyr::select(down, distance, seconds_left_in_half, box_players, 
                concept_1, yards_to_go, avg_speed) %>%
  mutate(label = avg_speed) %>%
  dplyr::select(-avg_speed) %>%
  mutate(concept_1 = as.factor(concept_1)) %>%
  mutate(down = as.factor(down)) %>%
  dplyr::select(label, everything())

trsf <- one_hot(as.data.table(speed_data_select))

colSums(is.na(trsf))

smp_size <- floor(0.50 * nrow(trsf))
set.seed(2011) #go lions
ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- as.matrix(trsf[ind, ])
test <- as.matrix(trsf[-ind, ])

dim(train)

speed_mod <-
  xgboost(
    data = train[, 2:19],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )   

pred_xgb <- predict(speed_mod, test[, 2:19])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y) #RMSE = 1.19

hyper_grid <- expand.grid(max_depth = seq(1, 6, 1),
                          eta = seq(.15, .3, .01))

xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:19],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j]
  )
  
  xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}

#ideal hyperparamters
hyper_grid[which.min(xgb_test_rmse), ]

speed_model <-
  xgboost(
    data = train[, 2:19],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 2, #ideal max depth
    eta = 0.3 #ideal eta
  )   

vip(speed_model, num_features = 18) +
  theme_reach()

shap_values <- shap.values(xgb_model = speed_model, X_train = train[, 2:19])
shape_values_mean <- as.data.frame(shap_values$mean_shap_score)
shap_long <- shap.prep(xgb_model = speed_model, X_train = train[, 2:19])
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train[, 2:19])
shap.plot.summary(shap_long)

pred_xgb <- predict(speed_model, test[, 2:19])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y) #RMSE = 1.07

speed_preds <- as.data.frame(
  matrix(predict(speed_model, as.matrix(trsf %>% select(-label))))
) %>%
  dplyr::rename(exp_speed = V1)

speed_projs <- cbind(rushing_data_speed, speed_preds)

speed_projs <- speed_projs %>%
  mutate(speed_oe = avg_speed - exp_speed)

summary(lm(ybc ~ speed_oe, data = speed_projs))$r.squared #0.18
summary(lm(ybc ~ exp_speed, data = speed_projs))$r.squared #0.00
summary(lm(ybc ~ avg_speed, data = speed_projs))$r.squared #0.16

speed_oe_stats <- speed_projs %>%
  group_by(player, offense) %>%
  summarize(rushes = n(), 
            avg_speed_oe = mean(speed_oe),
            avg_ybc = mean(ybc)) %>%
  filter(rushes >= 100) %>%
  arrange(-avg_speed_oe) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

speed_oe_stats %>%
  ggplot(aes(x = avg_speed_oe, y = avg_ybc)) +
  geom_smooth(method = "lm", size = 1.5, color = "black", se = FALSE) +
  geom_hline(yintercept = mean(speed_oe_stats$avg_ybc), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(speed_oe_stats$avg_speed_oe), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(fill = team_color, color = team_color2, size = rushes), shape = 21, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = player), size = 5, box.padding = 0.3) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = "Speed Over Expected",
       y = "Yards Before Contact",
       title = "How Speed Over Expected Impacts Yards Before Contact, 2020",
       subtitle = "Minimum of 100 rushes") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
ggsave('2020-speed-oe.png', width = 15, height = 10, dpi = "retina")

team_speed <- speed_projs %>%
  group_by(offense) %>%
  summarize(avg_exp_speed = mean(exp_speed),
            avg_speed = mean(avg_speed),
            avg_speed_oe = avg_speed - avg_exp_speed,
            avg_ybc = mean(ybc)) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

team_speed %>%
  ggplot(aes(x = avg_exp_speed, y = avg_speed)) +
  geom_smooth(method = "lm", size = 1.5, color = "black", se = FALSE) +
  geom_hline(yintercept = mean(team_speed$avg_speed), alpha = 0.5, linetype = "dashed") +
  geom_vline(xintercept = mean(team_speed$avg_exp_speed), alpha = 0.5, linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_reach() +
  labs(x = "Average Expected Speed",
       y = "Average Actual Speed",
       title = "Actual and Expected Speed on Rushing Plays, 2020",
       subtitle = "Rushing speed determined by a running back's speed from handoff to first contact") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
 

