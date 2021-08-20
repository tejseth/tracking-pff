play_speed_all_18 <- read.csv("~/tracking-pff/play-speed-year/play_speed_all_18.csv")
play_speed_all_19 <- read.csv("~/tracking-pff/play-speed-year/play_speed_all_19.csv")
play_speed_all_20 <- read.csv("~/tracking-pff/play-speed-year/play_speed_all_20.csv")
run_blocking <- pull_s3(paste0("analytics/projections/by_facet/", 'nfl', "/%i/run_blocking.csv.gz"), season_start = 2018, season_end = 2020)
scheme_data <<- pull_s3("analytics/projections/nfl/%i/scheme.csv.gz", season_start = 2018, season_end = 2020)
speed_projs <- read.csv("~/tracking-pff/speed_projs.csv")
stadiums <- pull_s3("flat_files/NFL_Stadiums.csv", bucket = "ml")

play_speed_all <- rbind(play_speed_all_18, play_speed_all_19, play_speed_all_20)

play_speed_all <- play_speed_all %>%
  dplyr::select(gameid, playid, player, avg_speed, seconds_before_contact, ybc)

stadiums_select <- stadiums %>%
  dplyr::select(franchise_id, season, dome, turf) %>%
  mutate(dome = as.factor(dome),
         turf = as.factor(turf))

rushing_data_speed <- rushing_data %>%
  dplyr::select(game_id, play_id, home_franchise_id, offense, defense, season, week, down, distance, player_id,
                position, quarter, seconds_left_in_quarter, concept_1, concept_2, run_position,
                intended_run_position, run_direction, box_players, rpo, yards,
                yards_after_contact, avoided_tackles, yards_to_go) %>%
  left_join(play_speed_all, by = c("game_id" = "gameid", "play_id" = "playid")) %>%
  left_join(stadiums_select, by = c("home_franchise_id" = "franchise_id", "season")) %>%
  filter(!is.na(avg_speed)) %>%
  dplyr::filter(grepl("HB",position))

rushing_data_speed$avoided_tackles[is.na(rushing_data_speed$avoided_tackles)] <- 0

colSums(is.na(rushing_data_speed))

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

speed_lmer <- lmer(avg_speed ~ down + distance + seconds_left_in_half + 
                     box_players + yards_to_go + turf + dome +
                     (1|player) + (1|home_franchise_id), data = rushing_data_speed)
summary(speed_lmer)

speed_effects <- REsim(speed_lmer)
speed_effects <- speed_effects %>%
  filter(groupFctr == "home_franchise_id") %>%
  dplyr::select(home_franchise_id = groupID, mean)

speed_effects$home_franchise_id <- as.integer(speed_effects$home_franchise_id)
rushing_data_speed$home_franchise_id <- as.integer(rushing_data_speed$home_franchise_id)

rushing_data_speed <- rushing_data_speed %>%
  left_join(speed_effects, by = c("home_franchise_id")) %>%
  mutate(avg_speed = avg_speed - mean)

blocks_sums <- run_blocking %>%
  filter(!is.na(run_blocking_grade)) %>%
  group_by(game_id, play_id) %>%
  summarize(total_blocks = n(),
            pos_blocks = sum(run_blocking_grade > 0),
            neg_blocks = sum(run_blocking_grade < 0))

rushing_data_speed <- rushing_data_speed %>%
  left_join(blocks_sums, by = c("game_id", "play_id"))

box_advantage <- scheme_data %>%
  dplyr::select(game_id, play_id, shotgun, offense_personnel, box_players) %>%
  mutate(num_te = substring(offense_personnel, 7, 7),
         num_rb = substring(offense_personnel, 5, 5),
         num_fb = substring(offense_personnel, 3, 3),
         num_te = ifelse(num_te > 0, num_te, 0),
         num_rb = ifelse(num_rb > 0, num_rb, 0),
         num_fb = ifelse(num_fb > 0, num_fb, 0),
         extra_oline = case_when(
           substring(offense_personnel, 10, 11) == "*)" ~ 1,
           substring(offense_personnel, 10, 11) == "**" ~ 2,
          TRUE ~ 0)) %>%
  filter(num_te != "X") %>%
  filter(num_rb != "e") %>%
  mutate(off_box_players = as.numeric(num_te) + as.numeric(num_rb) + as.numeric(num_fb) + extra_oline + 5,
         off_box_advantage = off_box_players - box_players) %>%
  dplyr::select(game_id, play_id, shotgun, off_box_advantage)

rushing_data_speed <- rushing_data_speed %>%
  left_join(box_advantage, by = c("game_id", "play_id"))

rushing_data_speed <- rushing_data_speed %>%
  filter(!is.na(total_blocks)) %>%
  filter(!is.na(off_box_advantage))

colSums(is.na(rushing_data_speed))

speed_data_select <- rushing_data_speed %>%
  dplyr::select(down, distance, seconds_left_in_half, off_box_advantage, 
                concept_1, yards_to_go, avg_speed, turf, dome, shotgun,
                total_blocks, pos_blocks, neg_blocks) %>%
  mutate(label = avg_speed) %>%
  dplyr::select(-avg_speed) %>%
  mutate(concept_1 = as.factor(concept_1)) %>%
  mutate(down = as.factor(down),
         shotgun = as.factor(shotgun)) %>%
  dplyr::select(label, everything())

trsf <- one_hot(as.data.table(speed_data_select))

colSums(is.na(trsf))

smp_size <- floor(0.50 * nrow(trsf))
set.seed(2016) #go lions
ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- as.matrix(trsf[ind, ])
test <- as.matrix(trsf[-ind, ])

dim(train)

speed_mod <-
  xgboost(
    data = train[, 2:28],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )   

pred_xgb <- predict(speed_mod, test[, 2:28])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y) #RMSE = 1.08

hyper_grid <- expand.grid(max_depth = seq(1, 6, 1),
                          eta = seq(.15, .3, .01))

xgb_train_rmse <- NULL
xgb_test_rmse <- NULL

for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    data = train[, 2:28],
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
    data = train[, 2:28],
    label = train[, 1],
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 2, #ideal max depth
    eta = 0.17 #ideal eta
  )   

vip(speed_model, num_features = 27) +
  theme_reach()

shap_values <- shap.values(xgb_model = speed_model, X_train = train[, 2:28])
shape_values_mean <- as.data.frame(shap_values$mean_shap_score)
shap_long <- shap.prep(xgb_model = speed_model, X_train = train[, 2:28])
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = train[, 2:28])
shap.plot.summary(shap_long)

pred_xgb <- predict(speed_model, test[, 2:28])

yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y) #RMSE = 0.96

speed_preds <- as.data.frame(
  matrix(predict(speed_model, as.matrix(trsf %>% dplyr::select(-label))))
) %>%
  dplyr::rename(exp_speed = V1)

speed_projs <- cbind(rushing_data_speed, speed_preds)

speed_projs <- speed_projs %>%
  mutate(speed_oe = avg_speed - exp_speed)

write.csv(speed_projs, "speed_projs.csv")


