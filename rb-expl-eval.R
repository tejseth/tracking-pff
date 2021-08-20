speed_projs <- read.csv("~/tracking-pff/speed_projs.csv")

speed_projs_filtered <- speed_projs %>%
  filter(seconds_before_contact >= 0.5 & seconds_before_contact <= 5.7)

summary(lm(ybc ~ speed_oe, data = speed_projs_filtered))$r.squared #0.11
summary(lm(ybc ~ exp_speed, data = speed_projs_filtered))$r.squared #0.00
summary(lm(ybc ~ avg_speed, data = speed_projs_filtered))$r.squared #0.15

speed_season_stats <- speed_projs_filtered %>%
  group_by(player, season, offense) %>%
  summarize(rushes = n(),
            avg_ybc = mean(ybc),
            avg_yac = mean(yards_after_contact),
            exp_speed = mean(exp_speed),
            avg_speed = mean(avg_speed),
            avg_ssoe = mean(speed_oe)) %>%
  arrange(season) %>%
  group_by(player) %>%
  mutate(next_rushes = lead(rushes),
         next_ybc = lead(avg_ybc),
         next_yac = mean(avg_yac),
         next_exp_speed = lead(exp_speed),
         next_avg_speed = lead(avg_speed),
         next_ssoe = lead(avg_ssoe)) %>%
  filter(rushes >= 100) %>%
  filter(next_rushes >= 100 | is.na(next_rushes))

summary(lm(avg_ybc ~ avg_ssoe, data = speed_season_stats, weights = rushes))$r.squared #0.20
summary(lm(avg_yac ~ avg_ssoe, data = speed_season_stats, weights = rushes))$r.squared #0.01
summary(lm(next_ybc ~ avg_ybc, data = speed_season_stats))$r.squared #0.09
summary(lm(next_yac ~ avg_yac, data = speed_season_stats))$r.squared #0.55

rb_colors <- rushing_data %>%
  group_by(player, offense) %>%
  summarize(plays = n()) %>%
  arrange(-plays) %>%
  group_by(player) %>%
  top_n(n = 1) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

speed_oe_stats <- speed_projs_filtered %>%
  group_by(player) %>%
  summarize(rushes = n(), 
            avg_speed_oe = mean(speed_oe),
            avg_ybc = mean(ybc)) %>%
  filter(rushes >= 220) %>%
  arrange(-avg_speed_oe) %>%
  left_join(rb_colors, by = c("player"))

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
       title = "How Speed Over Expected Impacts Yards Before Contact, 2018-2020",
       subtitle = "Minimum of 250 rushes") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
ggsave('2020-speed-oe.png', width = 15, height = 10, dpi = "retina")

team_speed <- speed_projs_filtered %>%
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
ggsave('team-speed.png', width = 15, height = 10, dpi = "retina")

game_speed <- speed_projs_filtered %>%
  group_by(player, offense, defense, season, week, game_id) %>%
  summarize(game_rushes = n(),
            game_exp_speed = mean(exp_speed),
            game_avg_speed = mean(avg_speed),
            game_avg_speed_oe = mean(speed_oe)) %>%
  arrange(season, week) %>%
  group_by(player, season) %>%
  mutate(next_rushes = lead(game_rushes),
         next_exp_speed = lead(game_exp_speed),
         next_avg_speed = lead(game_avg_speed),
         next_avg_speed_oe = lead(game_avg_speed_oe)) %>%
  filter(game_rushes >= 10) %>%
  filter(next_rushes >= 10) %>%
  left_join(teams_logos_select, by = c("defense" = "team_abbr")) %>%
  dplyr::select(-team_color, defense_logo = team_logo_espn) %>%
  left_join(teams_logos_select, by = c("offense" = "team_abbr")) %>%
  dplyr::select(-team_logo_espn)

summary(lm(next_rushes ~ game_rushes, data = game_speed))$r.squared #0.06
summary(lm(next_exp_speed ~ game_exp_speed, data = game_speed))$r.squared #0.15
summary(lm(next_avg_speed ~ game_avg_speed, data = game_speed))$r.squared #0.23
summary(lm(next_avg_speed_oe ~ game_avg_speed_oe, data = game_speed))$r.squared #0.22

combine_data <- pull_api("/v1/player_combine_results")$player_combine_results
pro_day_data <- pull_api("/v1/player_pro_day")$player_pro_day

combine_combine <- function(combine_data.frame, pro_day_data.frame) {
  combine <- combine_data.frame %>% dplyr::select(season = year, player_id, position = projected_position,
                                           height = height_in_inches, weight = weight_in_pounds,
                                           arm = arm_length_in_inches, right_hand = right_hand_size_in_inches,
                                           left_hand = left_hand_size_in_inches, wing = wingspan_in_inches,
                                           forty = fourty_time_in_seconds, twenty = twenty_time_in_seconds,
                                           ten = ten_time_in_seconds, bench = bench_press_in_reps,
                                           vertical = vertical_jump_in_inches, broad = broad_jump_in_inches,
                                           shuttle = twenty_shuttle_in_seconds, cone = three_cone_in_seconds)
  pro_day <- pro_day_data.frame %>% dplyr::select(season = year, player_id, position_pd = projected_position,
                                           height_pd = height_in_inches, weight_pd = weight_in_pounds,
                                           arm_pd = arm_length_in_inches, right_hand_pd = right_hand_size_in_inches,
                                           left_hand_pd = left_hand_size_in_inches, wing_pd = wingspan_in_inches,
                                           forty_pd = fourty_time_in_seconds, twenty_pd = twenty_time_in_seconds,
                                           ten_pd = ten_time_in_seconds, bench_pd = bench_press_in_reps,
                                           vertical_pd = vertical_jump_in_inches, broad_pd = broad_jump_in_inches,
                                           shuttle_pd = twenty_shuttle_in_seconds, cone_pd = three_cone_in_seconds)
  all_data <- full_join(combine, pro_day, by = c("season", "player_id"))
  height_lm <- lm(height ~ height_pd, all_data)
  weight_lm <- lm(weight ~ weight_pd, all_data)
  arm_lm <- lm(arm ~ arm_pd, all_data)
  right_hand_lm <- lm(right_hand ~ right_hand_pd, all_data)
  #left_hand_lm <- lm(left_hand ~ left_hand_pd, all_data) #Not really worth doing
  wing_lm <- lm(wing ~ wing_pd, all_data)
  forty_lm <- lm(forty ~ forty_pd, all_data)
  twenty_lm <- lm(twenty ~ twenty_pd, all_data)
  ten_lm <- lm(ten ~ ten_pd, all_data)
  bench_lm <- lm(bench ~ bench_pd, all_data)
  vertical_lm <- lm(vertical ~ vertical_pd, all_data)
  broad_lm <- lm(broad ~ broad_pd, all_data)
  shuttle_lm <- lm(shuttle ~ shuttle_pd, all_data)
  cone_lm <- lm(cone ~ cone_pd, all_data)
  all_data <- all_data %>%
    mutate(height_pd = round(predict(height_lm, all_data), 3),
           weight_pd = round(predict(weight_lm, all_data), 0),
           arm_pd = round(predict(arm_lm, all_data), 3),
           right_hand_pd = round(predict(right_hand_lm, all_data), 3),
           wing_pd = round(predict(wing_lm, all_data), 3),
           forty_pd = round(predict(forty_lm, all_data), 2),
           twenty_pd = round(predict(twenty_lm, all_data),2),
           ten_pd = round(predict(ten_lm, all_data), 2),
           bench_pd = round(predict(bench_lm, all_data), 0),
           vertical_pd = round(predict(vertical_lm, all_data), 1),
           broad_pd = round(predict(broad_lm, all_data), 0),
           shuttle_pd = round(predict(shuttle_lm, all_data), 2),
           cone_pd = round(predict(cone_lm, all_data), 2))
  all_data <- all_data %>%
    mutate(position = ifelse(is.na(position), position_pd, position),
           height = ifelse(is.na(height), height_pd, height), weight = ifelse(is.na(weight), weight_pd, weight),
           arm = ifelse(is.na(arm), arm_pd, arm), right_hand = ifelse(is.na(right_hand), right_hand_pd, right_hand),
           left_hand = ifelse(is.na(left_hand), left_hand_pd, left_hand), wing = ifelse(is.na(wing), wing_pd, wing),
           forty = ifelse(is.na(forty), forty_pd, forty), twenty = ifelse(is.na(twenty), twenty_pd, twenty),
           ten = ifelse(is.na(ten), ten_pd, ten), bench = ifelse(is.na(bench), bench_pd, bench),
           vertical = ifelse(is.na(vertical), vertical_pd, vertical), broad = ifelse(is.na(broad), broad_pd, broad),
           shuttle = ifelse(is.na(shuttle), shuttle_pd, shuttle), cone = ifelse(is.na(cone), cone_pd, cone)) %>%
    dplyr::select(season, player_id, position, height, weight, arm, hand = right_hand, wing, forty,
           twenty, ten, bench, vertical, broad, shuttle, cone)
  return(all_data)
}

combine_all_results <- combine_combine(combine_data, pro_day_data)

combine_select <- combine_all_results %>%
  dplyr::select(player_id, forty, combine_season = season, twenty, ten, combine_weight = weight, position) %>%
  mutate(speed_score = forty / combine_weight)

season_speed <- speed_projs_filtered %>%
  group_by(player, player_id, season, offense) %>%
  summarize(rushes = n(),
            exp_speed = mean(exp_speed),
            avg_speed = mean(avg_speed),
            avg_ssoe = mean(speed_oe)) %>%
  filter(rushes >= 50)

season_speed <- season_speed %>%
  left_join(combine_select, by = c("player_id"))

ncaa_ryoe_projs <- read_csv("~/RYOE/ncaa_ryoe_projs.csv")
nfl_ryoe_projs <- read.csv("~/in-season/in-season-data/nfl_ryoe_projs.csv")
ncaa_teams_colors_logos <- read.csv("~/Desktop/ncaa_teams_colors_logos.csv")

nfl_ryoe_stats <- nfl_ryoe_projs %>%
  group_by(player, player_id) %>%
  summarize(nfl_rushes = n(),
            nfl_ryoe = mean(ryoe),
            nfl_expl_rate = mean(ryoe >= 10),
            nfl_bad_rate = mean(ryoe <= -5))

ryoe_40_stats <- ncaa_ryoe_projs %>%
  group_by(player, player_id, offense) %>%
  summarize(ncaa_rushes = n(),
            ncaa_ryoe = mean(ryoe),
            ncaa_expl_rate = mean(ryoe >= 10),
            ncaa_bad_rate = mean(ryoe <= -5)) %>%
  filter(ncaa_rushes >= 200) %>%
  left_join(combine_select, by = c("player_id")) %>%
  filter(!is.na(forty)) %>%
  filter(position == "HB") %>%
  left_join(nfl_ryoe_stats, by = c("player", "player_id")) %>%
  left_join(ncaa_teams_colors_logos, by = c("offense" = "team")) %>%
  mutate(flying_20 = forty - twenty)

summary(lm(forty ~ (ncaa_expl_rate + combine_weight)^2, data = ryoe_40_stats, weights = ncaa_rushes))$r.squared #0.22
summary(lm(twenty ~ (ncaa_expl_rate + combine_weight)^2, data = ryoe_40_stats, weights = ncaa_rushes))$r.squared #0.30
summary(lm(ten ~ ncaa_expl_rate, data = ryoe_40_stats, weights = ncaa_rushes))$r.squared #0.13
summary(lm(forty ~ ncaa_ryoe, data = ryoe_40_stats, weights = ncaa_rushes))$r.squared #0.15
summary(lm(forty ~ ncaa_bad_rate, data = ryoe_40_stats, weights = ncaa_rushes))$r.squared #0.04

summary(lm(nfl_ryoe ~ forty, data = ryoe_40_stats, weights = nfl_rushes))$r.squared #0.04
summary(lm(nfl_expl_rate ~ forty, data = ryoe_40_stats, weights = nfl_rushes))$r.squared #0.03
summary(lm(ncaa_bad_rate ~ forty, data = ryoe_40_stats, weights = nfl_rushes))$r.squared #0.10

ryoe_40_stats %>%
  filter(nfl_rushes >= 150) %>%
  ggplot(aes(x = ncaa_expl_rate, y = twenty)) +
  geom_smooth(method = "lm", size = 2, se = FALSE, color = "black") +
  geom_hline(yintercept = mean(ryoe_40_stats$twenty, na.rm = T) - 0.018, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(ryoe_40_stats$ncaa_expl_rate, na.rm = T), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(fill = color, color = alt_color, size = ncaa_rushes), shape = 21, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = player), size = 4.5, box.padding = 0.35) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  scale_y_reverse() +
  scale_x_continuous(limits = c(0.025, 0.125)) +
  labs(x = "College Explosive Run Rate (RYOE >= 10)",
       y = "Twenty-Yard Split",
       title = "Explosive Run Rate Predicts a Running Back's Twenty-Yard Combine Split Well",
       subtitle = "2014-2020, bubble size is amount of rushes in college",
       caption = "By Tej Seth | @tejfbanalytics | PFF") +
  annotate("text", x = 0.035, 2.57, label = "R^2 = 0.22", size = 5)
ggsave('college-20.png', width = 15, height = 10, dpi = "retina")
  

ncaa_40 <- ryoe_40_stats %>%
  dplyr::select(starts_with('ncaa'), forty) %>%
  rename(`NCAA Rushes` = ncaa_rushes, `NCAA RYOE` = ncaa_ryoe, `NCAA Explosive Rate` = ncaa_expl_rate, `NCAA Bad Rush Rate` = ncaa_bad_rate) %>%
  pivot_longer(cols = starts_with('ncaa'))

ncaa_40 %>%
  ggplot(aes(x = value, y = forty)) +
  geom_point(aes(fill = name), color = "black", shape = 21, alpha = 0.8, size = 4) +
  geom_smooth(size = 2, color = "black", se = FALSE, method = "lm") +
  scale_fill_viridis_d() +
  theme_reach() +
  scale_y_reverse() +
  facet_wrap(~name, scales = "free_x") +
  labs(x = "Value",
       y = "Forty",
       title = "Collge Explosive Rate and RYOE Best Predict 40 Time",
       subtitle = "2014-2020, minimum of 200 college rushes")

nfl_40 <- ryoe_40_stats %>%
  filter(nfl_rushes >= 100) %>%
  dplyr::select(starts_with('nfl'), forty) %>%
  rename(`NFL Rushes` = nfl_rushes, `NFL RYOE` = nfl_ryoe, `NFL Explosive Rate` = nfl_expl_rate, `NFL Bad Rush Rate` = nfl_bad_rate) %>%
  pivot_longer(cols = starts_with('nfl'))

nfl_40 %>%
  ggplot(aes(x = forty, y = value)) +
  geom_point(aes(fill = name), color = "black", shape = 21, alpha = 0.8, size = 4) +
  geom_smooth(size = 2, color = "black", se = FALSE, method = "lm") +
  scale_fill_viridis_d() +
  theme_reach() +
  scale_x_reverse() +
  facet_wrap(~name, scales = "free_y") +
  labs(x = "Forty",
       y = "Value",
       title = "Forty Time Only Predicts Bad Run Rate in the NFL",
       subtitle = "2014-2020, minimum of 200 NFL rushes")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

season_speed_to_40 <- speed_projs_filtered %>%
  group_by(player, player_id, season, offense) %>%
  summarize(rushes = n(),
            exp_speed = mean(exp_speed),
            avg_speed = mean(avg_speed),
            avg_ybc = mean(ybc),
            avg_speed_oe = mean(speed_oe)) %>%
  filter(rushes >= 50) %>%
  left_join(combine_select, by = c("player_id")) %>%
  dplyr::select(-position, -combine_weight, -speed_score) %>%
  ungroup()

season_speed_to_40$forty[is.na(season_speed_to_40$forty)] <- mean(season_speed_to_40$forty, na.rm = T)
season_speed_to_40$twenty[is.na(season_speed_to_40$twenty)] <- mean(season_speed_to_40$twenty, na.rm = T)
season_speed_to_40$ten[is.na(season_speed_to_40$ten)] <- mean(season_speed_to_40$ten, na.rm = T)

forty_rank <- season_speed_to_40 %>%
  arrange(forty) %>%
  mutate(forty_rank = row_number()) %>%
  dplyr::select(forty_rank, adj_forty = forty)
twenty_rank <- season_speed_to_40 %>%
  arrange(twenty) %>%
  mutate(twenty_rank = row_number()) %>%
  dplyr::select(twenty_rank, adj_twenty = twenty)

season_speed_to_40 <- season_speed_to_40 %>%
  arrange(-avg_speed_oe) %>%
  mutate(speed_perc = round(100*range01(avg_speed_oe), 1),
         speed_rank = row_number()) %>%
  left_join(forty_rank, by = c("speed_rank" = "forty_rank")) %>%
  left_join(twenty_rank, by = c("speed_rank" = "twenty_rank"))

lm_40 <- lm(adj_forty ~ speed_perc, data = season_speed_to_40)
summary(lm_40)

teams_logos_select <- teams_colors_logos %>%
  dplyr::select(team_abbr, team_logo_espn, team_color)

game_speed_to_40 <-  speed_projs_filtered %>%
  group_by(player, player_id, season, week, game_id, offense, defense) %>%
  summarize(rushes = n(),
            exp_speed = mean(exp_speed),
            avg_speed = mean(avg_speed),
            avg_ybc = mean(ybc),
            avg_speed_oe = mean(speed_oe)) %>%
  filter(rushes >= 10) %>%
  mutate(speed_perc = round(100*(avg_speed_oe-min(game_speed_to_40$avg_speed_oe))/(max(game_speed_to_40$avg_speed_oe)-min(game_speed_to_40$avg_speed_oe)), 1)) %>%
  left_join(teams_logos_select, by = c("defense" = "team_abbr")) %>%
  dplyr::select(-team_color, defense_logo = team_logo_espn) %>%
  left_join(teams_logos_select, by = c("offense" = "team_abbr")) %>%
  dplyr::select(-team_logo_espn)

games_40 <- predict(lm_40, newdata = game_speed_to_40)

game_speed_to_40 <- cbind(game_speed_to_40, games_40)

game_speed_to_40 <- game_speed_to_40 %>%
  rename(game_forty = starts_with("..."))

write.csv(season_speed_to_40, "season_speed_to_40.csv")
write.csv(game_speed_to_40, "game_speed_to_40.csv")

############################################################################

the_rusher <- "Derrick Henry"
the_season <- 2020
player_games <- game_speed_to_40 %>%
  filter(player == the_rusher) %>%
  filter(season == the_season)
the_weeks <- unique(player_games$week)
league_games <- game_speed_to_40 %>%
  filter(player != the_rusher) %>%
  filter(season == the_season) %>%
  filter(week %in% the_weeks)

ggplot() +
  geom_jitter(data = league_games, aes(x = as.factor(week), y = game_forty), size = 4, color = "black", alpha = 0.2, width = 0.05) +
  geom_line(data = player_games, aes(x = as.factor(week), color = team_color, y = game_forty), size = 2, group = 1) +
  geom_image(data = player_games, aes(x = as.factor(week), image = defense_logo, y = game_forty), asp = 16/9, size = 0.05) +
  theme_reach() +
  scale_color_identity() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 5)) +
  geom_hline(yintercept = mean(game_speed_to_40$game_forty)) +
  labs(x = "Week",
       y = "Game-Adjusted 40 Time",
       title = paste0(the_rusher, "'s Game-Adjusted 40 Yard Dash Time, ", the_season),
       subtitle = "Black dots listed for every other rusher with at least 10 rushes")

player_season <- season_speed_to_40 %>%
  filter(player == the_rusher) %>%
  left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
the_seasons <- unique(player_season$season)
league_season <- season_speed_to_40 %>%
  filter(player != the_rusher) %>%
  filter(season %in% the_seasons)

ggplot() +
  geom_jitter(data = league_season, aes(x = as.factor(season), y = adj_forty), size = 4, color = "black", alpha = 0.2, width = 0.025) +
  geom_line(data = player_season, aes(x = as.factor(season), color = team_color, y = adj_forty), size = 2, group = 1) +
  geom_image(data = player_season, aes(x = as.factor(season), image = team_logo_espn, y = adj_forty), asp = 16/9, size = 0.05) +
  theme_reach() +
  scale_color_identity() +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 5)) +
  geom_hline(yintercept = mean(game_speed_to_40$game_forty)) +
  labs(x = "Season",
       y = "Season-Adjusted 40 Time",
       title = paste0(the_rusher, "'s Season-Adjusted 40 Yard Dash Time by Season"),
       subtitle = "Black dots listed for every other rusher with at least 50 rushes")


ryoe_stats <- nfl_ryoe_projs %>%
  filter(season >= 2018) %>%
  group_by(player) %>%
  summarize(avg_ryoe = mean(ryoe))

speed_oe_stats <- speed_oe_stats %>%
  left_join(ryoe_stats, by = c("player"))

speed_oe_stats %>%
  ggplot(aes(x = avg_speed_oe, y = avg_ryoe)) +
  geom_smooth(se = FALSE, method = "lm", size = 1.7, color = "black") +
  geom_hline(yintercept = mean(speed_oe_stats$avg_ryoe), linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(speed_oe_stats$avg_speed_oe), linetype = "dashed", alpha = 0.5) +
  geom_point(aes(fill = team_color, color = team_color2, size = rushes), shape = 21, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = player), size = 5) +
  theme_reach() +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(x = 'Average Speed Over Expected',
       y = "Average RYOE",
       title = "How Speed Over Expected Correlates with Rushing Yards Over Expected",
       subtitle = "2018-2020, minimum of 250 rushes") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
  


