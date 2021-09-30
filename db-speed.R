library(rdtools)
library(tidyverse)

tracking_pass_2020_1 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 1, wk_end = 1, run_pass_all = "p")
tracking_pass_2020_2 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 2, wk_end = 2, run_pass_all = "p")
tracking_pass_2020_3 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 3, wk_end = 3, run_pass_all = "p")
tracking_pass_2020_4 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 4, wk_end = 4, run_pass_all = "p")
tracking_pass_2020_5 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 5, wk_end = 5, run_pass_all = "p")
tracking_pass_2020_6 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 6, wk_end = 6, run_pass_all = "p")
tracking_pass_2020_7 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 7, wk_end = 7, run_pass_all = "p")
tracking_pass_2020_8 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 8, wk_end = 8, run_pass_all = "p")
tracking_pass_2020_9 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 9, wk_end = 9, run_pass_all = "p")
tracking_pass_2020_10 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 10, wk_end = 10, run_pass_all = "p")
tracking_pass_2020_11 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 11, wk_end = 11, run_pass_all = "p")
tracking_pass_2020_12 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 12, wk_end = 12, run_pass_all = "p")
tracking_pass_2020_13 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 13, wk_end = 13, run_pass_all = "p")
tracking_pass_2020_14 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 14, wk_end = 14, run_pass_all = "p")
tracking_pass_2020_15 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 15, wk_end = 15, run_pass_all = "p")
tracking_pass_2020_16 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 16, wk_end = 16, run_pass_all = "p")
tracking_pass_2020_17 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 17, wk_end = 17, run_pass_all = "p")

one_play <- tracking_pass_2020_1 %>% filter(gameid == "18532", playid == "3515736")

one_play %>% 
  filter(off_def == "D") %>%
  filter(time_since_snap < 4.1) %>%
  ggplot(aes(x = x, y = y, color = player_name)) +
  geom_point(aes(size = speed)) +
  geom_hline(yintercept = 0, color = "black", size = 1.5) +
  ggthemes::theme_fivethirtyeight()

tracking_1 <- tracking_pass_2020_1 %>%
  filter(run_pass == "P")
pass_arrival_1 <- tracking_1 %>%
  filter(play_event == "pass_forward") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(pass_forward_time = time_since_snap)
tracking_1 <- tracking_1 %>%
  left_join(pass_arrival_1, by = c("gameid", "playid"))
player_start_x_1 <- tracking_1 %>%
  filter(play_event == "ball_snap" | time_since_snap == 0) %>%
  dplyr::select(gameid, playid, player_name, start_x = x) %>%
  distinct(gameid, playid, player_name, start_x)
player_end_x_1 <- tracking_1 %>%
  filter(play_event == "pass_forward") %>%
  dplyr::select(gameid, playid, player_name, end_x = x) %>%
  distinct(gameid, playid, player_name, end_x)
player_start_y_1 <- tracking_1 %>%
  filter(play_event == "ball_snap") %>%
  dplyr::select(gameid, playid, player_name, start_y = y) %>%
  distinct(gameid, playid, player_name, start_y)
player_end_y_1 <- tracking_1 %>%
  filter(play_event == "pass_forward") %>%
  dplyr::select(gameid, playid, player_name, end_y = y) %>%
  distinct(gameid, playid, player_name, end_y)
tracking_1 <- tracking_1 %>%
  left_join(player_start_x_1, by = c("gameid", "playid", "player_name")) %>%
  left_join(player_end_x_1, by = c("gameid", "playid", "player_name")) %>%
  left_join(player_start_y_1, by = c("gameid", "playid", "player_name")) %>%
  left_join(player_end_y_1, by = c("gameid", "playid", "player_name"))
tracking_1_d <- tracking_1 %>%
  filter(off_def == "D") %>%
  mutate(abs_change_x = abs(end_x - start_x),
         change_y = end_y - end_x) %>%
  filter(abs_change_x <= 5, change_y >= 10)
play_speed_20_1 <- tracking_1_d %>%
  group_by(player_name, gameid, playid) %>%
  summarize(avg_speed = mean(speed),
            max_speed = max(speed))
write.csv(play_speed_20_1, "play_speed_20_1.csv")

tracking_2 <- tracking_pass_2020_2 %>%
  filter(run_pass == "P")
pass_arrival_2 <- tracking_2 %>%
  filter(play_event == "pass_forward") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(pass_forward_time = time_since_snap)
tracking_2 <- tracking_2 %>%
  left_join(pass_arrival_2, by = c("gameid", "playid"))
player_start_x_2 <- tracking_2 %>%
  filter(play_event == "ball_snap" | time_since_snap == 0) %>%
  dplyr::select(gameid, playid, player_name, start_x = x) %>%
  distinct(gameid, playid, player_name, start_x)
player_end_x_2 <- tracking_2 %>%
  filter(play_event == "pass_forward") %>%
  dplyr::select(gameid, playid, player_name, end_x = x) %>%
  distinct(gameid, playid, player_name, end_x)
player_start_y_2 <- tracking_2 %>%
  filter(play_event == "ball_snap") %>%
  dplyr::select(gameid, playid, player_name, start_y = y) %>%
  distinct(gameid, playid, player_name, start_y)
player_end_y_2 <- tracking_2 %>%
  filter(play_event == "pass_forward") %>%
  dplyr::select(gameid, playid, player_name, end_y = y) %>%
  distinct(gameid, playid, player_name, end_y)
tracking_2 <- tracking_2 %>%
  left_join(player_start_x_2, by = c("gameid", "playid", "player_name")) %>%
  left_join(player_end_x_2, by = c("gameid", "playid", "player_name")) %>%
  left_join(player_start_y_2, by = c("gameid", "playid", "player_name")) %>%
  left_join(player_end_y_2, by = c("gameid", "playid", "player_name"))
tracking_2_d <- tracking_2 %>%
  filter(off_def == "D") %>%
  mutate(abs_change_x = abs(end_x - start_x),
         change_y = end_y - end_x) %>%
  filter(abs_change_x <= 5, change_y >= 10)
play_speed_20_2 <- tracking_2_d %>%
  group_by(player_name, gameid, playid) %>%
  summarize(avg_speed = mean(speed),
            max_speed = max(speed))
write.csv(play_speed_20_2, "play_speed_20_2.csv")

tracking_pass_2019_1 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 1, wk_end = 1, run_pass_all = "p")
tracking_pass_2019_2 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 2, wk_end = 2, run_pass_all = "p")
tracking_pass_2019_3 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 3, wk_end = 3, run_pass_all = "p")
tracking_pass_2019_4 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 4, wk_end = 4, run_pass_all = "p")
tracking_pass_2019_5 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 5, wk_end = 5, run_pass_all = "p")
tracking_pass_2019_6 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 6, wk_end = 6, run_pass_all = "p")
tracking_pass_2019_7 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 7, wk_end = 7, run_pass_all = "p")
tracking_pass_2019_8 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 8, wk_end = 8, run_pass_all = "p")
tracking_pass_2019_9 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 9, wk_end = 9, run_pass_all = "p")
tracking_pass_2019_10 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 10, wk_end = 10, run_pass_all = "p")
tracking_pass_2019_11 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 11, wk_end = 11, run_pass_all = "p")
tracking_pass_2019_12 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 12, wk_end = 12, run_pass_all = "p")
tracking_pass_2019_13 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 13, wk_end = 13, run_pass_all = "p")
tracking_pass_2019_14 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 14, wk_end = 14, run_pass_all = "p")
tracking_pass_2019_15 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 15, wk_end = 15, run_pass_all = "p")
tracking_pass_2019_16 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 16, wk_end = 16, run_pass_all = "p")
tracking_pass_2019_17 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 17, wk_end = 17, run_pass_all = "p")


