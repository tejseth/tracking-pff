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

trace(pull_ngs,edit=TRUE)

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

rushing_data <<- pull_s3(paste0("analytics/projections/by_facet/", 'nfl', "/%i/rushing.csv.gz"), season_start = 2017, season_end = 2020)

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

tracking_1 <- tracking_run_2020_1 %>%
  filter(run_pass == "R")
first_contact_1 <- tracking_1 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_1 <- tracking_1 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_1 <- tracking_1 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_1 <- play_end_1 %>%
  left_join(first_contact_1, by = c("gameid", "playid")) %>%
  left_join(handoff_1, by = c("gameid", "playid"))
tracking_1 <- tracking_1 %>%
  left_join(frames_1, by = c("gameid", "playid"))
tracking_1_filtered <- tracking_1 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_1_rushers <- tracking_1_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_1_rushers <- tracking_1_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_1 <- tracking_1_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_1, "play_speed_1.csv")

tracking_2 <- tracking_run_2020_2 %>%
  filter(run_pass == "R")
first_contact_2 <- tracking_2 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_2 <- tracking_2 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_2 <- tracking_2 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_2 <- play_end_2 %>%
  left_join(first_contact_2, by = c("gameid", "playid")) %>%
  left_join(handoff_2, by = c("gameid", "playid"))
tracking_2 <- tracking_2 %>%
  left_join(frames_2, by = c("gameid", "playid"))
tracking_2_filtered <- tracking_2 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_2_rushers <- tracking_2_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_2_rushers <- tracking_2_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_2 <- tracking_2_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_2, "play_speed_2.csv")

tracking_3 <- tracking_run_2020_3 %>%
  filter(run_pass == "R")
first_contact_3 <- tracking_3 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_3 <- tracking_3 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_3 <- tracking_3 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_3 <- play_end_3 %>%
  left_join(first_contact_3, by = c("gameid", "playid")) %>%
  left_join(handoff_3, by = c("gameid", "playid"))
tracking_3 <- tracking_3 %>%
  left_join(frames_3, by = c("gameid", "playid"))
tracking_3_filtered <- tracking_3 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_3_rushers <- tracking_3_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_3_rushers <- tracking_3_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_3 <- tracking_3_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_3, "play_speed_3.csv")

tracking_4 <- tracking_run_2020_4 %>%
  filter(run_pass == "R")
first_contact_4 <- tracking_4 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_4 <- tracking_4 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_4 <- tracking_4 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_4 <- play_end_4 %>%
  left_join(first_contact_4, by = c("gameid", "playid")) %>%
  left_join(handoff_4, by = c("gameid", "playid"))
tracking_4 <- tracking_4 %>%
  left_join(frames_4, by = c("gameid", "playid"))
tracking_4_filtered <- tracking_4 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_4_rushers <- tracking_4_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_4_rushers <- tracking_4_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_4 <- tracking_4_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_4, "play_speed_4.csv")

tracking_5 <- tracking_run_2020_5 %>%
  filter(run_pass == "R")
first_contact_5 <- tracking_5 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_5 <- tracking_5 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_5 <- tracking_5 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_5 <- play_end_5 %>%
  left_join(first_contact_5, by = c("gameid", "playid")) %>%
  left_join(handoff_5, by = c("gameid", "playid"))
tracking_5 <- tracking_5 %>%
  left_join(frames_5, by = c("gameid", "playid"))
tracking_5_filtered <- tracking_5 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_5_rushers <- tracking_5_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_5_rushers <- tracking_5_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_5 <- tracking_5_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_5, "play_speed_5.csv")

tracking_6 <- tracking_run_2020_6 %>%
  filter(run_pass == "R")
first_contact_6 <- tracking_6 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_6 <- tracking_6 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_6 <- tracking_6 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_6 <- play_end_6 %>%
  left_join(first_contact_6, by = c("gameid", "playid")) %>%
  left_join(handoff_6, by = c("gameid", "playid"))
tracking_6 <- tracking_6 %>%
  left_join(frames_6, by = c("gameid", "playid"))
tracking_6_filtered <- tracking_6 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_6_rushers <- tracking_6_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_6_rushers <- tracking_6_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_6 <- tracking_6_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_6, "play_speed_6.csv")

tracking_7 <- tracking_run_2020_7 %>%
  filter(run_pass == "R")
first_contact_7 <- tracking_7 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_7 <- tracking_7 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_7 <- tracking_7 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_7 <- play_end_7 %>%
  left_join(first_contact_7, by = c("gameid", "playid")) %>%
  left_join(handoff_7, by = c("gameid", "playid"))
tracking_7 <- tracking_7 %>%
  left_join(frames_7, by = c("gameid", "playid"))
tracking_7_filtered <- tracking_7 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_7_rushers <- tracking_7_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_7_rushers <- tracking_7_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_7 <- tracking_7_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_7, "play_speed_7.csv")

tracking_8 <- tracking_run_2020_8 %>%
  filter(run_pass == "R")
first_contact_8 <- tracking_8 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_8 <- tracking_8 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_8 <- tracking_8 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_8 <- play_end_8 %>%
  left_join(first_contact_8, by = c("gameid", "playid")) %>%
  left_join(handoff_8, by = c("gameid", "playid"))
tracking_8 <- tracking_8 %>%
  left_join(frames_8, by = c("gameid", "playid"))
tracking_8_filtered <- tracking_8 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_8_rushers <- tracking_8_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_8_rushers <- tracking_8_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_8 <- tracking_8_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_8, "play_speed_8.csv")

tracking_9 <- tracking_run_2020_9 %>%
  filter(run_pass == "R")
first_contact_9 <- tracking_9 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_9 <- tracking_9 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_9 <- tracking_9 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_9 <- play_end_9 %>%
  left_join(first_contact_9, by = c("gameid", "playid")) %>%
  left_join(handoff_9, by = c("gameid", "playid"))
tracking_9 <- tracking_9 %>%
  left_join(frames_9, by = c("gameid", "playid"))
tracking_9_filtered <- tracking_9 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_9_rushers <- tracking_9_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_9_rushers <- tracking_9_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_9 <- tracking_9_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_9, "play_speed_9.csv")

tracking_10 <- tracking_run_2020_10 %>%
  filter(run_pass == "R")
first_contact_10 <- tracking_10 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_10 <- tracking_10 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_10 <- tracking_10 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_10 <- play_end_10 %>%
  left_join(first_contact_10, by = c("gameid", "playid")) %>%
  left_join(handoff_10, by = c("gameid", "playid"))
tracking_10 <- tracking_10 %>%
  left_join(frames_10, by = c("gameid", "playid"))
tracking_10_filtered <- tracking_10 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_10_rushers <- tracking_10_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_10_rushers <- tracking_10_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_10 <- tracking_10_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_10, "play_speed_10.csv")

tracking_11 <- tracking_run_2020_11 %>%
  filter(run_pass == "R")
first_contact_11 <- tracking_11 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_11 <- tracking_11 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_11 <- tracking_11 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_11 <- play_end_11 %>%
  left_join(first_contact_11, by = c("gameid", "playid")) %>%
  left_join(handoff_11, by = c("gameid", "playid"))
tracking_11 <- tracking_11 %>%
  left_join(frames_11, by = c("gameid", "playid"))
tracking_11_filtered <- tracking_11 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_11_rushers <- tracking_11_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_11_rushers <- tracking_11_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_11 <- tracking_11_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_11, "play_speed_11.csv")

tracking_12 <- tracking_run_2020_12 %>%
  filter(run_pass == "R")
first_contact_12 <- tracking_12 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_12 <- tracking_12 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_12 <- tracking_12 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_12 <- play_end_12 %>%
  left_join(first_contact_12, by = c("gameid", "playid")) %>%
  left_join(handoff_12, by = c("gameid", "playid"))
tracking_12 <- tracking_12 %>%
  left_join(frames_12, by = c("gameid", "playid"))
tracking_12_filtered <- tracking_12 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_12_rushers <- tracking_12_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_12_rushers <- tracking_12_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_12 <- tracking_12_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_12, "play_speed_12.csv")

tracking_13 <- tracking_run_2020_13 %>%
  filter(run_pass == "R")
first_contact_13 <- tracking_13 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_13 <- tracking_13 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_13 <- tracking_13 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_13 <- play_end_13 %>%
  left_join(first_contact_13, by = c("gameid", "playid")) %>%
  left_join(handoff_13, by = c("gameid", "playid"))
tracking_13 <- tracking_13 %>%
  left_join(frames_13, by = c("gameid", "playid"))
tracking_13_filtered <- tracking_13 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_13_rushers <- tracking_13_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_13_rushers <- tracking_13_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_13 <- tracking_13_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_13, "play_speed_13.csv")

tracking_14 <- tracking_run_2020_14 %>%
  filter(run_pass == "R")
first_contact_14 <- tracking_14 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_14 <- tracking_14 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_14 <- tracking_14 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_14 <- play_end_14 %>%
  left_join(first_contact_14, by = c("gameid", "playid")) %>%
  left_join(handoff_14, by = c("gameid", "playid"))
tracking_14 <- tracking_14 %>%
  left_join(frames_14, by = c("gameid", "playid"))
tracking_14_filtered <- tracking_14 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_14_rushers <- tracking_14_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_14_rushers <- tracking_14_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_14 <- tracking_14_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_14, "play_speed_14.csv")

tracking_15 <- tracking_run_2020_15 %>%
  filter(run_pass == "R")
first_contact_15 <- tracking_15 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_15 <- tracking_15 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_15 <- tracking_15 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_15 <- play_end_15 %>%
  left_join(first_contact_15, by = c("gameid", "playid")) %>%
  left_join(handoff_15, by = c("gameid", "playid"))
tracking_15 <- tracking_15 %>%
  left_join(frames_15, by = c("gameid", "playid"))
tracking_15_filtered <- tracking_15 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_15_rushers <- tracking_15_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_15_rushers <- tracking_15_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_15 <- tracking_15_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_15, "play_speed_15.csv")

tracking_16 <- tracking_run_2020_16 %>%
  filter(run_pass == "R")
first_contact_16 <- tracking_16 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_16 <- tracking_16 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_16 <- tracking_16 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_16 <- play_end_16 %>%
  left_join(first_contact_16, by = c("gameid", "playid")) %>%
  left_join(handoff_16, by = c("gameid", "playid"))
tracking_16 <- tracking_16 %>%
  left_join(frames_16, by = c("gameid", "playid"))
tracking_16_filtered <- tracking_16 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_16_rushers <- tracking_16_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_16_rushers <- tracking_16_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_16 <- tracking_16_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_16, "play_speed_16.csv")

tracking_17 <- tracking_run_2020_17 %>%
  filter(run_pass == "R")
first_contact_17 <- tracking_17 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_17 <- tracking_17 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_17 <- tracking_17 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_17 <- play_end_17 %>%
  left_join(first_contact_17, by = c("gameid", "playid")) %>%
  left_join(handoff_17, by = c("gameid", "playid"))
tracking_17 <- tracking_17 %>%
  left_join(frames_17, by = c("gameid", "playid"))
tracking_17_filtered <- tracking_17 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_17_rushers <- tracking_17_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_17_rushers <- tracking_17_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_17 <- tracking_17_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_17, "play_speed_17.csv")

ID <- c(seq(1, 17))

df_play_speed_all <- list()

for(i in 1:length(ID)){
  
  play_speed_temp <- read_csv(paste0("~/tracking-pff/play-speed-week/play_speed_",ID[i],".csv"),
                              col_types = cols())
  
  df_play_speed_all[[i]] <- play_speed_temp
  
}

play_speed_all_20 <- rbindlist(df_play_speed_all)
write.csv(play_speed_all_20, "play_speed_all_20.csv")

###################################### 2019 ###################################

tracking_run_2019_1 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 1, wk_end = 1, run_pass_all = "r")
tracking_run_2019_2 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 2, wk_end = 2, run_pass_all = "r")
tracking_run_2019_3 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 3, wk_end = 3, run_pass_all = "r")
tracking_run_2019_4 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 4, wk_end = 4, run_pass_all = "r")
tracking_run_2019_5 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 5, wk_end = 5, run_pass_all = "r")
tracking_run_2019_6 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 6, wk_end = 6, run_pass_all = "r")
tracking_run_2019_7 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 7, wk_end = 7, run_pass_all = "r")
tracking_run_2019_8 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 8, wk_end = 8, run_pass_all = "r")
tracking_run_2019_9 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 9, wk_end = 9, run_pass_all = "r")
tracking_run_2019_10 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 10, wk_end = 10, run_pass_all = "r")
tracking_run_2019_11 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 11, wk_end = 11, run_pass_all = "r")
tracking_run_2019_12 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 12, wk_end = 12, run_pass_all = "r")
tracking_run_2019_13 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 13, wk_end = 13, run_pass_all = "r")
tracking_run_2019_14 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 14, wk_end = 14, run_pass_all = "r")
tracking_run_2019_15 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 15, wk_end = 15, run_pass_all = "r")
tracking_run_2019_16 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 16, wk_end = 16, run_pass_all = "r")
tracking_run_2019_17 <- pull_ngs(season_start = 2019, season_end = 2019, wk_start = 17, wk_end = 17, run_pass_all = "r")

tracking_1 <- tracking_run_2019_1 %>%
  filter(run_pass == "R")
first_contact_1 <- tracking_1 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_1 <- tracking_1 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_1 <- tracking_1 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_1 <- play_end_1 %>%
  left_join(first_contact_1, by = c("gameid", "playid")) %>%
  left_join(handoff_1, by = c("gameid", "playid"))
tracking_1 <- tracking_1 %>%
  left_join(frames_1, by = c("gameid", "playid"))
tracking_1_filtered <- tracking_1 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_1_rushers <- tracking_1_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_1_rushers <- tracking_1_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_1_19 <- tracking_1_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_1_19, "play_speed_1_19.csv")

tracking_2 <- tracking_run_2019_2 %>%
  filter(run_pass == "R")
first_contact_2 <- tracking_2 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_2 <- tracking_2 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_2 <- tracking_2 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_2 <- play_end_2 %>%
  left_join(first_contact_2, by = c("gameid", "playid")) %>%
  left_join(handoff_2, by = c("gameid", "playid"))
tracking_2 <- tracking_2 %>%
  left_join(frames_2, by = c("gameid", "playid"))
tracking_2_filtered <- tracking_2 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_2_rushers <- tracking_2_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_2_rushers <- tracking_2_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_2_19 <- tracking_2_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_2_19, "play_speed_2_19.csv")

tracking_3 <- tracking_run_2019_3 %>%
  filter(run_pass == "R")
first_contact_3 <- tracking_3 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_3 <- tracking_3 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_3 <- tracking_3 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_3 <- play_end_3 %>%
  left_join(first_contact_3, by = c("gameid", "playid")) %>%
  left_join(handoff_3, by = c("gameid", "playid"))
tracking_3 <- tracking_3 %>%
  left_join(frames_3, by = c("gameid", "playid"))
tracking_3_filtered <- tracking_3 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_3_rushers <- tracking_3_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_3_rushers <- tracking_3_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_3_19 <- tracking_3_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_3_19, "play_speed_3_19.csv")

tracking_4 <- tracking_run_2019_4 %>%
  filter(run_pass == "R")
first_contact_4 <- tracking_4 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_4 <- tracking_4 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_4 <- tracking_4 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_4 <- play_end_4 %>%
  left_join(first_contact_4, by = c("gameid", "playid")) %>%
  left_join(handoff_4, by = c("gameid", "playid"))
tracking_4 <- tracking_4 %>%
  left_join(frames_4, by = c("gameid", "playid"))
tracking_4_filtered <- tracking_4 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_4_rushers <- tracking_4_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_4_rushers <- tracking_4_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_4_19 <- tracking_4_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_4_19, "play_speed_4_19.csv")

tracking_5 <- tracking_run_2019_5 %>%
  filter(run_pass == "R")
first_contact_5 <- tracking_5 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_5 <- tracking_5 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_5 <- tracking_5 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_5 <- play_end_5 %>%
  left_join(first_contact_5, by = c("gameid", "playid")) %>%
  left_join(handoff_5, by = c("gameid", "playid"))
tracking_5 <- tracking_5 %>%
  left_join(frames_5, by = c("gameid", "playid"))
tracking_5_filtered <- tracking_5 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_5_rushers <- tracking_5_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_5_rushers <- tracking_5_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_5_19 <- tracking_5_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_5_19, "play_speed_5_19.csv")

tracking_6 <- tracking_run_2019_6 %>%
  filter(run_pass == "R")
first_contact_6 <- tracking_6 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_6 <- tracking_6 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_6 <- tracking_6 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_6 <- play_end_6 %>%
  left_join(first_contact_6, by = c("gameid", "playid")) %>%
  left_join(handoff_6, by = c("gameid", "playid"))
tracking_6 <- tracking_6 %>%
  left_join(frames_6, by = c("gameid", "playid"))
tracking_6_filtered <- tracking_6 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_6_rushers <- tracking_6_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_6_rushers <- tracking_6_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_6_19 <- tracking_6_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_6_19, "play_speed_6_19.csv")

tracking_7 <- tracking_run_2019_7 %>%
  filter(run_pass == "R")
first_contact_7 <- tracking_7 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_7 <- tracking_7 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_7 <- tracking_7 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_7 <- play_end_7 %>%
  left_join(first_contact_7, by = c("gameid", "playid")) %>%
  left_join(handoff_7, by = c("gameid", "playid"))
tracking_7 <- tracking_7 %>%
  left_join(frames_7, by = c("gameid", "playid"))
tracking_7_filtered <- tracking_7 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_7_rushers <- tracking_7_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_7_rushers <- tracking_7_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_7_19 <- tracking_7_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_7_19, "play_speed_7_19.csv")

tracking_8 <- tracking_run_2019_8 %>%
  filter(run_pass == "R")
first_contact_8 <- tracking_8 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_8 <- tracking_8 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_8 <- tracking_8 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_8 <- play_end_8 %>%
  left_join(first_contact_8, by = c("gameid", "playid")) %>%
  left_join(handoff_8, by = c("gameid", "playid"))
tracking_8 <- tracking_8 %>%
  left_join(frames_8, by = c("gameid", "playid"))
tracking_8_filtered <- tracking_8 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_8_rushers <- tracking_8_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_8_rushers <- tracking_8_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_8_19 <- tracking_8_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_8_19, "play_speed_8_19.csv")

tracking_9 <- tracking_run_2019_9 %>%
  filter(run_pass == "R")
first_contact_9 <- tracking_9 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_9 <- tracking_9 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_9 <- tracking_9 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_9 <- play_end_9 %>%
  left_join(first_contact_9, by = c("gameid", "playid")) %>%
  left_join(handoff_9, by = c("gameid", "playid"))
tracking_9 <- tracking_9 %>%
  left_join(frames_9, by = c("gameid", "playid"))
tracking_9_filtered <- tracking_9 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_9_rushers <- tracking_9_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_9_rushers <- tracking_9_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_9_19 <- tracking_9_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_9_19, "play_speed_9_19.csv")

tracking_10 <- tracking_run_2019_10 %>%
  filter(run_pass == "R")
first_contact_10 <- tracking_10 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_10 <- tracking_10 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_10 <- tracking_10 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_10 <- play_end_10 %>%
  left_join(first_contact_10, by = c("gameid", "playid")) %>%
  left_join(handoff_10, by = c("gameid", "playid"))
tracking_10 <- tracking_10 %>%
  left_join(frames_10, by = c("gameid", "playid"))
tracking_10_filtered <- tracking_10 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_10_rushers <- tracking_10_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_10_rushers <- tracking_10_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_10_19 <- tracking_10_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_10_19, "play_speed_10_19.csv")

tracking_11 <- tracking_run_2019_11 %>%
  filter(run_pass == "R")
first_contact_11 <- tracking_11 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_11 <- tracking_11 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_11 <- tracking_11 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_11 <- play_end_11 %>%
  left_join(first_contact_11, by = c("gameid", "playid")) %>%
  left_join(handoff_11, by = c("gameid", "playid"))
tracking_11 <- tracking_11 %>%
  left_join(frames_11, by = c("gameid", "playid"))
tracking_11_filtered <- tracking_11 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_11_rushers <- tracking_11_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_11_rushers <- tracking_11_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_11_19 <- tracking_11_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_11_19, "play_speed_11_19.csv")

tracking_12 <- tracking_run_2019_12 %>%
  filter(run_pass == "R")
first_contact_12 <- tracking_12 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_12 <- tracking_12 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_12 <- tracking_12 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_12 <- play_end_12 %>%
  left_join(first_contact_12, by = c("gameid", "playid")) %>%
  left_join(handoff_12, by = c("gameid", "playid"))
tracking_12 <- tracking_12 %>%
  left_join(frames_12, by = c("gameid", "playid"))
tracking_12_filtered <- tracking_12 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_12_rushers <- tracking_12_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_12_rushers <- tracking_12_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_12_19 <- tracking_12_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_12_19, "play_speed_12_19.csv")

tracking_13 <- tracking_run_2019_13 %>%
  filter(run_pass == "R")
first_contact_13 <- tracking_13 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_13 <- tracking_13 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_13 <- tracking_13 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_13 <- play_end_13 %>%
  left_join(first_contact_13, by = c("gameid", "playid")) %>%
  left_join(handoff_13, by = c("gameid", "playid"))
tracking_13 <- tracking_13 %>%
  left_join(frames_13, by = c("gameid", "playid"))
tracking_13_filtered <- tracking_13 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_13_rushers <- tracking_13_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_13_rushers <- tracking_13_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_13_19 <- tracking_13_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_13_19, "play_speed_13_19.csv")

tracking_14 <- tracking_run_2019_14 %>%
  filter(run_pass == "R")
first_contact_14 <- tracking_14 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_14 <- tracking_14 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_14 <- tracking_14 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_14 <- play_end_14 %>%
  left_join(first_contact_14, by = c("gameid", "playid")) %>%
  left_join(handoff_14, by = c("gameid", "playid"))
tracking_14 <- tracking_14 %>%
  left_join(frames_14, by = c("gameid", "playid"))
tracking_14_filtered <- tracking_14 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_14_rushers <- tracking_14_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_14_rushers <- tracking_14_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_14_19 <- tracking_14_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_14_19, "play_speed_14_19.csv")

tracking_15 <- tracking_run_2019_15 %>%
  filter(run_pass == "R")
first_contact_15 <- tracking_15 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_15 <- tracking_15 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_15 <- tracking_15 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_15 <- play_end_15 %>%
  left_join(first_contact_15, by = c("gameid", "playid")) %>%
  left_join(handoff_15, by = c("gameid", "playid"))
tracking_15 <- tracking_15 %>%
  left_join(frames_15, by = c("gameid", "playid"))
tracking_15_filtered <- tracking_15 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_15_rushers <- tracking_15_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_15_rushers <- tracking_15_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_15_19 <- tracking_15_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_15_19, "play_speed_15_19.csv")

tracking_16 <- tracking_run_2019_16 %>%
  filter(run_pass == "R")
first_contact_16 <- tracking_16 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_16 <- tracking_16 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_16 <- tracking_16 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_16 <- play_end_16 %>%
  left_join(first_contact_16, by = c("gameid", "playid")) %>%
  left_join(handoff_16, by = c("gameid", "playid"))
tracking_16 <- tracking_16 %>%
  left_join(frames_16, by = c("gameid", "playid"))
tracking_16_filtered <- tracking_16 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_16_rushers <- tracking_16_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_16_rushers <- tracking_16_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_16_19 <- tracking_16_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_16_19, "play_speed_16_19.csv")

tracking_17 <- tracking_run_2019_17 %>%
  filter(run_pass == "R")
first_contact_17 <- tracking_17 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_17 <- tracking_17 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_17 <- tracking_17 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_17 <- play_end_17 %>%
  left_join(first_contact_17, by = c("gameid", "playid")) %>%
  left_join(handoff_17, by = c("gameid", "playid"))
tracking_17 <- tracking_17 %>%
  left_join(frames_17, by = c("gameid", "playid"))
tracking_17_filtered <- tracking_17 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_17_rushers <- tracking_17_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_17_rushers <- tracking_17_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_17_19 <- tracking_17_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_17_19, "play_speed_17_19.csv")

ID <- c(seq(1, 17))

df_play_speed_all_19 <- list()

for(i in 1:length(ID)){
  
  play_speed_temp_19 <- read_csv(paste0("~/tracking-pff/play-speed-week/play_speed_",ID[i],"_19.csv"),
                                 col_types = cols())
  
  df_play_speed_all_19[[i]] <- play_speed_temp_19
  
}

play_speed_all_19 <- rbindlist(df_play_speed_all_19)
write.csv(play_speed_all_19, "play_speed_all_19.csv")

###################################### 2018 ###################################

tracking_run_2018_1 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 1, wk_end = 1, run_pass_all = "r")
tracking_run_2018_2 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 2, wk_end = 2, run_pass_all = "r")
tracking_run_2018_3 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 3, wk_end = 3, run_pass_all = "r")
tracking_run_2018_4 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 4, wk_end = 4, run_pass_all = "r")
tracking_run_2018_5 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 5, wk_end = 5, run_pass_all = "r")
tracking_run_2018_6 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 6, wk_end = 6, run_pass_all = "r")
tracking_run_2018_7 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 7, wk_end = 7, run_pass_all = "r")
tracking_run_2018_8 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 8, wk_end = 8, run_pass_all = "r")
tracking_run_2018_9 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 9, wk_end = 9, run_pass_all = "r")
tracking_run_2018_10 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 10, wk_end = 10, run_pass_all = "r")
tracking_run_2018_11 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 11, wk_end = 11, run_pass_all = "r")
tracking_run_2018_12 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 12, wk_end = 12, run_pass_all = "r")
tracking_run_2018_13 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 13, wk_end = 13, run_pass_all = "r")
tracking_run_2018_14 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 14, wk_end = 14, run_pass_all = "r")
tracking_run_2018_15 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 15, wk_end = 15, run_pass_all = "r")
tracking_run_2018_16 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 16, wk_end = 16, run_pass_all = "r")
tracking_run_2018_17 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 17, wk_end = 17, run_pass_all = "r")

tracking_1 <- tracking_run_2018_1 %>%
  filter(run_pass == "R")
first_contact_1 <- tracking_1 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_1 <- tracking_1 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_1 <- tracking_1 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_1 <- play_end_1 %>%
  left_join(first_contact_1, by = c("gameid", "playid")) %>%
  left_join(handoff_1, by = c("gameid", "playid"))
tracking_1 <- tracking_1 %>%
  left_join(frames_1, by = c("gameid", "playid"))
tracking_1_filtered <- tracking_1 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_1_rushers <- tracking_1_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_1_rushers <- tracking_1_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_1_18 <- tracking_1_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_1_18, "play_speed_1_18.csv")

tracking_2 <- tracking_run_2018_2 %>%
  filter(run_pass == "R")
first_contact_2 <- tracking_2 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_2 <- tracking_2 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_2 <- tracking_2 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_2 <- play_end_2 %>%
  left_join(first_contact_2, by = c("gameid", "playid")) %>%
  left_join(handoff_2, by = c("gameid", "playid"))
tracking_2 <- tracking_2 %>%
  left_join(frames_2, by = c("gameid", "playid"))
tracking_2_filtered <- tracking_2 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_2_rushers <- tracking_2_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_2_rushers <- tracking_2_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_2_18 <- tracking_2_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_2_18, "play_speed_2_18.csv")

tracking_3 <- tracking_run_2018_3 %>%
  filter(run_pass == "R")
first_contact_3 <- tracking_3 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_3 <- tracking_3 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_3 <- tracking_3 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_3 <- play_end_3 %>%
  left_join(first_contact_3, by = c("gameid", "playid")) %>%
  left_join(handoff_3, by = c("gameid", "playid"))
tracking_3 <- tracking_3 %>%
  left_join(frames_3, by = c("gameid", "playid"))
tracking_3_filtered <- tracking_3 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_3_rushers <- tracking_3_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_3_rushers <- tracking_3_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_3_18 <- tracking_3_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_3_18, "play_speed_3_18.csv")

tracking_4 <- tracking_run_2018_4 %>%
  filter(run_pass == "R")
first_contact_4 <- tracking_4 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_4 <- tracking_4 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_4 <- tracking_4 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_4 <- play_end_4 %>%
  left_join(first_contact_4, by = c("gameid", "playid")) %>%
  left_join(handoff_4, by = c("gameid", "playid"))
tracking_4 <- tracking_4 %>%
  left_join(frames_4, by = c("gameid", "playid"))
tracking_4_filtered <- tracking_4 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_4_rushers <- tracking_4_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_4_rushers <- tracking_4_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_4_18 <- tracking_4_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_4_18, "play_speed_4_18.csv")

tracking_5 <- tracking_run_2018_5 %>%
  filter(run_pass == "R")
first_contact_5 <- tracking_5 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_5 <- tracking_5 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_5 <- tracking_5 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_5 <- play_end_5 %>%
  left_join(first_contact_5, by = c("gameid", "playid")) %>%
  left_join(handoff_5, by = c("gameid", "playid"))
tracking_5 <- tracking_5 %>%
  left_join(frames_5, by = c("gameid", "playid"))
tracking_5_filtered <- tracking_5 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_5_rushers <- tracking_5_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_5_rushers <- tracking_5_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_5_18 <- tracking_5_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_5_18, "play_speed_5_18.csv")

tracking_6 <- tracking_run_2018_6 %>%
  filter(run_pass == "R")
first_contact_6 <- tracking_6 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_6 <- tracking_6 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_6 <- tracking_6 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_6 <- play_end_6 %>%
  left_join(first_contact_6, by = c("gameid", "playid")) %>%
  left_join(handoff_6, by = c("gameid", "playid"))
tracking_6 <- tracking_6 %>%
  left_join(frames_6, by = c("gameid", "playid"))
tracking_6_filtered <- tracking_6 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_6_rushers <- tracking_6_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_6_rushers <- tracking_6_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_6_18 <- tracking_6_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_6_18, "play_speed_6_18.csv")

tracking_7 <- tracking_run_2018_7 %>%
  filter(run_pass == "R")
first_contact_7 <- tracking_7 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_7 <- tracking_7 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_7 <- tracking_7 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_7 <- play_end_7 %>%
  left_join(first_contact_7, by = c("gameid", "playid")) %>%
  left_join(handoff_7, by = c("gameid", "playid"))
tracking_7 <- tracking_7 %>%
  left_join(frames_7, by = c("gameid", "playid"))
tracking_7_filtered <- tracking_7 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_7_rushers <- tracking_7_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_7_rushers <- tracking_7_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_7_18 <- tracking_7_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_7_18, "play_speed_7_18.csv")

tracking_8 <- tracking_run_2018_8 %>%
  filter(run_pass == "R")
first_contact_8 <- tracking_8 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_8 <- tracking_8 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_8 <- tracking_8 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_8 <- play_end_8 %>%
  left_join(first_contact_8, by = c("gameid", "playid")) %>%
  left_join(handoff_8, by = c("gameid", "playid"))
tracking_8 <- tracking_8 %>%
  left_join(frames_8, by = c("gameid", "playid"))
tracking_8_filtered <- tracking_8 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_8_rushers <- tracking_8_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_8_rushers <- tracking_8_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_8_18 <- tracking_8_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_8_18, "play_speed_8_18.csv")

tracking_9 <- tracking_run_2018_9 %>%
  filter(run_pass == "R")
first_contact_9 <- tracking_9 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_9 <- tracking_9 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_9 <- tracking_9 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_9 <- play_end_9 %>%
  left_join(first_contact_9, by = c("gameid", "playid")) %>%
  left_join(handoff_9, by = c("gameid", "playid"))
tracking_9 <- tracking_9 %>%
  left_join(frames_9, by = c("gameid", "playid"))
tracking_9_filtered <- tracking_9 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_9_rushers <- tracking_9_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_9_rushers <- tracking_9_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_9_18 <- tracking_9_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_9_18, "play_speed_9_18.csv")

tracking_10 <- tracking_run_2018_10 %>%
  filter(run_pass == "R")
first_contact_10 <- tracking_10 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_10 <- tracking_10 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_10 <- tracking_10 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_10 <- play_end_10 %>%
  left_join(first_contact_10, by = c("gameid", "playid")) %>%
  left_join(handoff_10, by = c("gameid", "playid"))
tracking_10 <- tracking_10 %>%
  left_join(frames_10, by = c("gameid", "playid"))
tracking_10_filtered <- tracking_10 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_10_rushers <- tracking_10_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_10_rushers <- tracking_10_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_10_18 <- tracking_10_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_10_18, "play_speed_10_18.csv")

tracking_11 <- tracking_run_2018_11 %>%
  filter(run_pass == "R")
first_contact_11 <- tracking_11 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_11 <- tracking_11 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_11 <- tracking_11 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_11 <- play_end_11 %>%
  left_join(first_contact_11, by = c("gameid", "playid")) %>%
  left_join(handoff_11, by = c("gameid", "playid"))
tracking_11 <- tracking_11 %>%
  left_join(frames_11, by = c("gameid", "playid"))
tracking_11_filtered <- tracking_11 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_11_rushers <- tracking_11_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_11_rushers <- tracking_11_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_11_18 <- tracking_11_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_11_18, "play_speed_11_18.csv")

tracking_12 <- tracking_run_2018_12 %>%
  filter(run_pass == "R")
first_contact_12 <- tracking_12 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_12 <- tracking_12 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_12 <- tracking_12 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_12 <- play_end_12 %>%
  left_join(first_contact_12, by = c("gameid", "playid")) %>%
  left_join(handoff_12, by = c("gameid", "playid"))
tracking_12 <- tracking_12 %>%
  left_join(frames_12, by = c("gameid", "playid"))
tracking_12_filtered <- tracking_12 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_12_rushers <- tracking_12_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_12_rushers <- tracking_12_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_12_18 <- tracking_12_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_12_18, "play_speed_12_18.csv")

tracking_13 <- tracking_run_2018_13 %>%
  filter(run_pass == "R")
first_contact_13 <- tracking_13 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_13 <- tracking_13 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_13 <- tracking_13 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_13 <- play_end_13 %>%
  left_join(first_contact_13, by = c("gameid", "playid")) %>%
  left_join(handoff_13, by = c("gameid", "playid"))
tracking_13 <- tracking_13 %>%
  left_join(frames_13, by = c("gameid", "playid"))
tracking_13_filtered <- tracking_13 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_13_rushers <- tracking_13_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_13_rushers <- tracking_13_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_13_18 <- tracking_13_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_13_18, "play_speed_13_18.csv")

tracking_14 <- tracking_run_2018_14 %>%
  filter(run_pass == "R")
first_contact_14 <- tracking_14 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_14 <- tracking_14 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_14 <- tracking_14 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_14 <- play_end_14 %>%
  left_join(first_contact_14, by = c("gameid", "playid")) %>%
  left_join(handoff_14, by = c("gameid", "playid"))
tracking_14 <- tracking_14 %>%
  left_join(frames_14, by = c("gameid", "playid"))
tracking_14_filtered <- tracking_14 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_14_rushers <- tracking_14_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_14_rushers <- tracking_14_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_14_18 <- tracking_14_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_14_18, "play_speed_14_18.csv")

tracking_15 <- tracking_run_2018_15 %>%
  filter(run_pass == "R")
first_contact_15 <- tracking_15 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_15 <- tracking_15 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_15 <- tracking_15 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_15 <- play_end_15 %>%
  left_join(first_contact_15, by = c("gameid", "playid")) %>%
  left_join(handoff_15, by = c("gameid", "playid"))
tracking_15 <- tracking_15 %>%
  left_join(frames_15, by = c("gameid", "playid"))
tracking_15_filtered <- tracking_15 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_15_rushers <- tracking_15_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_15_rushers <- tracking_15_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_15_18 <- tracking_15_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_15_18, "play_speed_15_18.csv")

tracking_16 <- tracking_run_2018_16 %>%
  filter(run_pass == "R")
first_contact_16 <- tracking_16 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_16 <- tracking_16 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_16 <- tracking_16 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_16 <- play_end_16 %>%
  left_join(first_contact_16, by = c("gameid", "playid")) %>%
  left_join(handoff_16, by = c("gameid", "playid"))
tracking_16 <- tracking_16 %>%
  left_join(frames_16, by = c("gameid", "playid"))
tracking_16_filtered <- tracking_16 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_16_rushers <- tracking_16_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_16_rushers <- tracking_16_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_16_18 <- tracking_16_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_16_18, "play_speed_16_18.csv")

tracking_17 <- tracking_run_2018_17 %>%
  filter(run_pass == "R")
first_contact_17 <- tracking_17 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_17 <- tracking_17 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_17 <- tracking_17 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_17 <- play_end_17 %>%
  left_join(first_contact_17, by = c("gameid", "playid")) %>%
  left_join(handoff_17, by = c("gameid", "playid"))
tracking_17 <- tracking_17 %>%
  left_join(frames_17, by = c("gameid", "playid"))
tracking_17_filtered <- tracking_17 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_17_rushers <- tracking_17_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_17_rushers <- tracking_17_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_17_18 <- tracking_17_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_17_18, "play_speed_17_18.csv")

ID <- c(seq(1, 17))

df_play_speed_all_18 <- list()

for(i in 1:length(ID)){
  
  play_speed_temp_18 <- read_csv(paste0("~/tracking-pff/play-speed-week/play_speed_",ID[i],"_18.csv"),
                                 col_types = cols())
  
  df_play_speed_all_18[[i]] <- play_speed_temp_18
  
}

play_speed_all_18 <- rbindlist(df_play_speed_all_18)
write.csv(play_speed_all_18, "play_speed_all_18.csv")

###################################### 2017 ###################################

tracking_run_2017_1 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 1, wk_end = 1, run_pass_all = "r")
tracking_run_2017_2 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 2, wk_end = 2, run_pass_all = "r")
tracking_run_2017_3 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 3, wk_end = 3, run_pass_all = "r")
tracking_run_2017_4 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 4, wk_end = 4, run_pass_all = "r")
tracking_run_2017_5 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 5, wk_end = 5, run_pass_all = "r")
tracking_run_2017_6 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 6, wk_end = 6, run_pass_all = "r")
tracking_run_2017_7 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 7, wk_end = 7, run_pass_all = "r")
tracking_run_2017_8 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 8, wk_end = 8, run_pass_all = "r")
tracking_run_2017_9 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 9, wk_end = 9, run_pass_all = "r")
tracking_run_2017_10 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 10, wk_end = 10, run_pass_all = "r")
tracking_run_2017_11 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 11, wk_end = 11, run_pass_all = "r")
tracking_run_2017_12 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 12, wk_end = 12, run_pass_all = "r")
tracking_run_2017_13 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 13, wk_end = 13, run_pass_all = "r")
tracking_run_2017_14 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 14, wk_end = 14, run_pass_all = "r")
tracking_run_2017_15 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 15, wk_end = 15, run_pass_all = "r")
tracking_run_2017_16 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 16, wk_end = 16, run_pass_all = "r")
tracking_run_2017_17 <- pull_ngs(season_start = 2017, season_end = 2017, wk_start = 17, wk_end = 17, run_pass_all = "r")

tracking_1 <- tracking_run_2017_1 %>%
  filter(run_pass == "R")
first_contact_1 <- tracking_1 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_1 <- tracking_1 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_1 <- tracking_1 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_1 <- play_end_1 %>%
  left_join(first_contact_1, by = c("gameid", "playid")) %>%
  left_join(handoff_1, by = c("gameid", "playid"))
tracking_1 <- tracking_1 %>%
  left_join(frames_1, by = c("gameid", "playid"))
tracking_1_filtered <- tracking_1 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_1_rushers <- tracking_1_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_1_rushers <- tracking_1_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_1_17 <- tracking_1_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_1_17, "play_speed_1_17.csv")

tracking_2 <- tracking_run_2017_2 %>%
  filter(run_pass == "R")
first_contact_2 <- tracking_2 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_2 <- tracking_2 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_2 <- tracking_2 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_2 <- play_end_2 %>%
  left_join(first_contact_2, by = c("gameid", "playid")) %>%
  left_join(handoff_2, by = c("gameid", "playid"))
tracking_2 <- tracking_2 %>%
  left_join(frames_2, by = c("gameid", "playid"))
tracking_2_filtered <- tracking_2 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_2_rushers <- tracking_2_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_2_rushers <- tracking_2_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_2_17 <- tracking_2_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_2_17, "play_speed_2_17.csv")

tracking_3 <- tracking_run_2017_3 %>%
  filter(run_pass == "R")
first_contact_3 <- tracking_3 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_3 <- tracking_3 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_3 <- tracking_3 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_3 <- play_end_3 %>%
  left_join(first_contact_3, by = c("gameid", "playid")) %>%
  left_join(handoff_3, by = c("gameid", "playid"))
tracking_3 <- tracking_3 %>%
  left_join(frames_3, by = c("gameid", "playid"))
tracking_3_filtered <- tracking_3 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_3_rushers <- tracking_3_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_3_rushers <- tracking_3_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_3_17 <- tracking_3_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_3_17, "play_speed_3_17.csv")

tracking_4 <- tracking_run_2017_4 %>%
  filter(run_pass == "R")
first_contact_4 <- tracking_4 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_4 <- tracking_4 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_4 <- tracking_4 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_4 <- play_end_4 %>%
  left_join(first_contact_4, by = c("gameid", "playid")) %>%
  left_join(handoff_4, by = c("gameid", "playid"))
tracking_4 <- tracking_4 %>%
  left_join(frames_4, by = c("gameid", "playid"))
tracking_4_filtered <- tracking_4 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_4_rushers <- tracking_4_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_4_rushers <- tracking_4_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_4_17 <- tracking_4_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_4_17, "play_speed_4_17.csv")

tracking_5 <- tracking_run_2017_5 %>%
  filter(run_pass == "R")
first_contact_5 <- tracking_5 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_5 <- tracking_5 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_5 <- tracking_5 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_5 <- play_end_5 %>%
  left_join(first_contact_5, by = c("gameid", "playid")) %>%
  left_join(handoff_5, by = c("gameid", "playid"))
tracking_5 <- tracking_5 %>%
  left_join(frames_5, by = c("gameid", "playid"))
tracking_5_filtered <- tracking_5 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_5_rushers <- tracking_5_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_5_rushers <- tracking_5_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_5_17 <- tracking_5_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_5_17, "play_speed_5_17.csv")

tracking_6 <- tracking_run_2017_6 %>%
  filter(run_pass == "R")
first_contact_6 <- tracking_6 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_6 <- tracking_6 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_6 <- tracking_6 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_6 <- play_end_6 %>%
  left_join(first_contact_6, by = c("gameid", "playid")) %>%
  left_join(handoff_6, by = c("gameid", "playid"))
tracking_6 <- tracking_6 %>%
  left_join(frames_6, by = c("gameid", "playid"))
tracking_6_filtered <- tracking_6 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_6_rushers <- tracking_6_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_6_rushers <- tracking_6_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_6_17 <- tracking_6_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_6_17, "play_speed_6_17.csv")

tracking_7 <- tracking_run_2017_7 %>%
  filter(run_pass == "R")
first_contact_7 <- tracking_7 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_7 <- tracking_7 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_7 <- tracking_7 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_7 <- play_end_7 %>%
  left_join(first_contact_7, by = c("gameid", "playid")) %>%
  left_join(handoff_7, by = c("gameid", "playid"))
tracking_7 <- tracking_7 %>%
  left_join(frames_7, by = c("gameid", "playid"))
tracking_7_filtered <- tracking_7 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_7_rushers <- tracking_7_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_7_rushers <- tracking_7_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_7_17 <- tracking_7_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_7_17, "play_speed_7_17.csv")

tracking_8 <- tracking_run_2017_8 %>%
  filter(run_pass == "R")
first_contact_8 <- tracking_8 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_8 <- tracking_8 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_8 <- tracking_8 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_8 <- play_end_8 %>%
  left_join(first_contact_8, by = c("gameid", "playid")) %>%
  left_join(handoff_8, by = c("gameid", "playid"))
tracking_8 <- tracking_8 %>%
  left_join(frames_8, by = c("gameid", "playid"))
tracking_8_filtered <- tracking_8 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_8_rushers <- tracking_8_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_8_rushers <- tracking_8_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_8_17 <- tracking_8_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_8_17, "play_speed_8_17.csv")

tracking_9 <- tracking_run_2017_9 %>%
  filter(run_pass == "R")
first_contact_9 <- tracking_9 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_9 <- tracking_9 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_9 <- tracking_9 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_9 <- play_end_9 %>%
  left_join(first_contact_9, by = c("gameid", "playid")) %>%
  left_join(handoff_9, by = c("gameid", "playid"))
tracking_9 <- tracking_9 %>%
  left_join(frames_9, by = c("gameid", "playid"))
tracking_9_filtered <- tracking_9 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_9_rushers <- tracking_9_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_9_rushers <- tracking_9_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_9_17 <- tracking_9_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_9_17, "play_speed_9_17.csv")

tracking_10 <- tracking_run_2017_10 %>%
  filter(run_pass == "R")
first_contact_10 <- tracking_10 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_10 <- tracking_10 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_10 <- tracking_10 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_10 <- play_end_10 %>%
  left_join(first_contact_10, by = c("gameid", "playid")) %>%
  left_join(handoff_10, by = c("gameid", "playid"))
tracking_10 <- tracking_10 %>%
  left_join(frames_10, by = c("gameid", "playid"))
tracking_10_filtered <- tracking_10 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_10_rushers <- tracking_10_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_10_rushers <- tracking_10_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_10_17 <- tracking_10_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_10_17, "play_speed_10_17.csv")

tracking_11 <- tracking_run_2017_11 %>%
  filter(run_pass == "R")
first_contact_11 <- tracking_11 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_11 <- tracking_11 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_11 <- tracking_11 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_11 <- play_end_11 %>%
  left_join(first_contact_11, by = c("gameid", "playid")) %>%
  left_join(handoff_11, by = c("gameid", "playid"))
tracking_11 <- tracking_11 %>%
  left_join(frames_11, by = c("gameid", "playid"))
tracking_11_filtered <- tracking_11 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_11_rushers <- tracking_11_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_11_rushers <- tracking_11_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_11_17 <- tracking_11_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_11_17, "play_speed_11_17.csv")

tracking_12 <- tracking_run_2017_12 %>%
  filter(run_pass == "R")
first_contact_12 <- tracking_12 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_12 <- tracking_12 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_12 <- tracking_12 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_12 <- play_end_12 %>%
  left_join(first_contact_12, by = c("gameid", "playid")) %>%
  left_join(handoff_12, by = c("gameid", "playid"))
tracking_12 <- tracking_12 %>%
  left_join(frames_12, by = c("gameid", "playid"))
tracking_12_filtered <- tracking_12 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_12_rushers <- tracking_12_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_12_rushers <- tracking_12_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_12_17 <- tracking_12_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_12_17, "play_speed_12_17.csv")

tracking_13 <- tracking_run_2017_13 %>%
  filter(run_pass == "R")
first_contact_13 <- tracking_13 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_13 <- tracking_13 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_13 <- tracking_13 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_13 <- play_end_13 %>%
  left_join(first_contact_13, by = c("gameid", "playid")) %>%
  left_join(handoff_13, by = c("gameid", "playid"))
tracking_13 <- tracking_13 %>%
  left_join(frames_13, by = c("gameid", "playid"))
tracking_13_filtered <- tracking_13 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_13_rushers <- tracking_13_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_13_rushers <- tracking_13_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_13_17 <- tracking_13_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_13_17, "play_speed_13_17.csv")

tracking_14 <- tracking_run_2017_14 %>%
  filter(run_pass == "R")
first_contact_14 <- tracking_14 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_14 <- tracking_14 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_14 <- tracking_14 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_14 <- play_end_14 %>%
  left_join(first_contact_14, by = c("gameid", "playid")) %>%
  left_join(handoff_14, by = c("gameid", "playid"))
tracking_14 <- tracking_14 %>%
  left_join(frames_14, by = c("gameid", "playid"))
tracking_14_filtered <- tracking_14 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_14_rushers <- tracking_14_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_14_rushers <- tracking_14_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_14_17 <- tracking_14_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_14_17, "play_speed_14_17.csv")

tracking_15 <- tracking_run_2017_15 %>%
  filter(run_pass == "R")
first_contact_15 <- tracking_15 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_15 <- tracking_15 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_15 <- tracking_15 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_15 <- play_end_15 %>%
  left_join(first_contact_15, by = c("gameid", "playid")) %>%
  left_join(handoff_15, by = c("gameid", "playid"))
tracking_15 <- tracking_15 %>%
  left_join(frames_15, by = c("gameid", "playid"))
tracking_15_filtered <- tracking_15 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_15_rushers <- tracking_15_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_15_rushers <- tracking_15_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_15_17 <- tracking_15_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_15_17, "play_speed_15_17.csv")

tracking_16 <- tracking_run_2017_16 %>%
  filter(run_pass == "R")
first_contact_16 <- tracking_16 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_16 <- tracking_16 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_16 <- tracking_16 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_16 <- play_end_16 %>%
  left_join(first_contact_16, by = c("gameid", "playid")) %>%
  left_join(handoff_16, by = c("gameid", "playid"))
tracking_16 <- tracking_16 %>%
  left_join(frames_16, by = c("gameid", "playid"))
tracking_16_filtered <- tracking_16 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_16_rushers <- tracking_16_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_16_rushers <- tracking_16_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_16_17 <- tracking_16_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_16_17, "play_speed_16_17.csv")

tracking_17 <- tracking_run_2017_17 %>%
  filter(run_pass == "R")
first_contact_17 <- tracking_17 %>%
  filter(play_event == "first_contact") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(first_contact_time = time_since_snap)
play_end_17 <- tracking_17 %>%
  group_by(gameid, playid) %>%
  summarize(play_end_frame = max(time_since_snap))
handoff_17 <- tracking_17 %>%
  filter(play_event == "handoff") %>%
  distinct(gameid, playid, time_since_snap) %>%
  rename(handoff_time = time_since_snap)
frames_17 <- play_end_17 %>%
  left_join(first_contact_17, by = c("gameid", "playid")) %>%
  left_join(handoff_17, by = c("gameid", "playid"))
tracking_17 <- tracking_17 %>%
  left_join(frames_17, by = c("gameid", "playid"))
tracking_17_filtered <- tracking_17 %>%
  filter(!is.na(handoff_time)) %>%
  filter(time_since_snap <= first_contact_time) %>%
  filter(time_since_snap >= handoff_time)
tracking_17_rushers <- tracking_17_filtered %>%
  left_join(rushers, by = c("gameid" = "game_id", "playid" = "play_id"))
tracking_17_rushers <- tracking_17_rushers %>%
  mutate(is_rusher = ifelse(player_name == player, 1, 0)) %>%
  filter(is_rusher == 1)
play_speed_17_17 <- tracking_17_rushers %>%
  group_by(gameid, playid, player) %>%
  summarize(avg_speed = mean(speed),
            seconds_before_contact = n()/10,
            ybc = mean(yards_before_contact))
write.csv(play_speed_17_17, "play_speed_17_17.csv")

ID <- c(seq(1, 17))

df_play_speed_all_17 <- list()

for(i in 1:length(ID)){
  
  play_speed_temp_17 <- read_csv(paste0("~/tracking-pff/play-speed-week/play_speed_",ID[i],"_17.csv"),
                                 col_types = cols())
  
  df_play_speed_all_17[[i]] <- play_speed_temp_17
  
}

play_speed_all_17 <- rbindlist(df_play_speed_all_17)
write.csv(play_speed_all_17, "play_speed_all_17.csv")




