library(rdtools)
library(tidyverse)
library(ggthemes)
library(nflfastR)

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

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
#tracking_run_2020_2 <- pull_ngs(season_start = 2020, season_end = 2020, wk_start = 2, wk_end = 2, run_pass_all = "r")
tracking_run_2020_2 <- read_csv("~/Downloads/2020_week_2.csv")
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


play_speed_all <- do.call("rbind", list(play_speed_1, play_speed_2, play_speed_3, 
                                        play_speed_4, play_speed_5, play_speed_6, 
                                        play_speed_7,play_speed_8, play_speed_9, 
                                        play_speed_10, play_speed_11, play_speed_12, play_speed_13,
                                        play_speed_14, play_speed_15, play_speed_16, play_speed_17))
write.csv(play_speed_all, "play_speed_all.csv")

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
                yards_after_contact, avoided_tackles) %>%
  left_join(play_speed_all, by = c("game_id" = "gameid", "play_id" = "playid")) %>%
  filter(!is.na(avg_speed))

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

ekeler <- tracking_ %>%
  filter(gameid == 18542 & playid == 3526308) %>%
  filter(player_name == "Austin Ekeler")

ekeler %>%
  filter(time_since_snap < 6) %>%
  ggplot(aes(x = time_since_snap, y = speed)) +
  geom_line(color = "#0073cf", size = 2) +
  geom_point(fill = "#002244", color = "#0073cf", shape = 21, size = 6) +
  theme_reach() +
  geom_vline(xintercept = 2.1, linetype = "dashed", alpha = 0.8) +
  annotate("text", x = 2.0, y = 0.9, label = "Handoff", angle = 90, size = 5) +
  geom_vline(xintercept = 4.2, linetype = "dashed", alpha = 0.8) +
  annotate("text", x = 4.1, y = 1.35, label = "First Contact", angle = 90, size = 5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = "Seconds Since Snap",
       y = "Speed",
       title = "Austin Ekeler had the Highest Average Burst in Week 1 on This Play")





