library(rdtools)
library(tidyverse)
library(data.table)

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

db_speed_func <- function(the_tracking) {
  tracking <- the_tracking %>%
    filter(run_pass == "P")
  pass_arrival <- tracking %>%
    filter(play_event == "pass_forward") %>%
    distinct(gameid, playid, time_since_snap) %>%
    rename(pass_forward_time = time_since_snap)
  tracking <- tracking %>%
    left_join(pass_arrival, by = c("gameid", "playid"))
  player_start_x <- tracking %>%
    filter(play_event == "ball_snap" | time_since_snap == 0) %>%
    dplyr::select(gameid, playid, player_name, start_x = x) %>%
    distinct(gameid, playid, player_name, start_x)
  player_end_x <- tracking %>%
    filter(play_event == "pass_forward") %>%
    dplyr::select(gameid, playid, player_name, end_x = x) %>%
    distinct(gameid, playid, player_name, end_x)
  player_start_y <- tracking %>%
    filter(play_event == "ball_snap") %>%
    dplyr::select(gameid, playid, player_name, start_y = y) %>%
    distinct(gameid, playid, player_name, start_y)
  player_end_y <- tracking %>%
    filter(play_event == "pass_forward") %>%
    dplyr::select(gameid, playid, player_name, end_y = y) %>%
    distinct(gameid, playid, player_name, end_y)
  tracking <- tracking %>%
    left_join(player_start_x, by = c("gameid", "playid", "player_name")) %>%
    left_join(player_end_x, by = c("gameid", "playid", "player_name")) 
  tracking <- tracking %>%
    left_join(player_start_y, by = c("gameid", "playid", "player_name")) %>%
    left_join(player_end_y, by = c("gameid", "playid", "player_name"))
  tracking_d <- tracking %>%
    filter(off_def == "D") %>%
    mutate(abs_change_x = abs(end_x - start_x),
           change_y = end_y - end_x) %>%
    filter(abs_change_x <= 5, change_y >= 10)
  play_speed_20 <- tracking_d %>%
    group_by(player_name, gameid, playid) %>%
    summarize(avg_speed = mean(speed),
              max_speed = max(speed))
  
  return(play_speed_20)
}

play_speed_20_1 <- db_speed_func(tracking_pass_2020_1)
write.csv(play_speed_20_1, "play_speed_20_1.csv")

play_speed_20_2 <- db_speed_func(tracking_pass_2020_2)
write.csv(play_speed_20_2, "play_speed_20_2.csv")

play_speed_20_3 <- db_speed_func(tracking_pass_2020_3)
write.csv(play_speed_20_3, "play_speed_20_3.csv")

play_speed_20_4 <- db_speed_func(tracking_pass_2020_4)
write.csv(play_speed_20_4, "play_speed_20_4.csv")

play_speed_20_5 <- db_speed_func(tracking_pass_2020_5)
write.csv(play_speed_20_5, "play_speed_20_5.csv")

play_speed_20_6 <- db_speed_func(tracking_pass_2020_6)
write.csv(play_speed_20_6, "play_speed_20_6.csv")

play_speed_20_7 <- db_speed_func(tracking_pass_2020_7)
write.csv(play_speed_20_7, "play_speed_20_7.csv")

play_speed_20_8 <- db_speed_func(tracking_pass_2020_8)
write.csv(play_speed_20_8, "play_speed_20_8.csv")

play_speed_20_9 <- db_speed_func(tracking_pass_2020_9)
write.csv(play_speed_20_9, "play_speed_20_9.csv")

play_speed_20_10 <- db_speed_func(tracking_pass_2020_10)
write.csv(play_speed_20_10, "play_speed_20_10.csv")

play_speed_20_11 <- db_speed_func(tracking_pass_2020_11)
write.csv(play_speed_20_11, "play_speed_20_11.csv")

play_speed_20_12 <- db_speed_func(tracking_pass_2020_12)
write.csv(play_speed_20_12, "play_speed_20_12.csv")

play_speed_20_13 <- db_speed_func(tracking_pass_2020_13)
write.csv(play_speed_20_13, "play_speed_20_13.csv")

play_speed_20_14 <- db_speed_func(tracking_pass_2020_14)
write.csv(play_speed_20_14, "play_speed_20_14.csv")

play_speed_20_15 <- db_speed_func(tracking_pass_2020_15)
write.csv(play_speed_20_15, "play_speed_20_15.csv")

play_speed_20_16 <- db_speed_func(tracking_pass_2020_16)
write.csv(play_speed_20_16, "play_speed_20_16.csv")

play_speed_20_17 <- db_speed_func(tracking_pass_2020_17)
write.csv(play_speed_20_17, "play_speed_20_17.csv")

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

play_speed_19_1 <- db_speed_func(tracking_pass_2019_1)
write.csv(play_speed_19_1, "play_speed_19_1.csv")

play_speed_19_2 <- db_speed_func(tracking_pass_2019_2)
write.csv(play_speed_19_2, "play_speed_19_2.csv")

play_speed_19_3 <- db_speed_func(tracking_pass_2019_3)
write.csv(play_speed_19_3, "play_speed_19_3.csv")

play_speed_19_4 <- db_speed_func(tracking_pass_2019_4)
write.csv(play_speed_19_4, "play_speed_19_4.csv")

play_speed_19_5 <- db_speed_func(tracking_pass_2019_5)
write.csv(play_speed_19_5, "play_speed_19_5.csv")

play_speed_19_6 <- db_speed_func(tracking_pass_2019_6)
write.csv(play_speed_19_6, "play_speed_19_6.csv")

play_speed_19_7 <- db_speed_func(tracking_pass_2019_7)
write.csv(play_speed_19_7, "play_speed_19_7.csv")

play_speed_19_8 <- db_speed_func(tracking_pass_2019_8)
write.csv(play_speed_19_8, "play_speed_19_8.csv")

play_speed_19_9 <- db_speed_func(tracking_pass_2019_9)
write.csv(play_speed_19_9, "play_speed_19_9.csv")

play_speed_19_10 <- db_speed_func(tracking_pass_2019_10)
write.csv(play_speed_19_10, "play_speed_19_10.csv")

play_speed_19_11 <- db_speed_func(tracking_pass_2019_11)
write.csv(play_speed_19_11, "play_speed_19_11.csv")

play_speed_19_12 <- db_speed_func(tracking_pass_2019_12)
write.csv(play_speed_19_12, "play_speed_19_12.csv")

play_speed_19_13 <- db_speed_func(tracking_pass_2019_13)
write.csv(play_speed_19_13, "play_speed_19_13.csv")

play_speed_19_14 <- db_speed_func(tracking_pass_2019_14)
write.csv(play_speed_19_14, "play_speed_19_14.csv")

play_speed_19_15 <- db_speed_func(tracking_pass_2019_15)
write.csv(play_speed_19_15, "play_speed_19_15.csv")

play_speed_19_16 <- db_speed_func(tracking_pass_2019_16)
write.csv(play_speed_19_16, "play_speed_19_16.csv")

play_speed_19_17 <- db_speed_func(tracking_pass_2019_17)
write.csv(play_speed_19_17, "play_speed_19_17.csv")

tracking_pass_2018_1 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 1, wk_end = 1, run_pass_all = "p")
tracking_pass_2018_2 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 2, wk_end = 2, run_pass_all = "p")
tracking_pass_2018_3 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 3, wk_end = 3, run_pass_all = "p")
tracking_pass_2018_4 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 4, wk_end = 4, run_pass_all = "p")
tracking_pass_2018_5 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 5, wk_end = 5, run_pass_all = "p")
tracking_pass_2018_6 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 6, wk_end = 6, run_pass_all = "p")
tracking_pass_2018_7 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 7, wk_end = 7, run_pass_all = "p")
tracking_pass_2018_8 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 8, wk_end = 8, run_pass_all = "p")
tracking_pass_2018_9 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 9, wk_end = 9, run_pass_all = "p")
tracking_pass_2018_10 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 10, wk_end = 10, run_pass_all = "p")
tracking_pass_2018_11 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 11, wk_end = 11, run_pass_all = "p")
tracking_pass_2018_12 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 12, wk_end = 12, run_pass_all = "p")
tracking_pass_2018_13 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 13, wk_end = 13, run_pass_all = "p")
tracking_pass_2018_14 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 14, wk_end = 14, run_pass_all = "p")
tracking_pass_2018_15 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 15, wk_end = 15, run_pass_all = "p")
tracking_pass_2018_16 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 16, wk_end = 16, run_pass_all = "p")
tracking_pass_2018_17 <- pull_ngs(season_start = 2018, season_end = 2018, wk_start = 17, wk_end = 17, run_pass_all = "p")

play_speed_18_1 <- db_speed_func(tracking_pass_2018_1)
write.csv(play_speed_18_1, "play_speed_18_1.csv")

play_speed_18_2 <- db_speed_func(tracking_pass_2018_2)
write.csv(play_speed_18_2, "play_speed_18_2.csv")

play_speed_18_3 <- db_speed_func(tracking_pass_2018_3)
write.csv(play_speed_18_3, "play_speed_18_3.csv")

play_speed_18_4 <- db_speed_func(tracking_pass_2018_4)
write.csv(play_speed_18_4, "play_speed_18_4.csv")

play_speed_18_5 <- db_speed_func(tracking_pass_2018_5)
write.csv(play_speed_18_5, "play_speed_18_5.csv")

play_speed_18_6 <- db_speed_func(tracking_pass_2018_6)
write.csv(play_speed_18_6, "play_speed_18_6.csv")

play_speed_18_7 <- db_speed_func(tracking_pass_2018_7)
write.csv(play_speed_18_7, "play_speed_18_7.csv")

play_speed_18_8 <- db_speed_func(tracking_pass_2018_8)
write.csv(play_speed_18_8, "play_speed_18_8.csv")

play_speed_18_9 <- db_speed_func(tracking_pass_2018_9)
write.csv(play_speed_18_9, "play_speed_18_9.csv")

play_speed_18_10 <- db_speed_func(tracking_pass_2018_10)
write.csv(play_speed_18_10, "play_speed_18_10.csv")

play_speed_18_11 <- db_speed_func(tracking_pass_2018_11)
write.csv(play_speed_18_11, "play_speed_18_11.csv")

play_speed_18_12 <- db_speed_func(tracking_pass_2018_12)
write.csv(play_speed_18_12, "play_speed_18_12.csv")

play_speed_18_13 <- db_speed_func(tracking_pass_2018_13)
write.csv(play_speed_18_13, "play_speed_18_13.csv")

play_speed_18_14 <- db_speed_func(tracking_pass_2018_14)
write.csv(play_speed_18_14, "play_speed_18_14.csv")

play_speed_18_15 <- db_speed_func(tracking_pass_2018_15)
write.csv(play_speed_18_15, "play_speed_18_15.csv")

play_speed_18_16 <- db_speed_func(tracking_pass_2018_16)
write.csv(play_speed_18_16, "play_speed_18_16.csv")

play_speed_18_17 <- db_speed_func(tracking_pass_2018_17)
write.csv(play_speed_18_17, "play_speed_18_17.csv")

##########################################################################

ID <- c(seq(1, 17))

df_play_speed_all_20 <- list()

for(i in 1:length(ID)){
  
  play_speed_temp_20 <- read_csv(paste0("~/tracking-pff/cb-speed-20/play_speed_20_",ID[i],".csv"),
                                 col_types = cols())
  
  df_play_speed_all_20[[i]] <- play_speed_temp_20
  
}

cb_speed_all_20 <- rbindlist(df_play_speed_all_20)
write.csv(cb_speed_all_20, "cb_speed_all_20.csv")

cb_speed_all_20 %>% 
  group_by(player_name) %>% 
  summarize(count = n(), avg_speed = mean(avg_speed)) %>% 
  filter(count >= 5) %>%
  arrange(-avg_speed) 









