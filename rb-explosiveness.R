play_speed_all_18 <- read.csv("~/tracking-pff/play-speed-year/play_speed_all_18.csv")
play_speed_all_19 <- read.csv("~/tracking-pff/play-speed-year/play_speed_all_19.csv")
play_speed_all_20 <- read.csv("~/tracking-pff/play-speed-year/play_speed_all_20.csv")
run_blocking <- pull_s3(paste0("analytics/projections/by_facet/", 'nfl', "/%i/run_blocking.csv.gz"), season_start = 2018, season_end = 2020)

play_speed_all <- rbind(play_speed_all_18, play_speed_all_19, play_speed_all_20)

play_speed_all <- play_speed_all %>%
  dplyr::select(gameid, playid, player, avg_speed, seconds_before_contact, ybc)

#write.csv(play_speed_all, "play_speed_all.csv")

speed_projs <- read.csv("~/tracking-pff/speed_projs.csv")
stadiums <- pull_s3("flat_files/NFL_Stadiums.csv", bucket = "ml")

summary(lm(ybc ~ avg_speed, data = play_speed_all))$r.squared #0.15
summary(lm(ybc ~ seconds_before_contact, data = play_speed_all))$r.squared #0.31
summary(lm(ybc ~ (avg_speed + seconds_before_contact)^2, data = play_speed_all))

play_speed_all %>%
  ggplot(aes(x = avg_speed, y = ybc)) +
  geom_jitter(alpha = 0.5, size = 1.5) +
  geom_smooth(size = 2, color = "darkorange") +
  theme_reach() +
  labs(x = "Average Speed",
       y = "Yards Before Contact",
       title = "Higher Speed Before Contact Leads to Higher Yards Before Contact On a Play",
       subtitle = "2017-2020, Weeks 1-17") +
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
       subtitle = "2017-2020, Weeks 1-17") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  annotate("text", x = 3, y = 50, label = "R^2 = 0.31", size = 5)

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

rushing_data_speed %>%
  filter(distance < 15) %>%
  filter(down %in% c(1, 2, 3)) %>%
  group_by(down, distance) %>%
  summarize(rushes = n(),
            avg_speed = mean(avg_speed)) %>%
  #filter(avg_speed > 4 & avg_speed < 6) %>%
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

rushing_data_speed %>%
  mutate(`Turf Indicator`  = as.factor(turf)) %>%
  ggplot(aes(x = as.factor(dome), y = avg_speed, fill = `Turf Indicator`)) +
  geom_violin(aes(color = `Turf Indicator`), fill = "white") +
  geom_boxplot(aes(fill = `Turf Indicator`)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_reach() +
  labs(x = "Dome Indicator",
       y = "Average Speed",
       title = "How Playing in a Dome and on Turf Impacts Rushing Speed",
       subtitle = "2018-2020, weeks 1-17",
       fill = "Turf Indicator") +
  theme(legend.position = "bottom")








