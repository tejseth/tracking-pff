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
library(ggcorrplot)
library(lme4)
library(merTools)
library(shiny)
library(shinythemes)
library(gt)
library(ggrepel)

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
gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "white", weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "white",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "middle",
      ...
    )}

speed_projs <- read_csv(url("https://raw.githubusercontent.com/tejseth/tracking-pff/master/speed_projs.csv"))
combine_data <- read_csv(url("https://raw.githubusercontent.com/tejseth/tracking-pff/master/combine_data.csv"))
pro_day_data <- read_csv(url("https://raw.githubusercontent.com/tejseth/tracking-pff/master/pro_day_data.csv"))

speed_projs <- speed_projs %>%
  dplyr::select(-starts_with(".."))

combine_data <- combine_data %>%
  dplyr::select(-starts_with(".."))

pro_day_data <- pro_day_data %>%
  dplyr::select(-starts_with(".."))

speed_projs_filtered <- speed_projs %>%
  filter(seconds_before_contact >= 0.5 & seconds_before_contact <= 5.7)

speed_projs_filtered <- speed_projs_filtered %>%
  group_by(player) %>%
  mutate(total_rushes = n()) %>%
  filter(total_rushes >= 50) %>%
  ungroup() 

rushers <- unique(speed_projs_filtered$player)
seasons <- unique(speed_projs_filtered$season)

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

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

season_speed_to_40 <- speed_projs_filtered %>%
  group_by(player, player_id, season, offense) %>%
  summarize(rushes = n(),
            exp_speed = mean(exp_speed),
            avg_speed = mean(avg_speed),
            avg_speed_oe = mean(speed_oe),
            avg_ybc = mean(ybc)) %>%
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

options(shiny.usecairo=T)

ui <- fluidPage(
  
  titlePanel("Running Back Adjusted Forty"),
  
  mainPanel(
    navbarPage("By Tej Seth",
               tabPanel("Player Breakdown",
                        fluidRow(
                          column(4, align = "center",
                                 selectInput("player_1",
                                             "Player", 
                                             c(sort(unique(as.character(rushers)))), selected = "Aaron Jones"),
          ),
                          column(7, align = "center",
                                 selectInput("season_1",
                                             "Season", 
                                             c(sort(unique(as.character(seasons)))), selected = "2020"))
        ),
        mainPanel(
          tableOutput(outputId = "rusher_table"), 
          br(), 
          plotOutput(outputId = "season_graph",
                     width = "750px", height = "500px"),
        ),
        ),
        tabPanel("Season Breakdown",
                 fluidRow(
                   column(4, align = "center",
                          selectInput("season_2",
                                      "Season", 
                                      c(sort(unique(as.character(seasons)))), selected = "2020"),
                      ),
                   column(7, aling = "center",
                          sliderInput(
                            inputId =  "min_rushes",
                            label = "Minimum Rushes:",
                            min = 1, max = 200,
                            value = 100
                          ),)
                 ),
                 plotOutput(outputId = "the_season_graph",
                            width = "750px", height = "500px"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 tableOutput(outputId = "season_table"))
      )
    )
  )

server <- function(input, output) {
  
  output$rusher_table <- render_gt({
    
    rusher_tab <- season_speed_to_40 %>%
      filter(player == input$player_1)
    
    rusher_tab <- rusher_tab %>%
      left_join(teams_logos_select, by = c("offense" = "team_abbr"))
    
    rusher_tab <- rusher_tab %>%
      arrange(season) %>%
      ungroup() %>%
      dplyr::select(player, season, team_logo_espn, rushes, speed_perc, adj_twenty, adj_forty) %>%
      mutate(speed_perc = round(speed_perc, 1),
             adj_twenty = round(adj_twenty, 2),
             adj_forty = round(adj_forty, 2))
    
    rusher_tab %>% gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        player = "Player",
        season = "Season",
        team_logo_espn = "Team",
        rushes = "Rushes",
        speed_perc = "Speed Percentile",
        adj_twenty = "Updated 20 Split",
        adj_forty = "Updated 40 Time") %>%
      data_color(
        columns = c(adj_forty),
        colors = scales::col_numeric(
          palette = c("#085D29", "#FD8C24", "#D70915"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      tab_header(
        title = md(paste0(input$player_1, "'s Adjusted 40 Time by Season")),
      ) %>%
      gt_theme_538()
  }, width = 850)
  
  output$season_graph <-  renderPlot({
    
    player_games <- game_speed_to_40 %>%
      filter(player == input$player_1) %>%
      filter(season == input$season_1)
    the_weeks <- unique(player_games$week)
    league_games <- game_speed_to_40 %>%
      filter(player != input$season_1) %>%
      filter(season == input$season_1) %>%
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
           title = paste0(input$player_1, "'s Game-Adjusted 40 Yard Dash Time, ", input$season_1),
           subtitle = "Black dots listed for every other rusher with at least 10 rushes")
  },  height = 600, width = 850)
  
  output$the_season_graph <-  renderPlot({
    
    graph_data <- season_speed_to_40 %>%
      filter(season == input$season_2) %>%
      filter(rushes >= input$min_rushes) %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))
    
    graph_data %>%
      ggplot(aes(x = adj_forty, y = avg_ybc)) +
      geom_smooth(method = "lm", size = 1.5, se = FALSE, color = "black") +
      geom_hline(yintercept = mean(graph_data$avg_ybc), linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = mean(graph_data$adj_forty), linetype = "dashed", alpha = 0.5) +
      geom_point(aes(size = rushes, fill = team_color, color = team_color2), shape = 21) +
      geom_text_repel(aes(label = player), size = 5, box.padding = 0.35) +
      theme_reach() +
      scale_color_identity(aesthetics = c("fill", "color")) +
      labs(x = "Updated 40 Time",
           y = "Average Yards Before Contact",
           title = paste0("Yards Before Contact and Adjusted 40 Time in ", input$season_2),
           subtitle = paste0("Minimum of ", input$min_rushes, " rushes, bubble size is amount of runs")) +
      scale_x_reverse(breaks = scales::pretty_breaks(n = 6)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
    
  },  height = 600, width = 850)
  
  output$season_table <- render_gt({

    graph_data <- season_speed_to_40 %>%
      filter(season == input$season_2) %>%
      filter(rushes >= input$min_rushes) %>%
      left_join(teams_colors_logos, by = c("offense" = "team_abbr"))

    tab <- graph_data %>%
      arrange(adj_forty) %>%
      mutate(rank = row_number()) %>%
      dplyr::select(rank, player, season, team_logo_espn, rushes, avg_ybc, adj_forty)
    
    tab <- tab %>%
      mutate_if(is.numeric, ~round(., 2))

      tab %>% gt() %>%
      text_transform(
        locations = cells_body(c(team_logo_espn)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(
        rank = "Rank",
        player = "Player",
        season = "Season",
        rushes = "Rushes",
        team_logo_espn = "Team",
        rushes = "Rushes",
        avg_ybc = "Yards Before Contact",
        adj_forty = "Updated 40 Time") %>%
      data_color(
        columns = c(adj_forty),
        colors = scales::col_numeric(
          palette = c("#085D29", "#FD8C24", "#D70915"),
          domain = NULL
        )
      ) %>%
      opt_align_table_header(align = "center") %>%
      tab_header(
        title = md(paste0("Each Running Back's Updated 40 Time in ", input$season_2)),
      ) %>%
      opt_row_striping() %>%
      gt_theme_538()
  }, width = 850)

}

shinyApp(ui = ui, server = server)




