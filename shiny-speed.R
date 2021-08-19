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
game_speed_to_40 <- read_csv(url("https://raw.githubusercontent.com/tejseth/tracking-pff/master/game_speed_to_40.csv"))
season_speed_to_40 <- read_csv(url("https://raw.githubusercontent.com/tejseth/tracking-pff/master/season_speed_to_40.csv"))

speed_projs <- speed_projs %>%
  dplyr::select(-starts_with(".."))

game_speed_to_40 <- game_speed_to_40 %>%
  dplyr::select(-starts_with(".."))

season_speed_to_40 <- season_speed_to_40 %>%
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




