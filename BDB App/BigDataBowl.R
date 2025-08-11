#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(tidyverse)
library(janitor)
library(reactable)
library(gt)
library(ggplot2)
library(stringr)
library(aws.s3)
library(arrow)
library(rsconnect)
library(httr2)
library(log4r)

#Data Read
players <- read_csv("players.csv") |> clean_names()
plays <- read_csv("plays.csv") |> clean_names()
player_play <- read_csv("player_play.csv") |> clean_names()
games <- read_csv("games.csv") |> clean_names()

run_plays <- player_play |>
  filter(had_rush_attempt == 1) |>
  left_join(players |> select(nfl_id, display_name, position), by = "nfl_id") |>
  left_join(
    plays |> select(game_id, play_id, down, yards_to_go, yards_gained,
                     offense_formation, rush_location_type, pff_run_concept_primary),
    by = c("game_id", "play_id")
  ) |>
  left_join(games |> select(game_id, week), by = "game_id") |>
  filter(!is.na(display_name))

run_plays <- run_plays |>
  mutate(
    required_yards = case_when(
      down == 1 ~ 0.4 * yards_to_go,
      down == 2 ~ 0.5 * yards_to_go,
      TRUE ~ yards_to_go
    ),
    successful_run = yards_gained >= required_yards,
    run_location_pretty = case_when(
      is.na(rush_location_type) ~ NA_character_,
      TRUE ~ gsub("_", " ", str_to_title(str_to_lower(rush_location_type)))
    ),
    offense_formation_pretty = case_when(
      is.na(offense_formation) ~ NA_character_,
      TRUE ~ gsub("_", " ", str_to_title(str_to_lower(offense_formation)))
    ),
    pff_run_concept_pretty = case_when(
      is.na(pff_run_concept_primary) ~ NA_character_,
      TRUE ~ gsub("_", " ", str_to_title(str_to_lower(pff_run_concept_primary)))
    )
  )

set.seed(1)
run_plays$pred_success <- runif(nrow(run_plays), 0.3, 0.7)

#App CSS formatting
nfl_css <- HTML('
body {
  background: #000 !important;
  color: #fff;
  font-family: "Segoe UI", "Arial Black", Arial, sans-serif;
}
.navbar, .navbar-default {
  background: #000 !important;
  color: #fff;
}
#app-title-box {
  background: #000;
  padding: 18px;
  border-bottom: 4px solid #cc0000;
  display: flex;
  align-items: center;
  gap: 25px;
}
#app-title-box .nfl-logo {
  width: 60px; height: 60px; margin-right:12px; border-radius:11px;
  background: #fff;
  box-shadow: 0 1px 8px #000b;
  padding: 3px;
}
#app-title-box .title-main {
  color: #fff;
  font-size: 2.4rem;
  font-weight: 700;
  letter-spacing: 1px;
  line-height: 1;
  margin-bottom: 4px;
}
#app-title-box .title-sub {
  color: #D50A0A;
  font-size: 1.2rem;
  font-weight: 600;
  letter-spacing: 0.5px;
}
.sidebar {
  background: #000 !important;
  border-radius: 9px;
  box-shadow: 0 1px 8px #18191a3b !important;
}
#sidebarPanel {
  background: #444444;
  border-radius: 9px;
  box-shadow: 0 1px 8px #18191a3b !important;
  color: #fff;
  font-weight: 500;
  padding: 24px;
  margin-bottom: 12px;
}
.form-check-input:checked {
  background-color: #D50A0A !important;
  border-color: #D50A0A !important;
}
.selectize-input, .form-control, .shiny-input-container input {
  background: #fff;
  color: #000;
  border-radius: 6px !important;
}
.selectize-dropdown-content {
  background: #000;
  color: #fff;
}
.shiny-download-link {
  background: #D50A0A;
  color: #fff !important;
  border-radius: 6px;
  padding: 7px 15px;
  border: none;
  font-weight: 600;
  font-size: 1rem;
  margin-top: 10px;
}
.shiny-download-link:hover {
  background: #D50A0A;
}
.nav-tabs { background: #000; }
.nav-tabs .nav-link.active {
  background: #D50A0A;
  color: #fff !important;
  font-weight: 700;
  border-radius: 7px 7px 0 0;
  border: none !important;
}
.nav-tabs .nav-link {
  color: #fff !important;
  font-weight: 600;
  background: #000;
  margin-right: 2px;
}
hr {
  border-top: 2px solid #f00;
  opacity: 0.5;
}
.reactable, .reactable-table, .table, .gt-table {
  background: #000 !important;
  color: #fff !important;
  font-size: 0.98rem;
  border-radius: 8px !important;
}
.reactable-thead, .reactable-header {
  background: #000 !important;
  color: #ffffff !important;
  font-weight: 700;
  border-radius: 8px 8px 0 0 !important;
}
.reactable tr:nth-child(odd) { background: #000 !important; }
.reactable tr:nth-child(even) { background: #000 !important; }
.gt-table thead { background: #000 !important; }
h3, .tab-content h3 { color: #D50A0A; font-weight:900; letter-spacing:1px; }
.shiny-output-error { color: #D50A0A; }
input[type="range"]::-webkit-slider-thumb {
  background: #D50A0A;
}
input[type="range"]::-moz-range-thumb {
  background: #D50A0A;
}
input[type="range"]::-ms-thumb {
  background: #D50A0A;
}
/* ====== Success Report filter styling ====== */
.success-report-label {
  color: #D50A0A !important;
  font-size: 1.12rem;
  font-weight: bold;
  letter-spacing: 1px;
  margin-bottom: 4px;
  display: block;
}
#sr_down, #sr_player {
  border: 2px solid #D50A0A !important;
  border-radius: 5px !important;
  font-weight: 600 !important;
  background: #fff !important;
  color: #000 !important;
}
/* Slider track accent for yards to go */
.js-range-slider .irs-bar {
  background: #D50A0A !important;
  border-radius: 4px;
}
.js-range-slider .irs-single,
.js-range-slider .irs-from,
.js-range-slider .irs-to {
  background: #D50A0A !important;
  color: #fff !important;
  font-weight: bold;
  border-radius: 4px;
}
')

# DROPDOWN SPECIAL STYLING (Input black/white, dropdown list white on dark)
drop_css <- HTML('
.selectize-input {
  background: #fff !important;
  color: #000 !important;
}
.selectize-dropdown, .selectize-dropdown-content {
  background: #18191a !important;
  color: #fff !important;
}
.selectize-dropdown .active {
  background: #D50A0A !important;
  color: #000 !important;
}
.reactable input[type="text"],
.reactable input[type="search"],
.reactable select {
  color: #000 !important;
  background: #fff !important;
  font-weight: 600;
}
')

#NFL logo
nfl_logo_url <- "https://a.espncdn.com/i/teamlogos/leagues/500/nfl.png"

ui <- fluidPage(
  tags$head(tags$style(nfl_css)),
  tags$head(tags$style(drop_css)),
  div(id="app-title-box",
      tags$img(src = nfl_logo_url, class="nfl-logo"),
      div(
        span("NFL Big Data Bowl: Player Run Analysis", class="title-main"), br(),
        span("Interactive Rushing Analytics Dashboard", class="title-sub")
      )
  ),
  sidebarLayout(
    sidebarPanel(
      id="sidebarPanel",
      checkboxGroupInput(
        "positions",
        "Select positions to include:",
        choices = NULL,
        selected = NULL
      ),
      selectInput("player", "Choose Runner:", choices = NULL),
      downloadButton("download", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Player Analysis",
                 h3(textOutput("summaryTitle")),
                 tableOutput("playerSummary"),
                 plotOutput("yardDistPlot"),
                 reactableOutput("detailTable")
        ),
        tabPanel("Success Report",
                 br(),
                 fluidRow(
                   column(4,
                          tags$span("Yards to Go (Range):", class = "success-report-label"),
                          sliderInput("sr_ydstogo", label = NULL,
                                      min = floor(min(run_plays$yards_to_go, na.rm=TRUE)),
                                      max = ceiling(max(run_plays$yards_to_go, na.rm=TRUE)),
                                      value = c(1, 10))
                   ),
                   column(2,
                          tags$span("Down:", class = "success-report-label"),
                          selectInput("sr_down", label = NULL,
                                      choices = c("All", sort(unique(run_plays$down))), selected = "All")
                   )
                 ),
                 br(),
                 reactableOutput("conceptSuccessTable")
        ),
        tabPanel("Prediction",
                 h3("NFL Rushing Play Success Prediction"),
                 numericInput("pred_down", "Down:", 1, min=1, max=4),
                 numericInput("pred_yards_to_go", "Yards to Go:", 10, min=1, max=99),
                 numericInput("pred_yardline", "Absolute Yardline:", 45, min=1, max=99),
                 selectInput("pred_offense_formation", "Offense Formation:",
                             choices = c("SINGLEBACK", "SHOTGUN", "I_FORM", "PISTOL", "JUMBO", "WILDCAT")),
                 selectInput("pred_location_type", "Rush Location:",
                             choices = c("INSIDE_RIGHT", "INSIDE_LEFT", "OUTSIDE_RIGHT", "OUTSIDE_LEFT")),
                 numericInput("pred_expected_points", "Expected Points:", 2.43, min=-10, max=10),
                 numericInput("pred_score_diff", "Score Difference:", 0, min=-100, max=100),
                 selectInput("pred_run_concept", "Run Concept:",
                             choices = c("TRAP", "ZONE", "POWER", "COUNTER")),
                 actionButton("go_button", "Predict Success Probability", class = "btn-primary"),
                 br(), br(),
                 verbatimTextOutput("prediction"),
                 textOutput("error_msg")
        )
      )
    )
  )
)

log <- log4r::logger()
log4r::info(log, "Shiny NFL Rushing App Started")

server <- function(input, output, session) {
  observe({
    pos_choices <- run_plays |>
      filter(!is.na(position)) |>
      arrange(position) |>
      pull(position) |>
      unique()
    updateCheckboxGroupInput(
      session, "positions",
      choices = pos_choices,
      selected = pos_choices
    )
  })
  
  runner_choices <- reactive({
    pos_selected <- input$positions
    if (is.null(pos_selected) || length(pos_selected) == 0) return(character(0))
    run_plays |>
      filter(position %in% pos_selected) |>
      pull(display_name) |>
      unique()
  })
  
  observe({
    updateSelectInput(session, "player", choices = sort(runner_choices()))
  })
  
  player_data <- reactive({
    req(input$player, input$positions)
    run_plays |>
      filter(display_name == input$player, position %in% input$positions)
  })
  
  output$playerSummary <- renderTable({
    req(player_data())
    tibble(
      "Games Played" = length(unique(player_data()$game_id)),
      "Attempts"     = nrow(player_data()),
      "Avg Yards"    = mean(player_data()$yards_gained, na.rm = TRUE),
      "Success Rate (%)" = round(mean(player_data()$successful_run, na.rm = TRUE)*100, 1),
      "Predicted Success Rate (%)" = round(mean(player_data()$pred_success, na.rm = TRUE)*100, 1)
    )
  })
  
  output$summaryTitle <- renderText({
    paste("Run Summary for", input$player)
  })
  
  output$yardDistPlot <- renderPlot({
    pd <- player_data()
    if (nrow(pd) == 0 || all(is.na(pd$yards_gained))) return(NULL)
    ggplot(pd, aes(yards_gained)) +
      geom_histogram(binwidth = 1, fill = "#D50A0A", color = "#ffffff", alpha = 0.9) +
      labs(x = "Yards Gained", y = "Number of Runs",
           title = paste("Yard Gain Distribution (", input$player, ")")) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#000000", color = NA),
        panel.background = element_rect(fill = "#000000", color = NA),
        panel.grid.major = element_line(color = "#444444"),
        axis.text = element_text(color = "#ffffff"),
        axis.title = element_text(color = "#ffffff"),
        plot.title = element_text(color = "#D50A0A")
      )
  })
  
  output$detailTable <- renderReactable({
    req(player_data())
    reactable(
      player_data() |>
        select(
          game_id, week, play_id, down, yards_to_go, yards_gained,
          offense_formation_pretty, run_location_pretty,
          successful_run, pred_success, pff_run_concept_pretty
        ),
      columns = list(
        game_id = colDef(name = "Game ID"),
        week = colDef(name = "Week"),
        down = colDef(name = "Down"),
        play_id = colDef(name = "Play ID"),
        yards_to_go = colDef(name = "Yards To Go"),
        yards_gained = colDef(name = "Yards Gained"),
        offense_formation_pretty = colDef(name = "Formation"),
        run_location_pretty = colDef(name = "Run Direction"),
        pff_run_concept_pretty = colDef(name = "Run Concept"),
        successful_run = colDef(
          name = "Successful Run",
          cell = function(value) ifelse(value, "Yes", "No")
        ),
        pred_success = colDef(
          name = "Predicted Success Probability",
          cell = function(value) {
            if (is.na(value)) return("")
            sprintf("%.2f%%", 100 * value)
          },
          align = "right"
        )
      ),
      theme = reactableTheme(
        backgroundColor = "#000000",
        color = "#ffffff",
        borderColor = "#000000",
        stripedColor = "#000000",
        highlightColor = "#444444",
        tableStyle = list(fontSize = "1rem"),
        headerStyle = list(
          background = "#000000",
          color = "#D50A0A",
          fontWeight = "bold",
          fontSize = "1.1rem",
          borderBottom = "2px solid #e11d26",
          letterSpacing = "1px"
        ),
        inputStyle = list(
          background = "#ffffff",
          color = "#000000",
          border = "1px solid #e11d26"
        ),
        filterInputStyle = list(
          background = "#ffffff",
          color = "#000000"
        ),
        searchInputStyle = list(
          background = "#ffffff",
          color = "#000000"
        ),
        selectStyle = list(
          background = "#ffffff",
          color = "#000000"
        )
      ),
      searchable = TRUE,
      filterable = TRUE,
      pagination = TRUE,
      defaultSorted = list(week = "asc", play_id = "asc")
    )
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("run_data_", gsub(" ", "_", input$player), ".csv")
    },
    content = function(file) {
      write.csv(player_data(), file, row.names = FALSE)
    }
  )
  
  observe({
    updateSelectInput(
      session, "sr_down",
      choices = c("All", sort(unique(run_plays$down))),
      selected = input$sr_down
    )
  })
  
  report_data <- reactive({
    d <- run_plays |>
      filter(
        yards_to_go >= input$sr_ydstogo[1],
        yards_to_go <= input$sr_ydstogo[2]
      )
    if(input$sr_down != "All") d <- d |> filter(down == input$sr_down)
    d
  })
  
  output$conceptSuccessTable <- renderReactable({
    req(report_data())
    report_data() |>
      group_by(
        pff_run_concept_pretty,
        offense_formation_pretty,
        run_location_pretty
      ) |>
      summarize(
        Attempts = n(),
        Success_Rate = mean(successful_run, na.rm = TRUE),
        Avg_Yards = mean(yards_gained, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(Attempts >= 5) |>
      arrange(desc(Success_Rate)) |>
      reactable(
        columns = list(
          pff_run_concept_pretty = colDef(name = "Run Concept"),
          offense_formation_pretty = colDef(name = "Formation"),
          run_location_pretty = colDef(name = "Run Direction"),
          Success_Rate = colDef(
            name = "Success Rate (%)",
            format = colFormat(percent = TRUE, digits = 1)
          ),
          Avg_Yards = colDef(
            name = "Avg Yards",
            cell = function(value) {
              if (is.na(value)) "" else sprintf("%.2f", value)
            }
          ),
          Attempts = colDef(name = "Attempts")
        ),
        theme = reactableTheme(
          backgroundColor = "#000000",
          color = "#ffffff",
          borderColor = "#000000",
          stripedColor = "#000000",
          highlightColor = "#444444",
          tableStyle = list(fontSize = "1rem"),
          headerStyle = list(
            background = "#000000",
            color = "#D50A0A",
            fontWeight = "bold",
            fontSize = "1.1rem",
            borderBottom = "2px solid #e11d26",
            letterSpacing = "1px"
          ),
          inputStyle = list(
            background = "#ffffff",
            color = "#000000",
            border = "1px solid #e11d26"
          ),
          filterInputStyle = list(
            background = "#ffffff",
            color = "#000000"
          ),
          searchInputStyle = list(
            background = "#ffffff",
            color = "#000000"
          ),
          selectStyle = list(
            background = "#ffffff",
            color = "#000000"
          )
        ),
        searchable = TRUE,
        filterable = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        defaultSorted = list(Success_Rate = "desc")
      )
  })
  api_url <- "http://13.58.8.97:8080/predict"
  
  observeEvent(input$go_button, {
    validate(
      need(!is.null(input$pred_down) && input$pred_down != "", "Please enter a value for Down."),
      need(!is.null(input$pred_yards_to_go) && input$pred_yards_to_go != "", "Please enter Yards To Go."),
      need(!is.null(input$pred_yardline) && input$pred_yardline != "", "Please enter the Absolute Yardline."),
      need(!is.null(input$pred_offense_formation) && input$pred_offense_formation != "", "Please select Offense Formation."),
      need(!is.null(input$pred_location_type) && input$pred_location_type != "", "Please select Rush Location."),
      need(!is.null(input$pred_expected_points) && input$pred_expected_points != "", "Please enter Expected Points."),
      need(!is.null(input$pred_score_diff) && input$pred_score_diff != "", "Please enter Score Difference."),
      need(!is.null(input$pred_run_concept) && input$pred_run_concept != "", "Please select Run Concept.")
    )
    req_data <- list(
      list(
        down = as.character(input$pred_down),  # API expects string
        yards_to_go = input$pred_yards_to_go,
        absolute_yardline_number = input$pred_yardline,
        offense_formation = input$pred_offense_formation,
        rush_location_type = input$pred_location_type,
        expected_points = input$pred_expected_points,
        score_diff = input$pred_score_diff,
        pff_run_concept_primary = input$pred_run_concept
      )
    )
    
    tryCatch({
      log4r::info(log, "Prediction Requested")
      req <- request(api_url) |>
        req_method("POST") |>
        req_body_json(req_data)
      log4r::info(log, "Prediction Returned")
      
      resp <- req_perform(req)
      pred <- resp_body_json(resp)
      
      output$prediction <- renderText({
        paste("Predicted Success Probability:", round(pred[[1]], 3))
      })
      output$error_msg <- renderText({ "" })
    }, error = function(e) {
      log4r::error(log, paste("HTTP Error"))
      output$prediction <- renderText({ "" })
      output$error_msg <- renderText({ paste("Error:", e$message) })
      
    })
  })
}

shinyApp(ui, server)
