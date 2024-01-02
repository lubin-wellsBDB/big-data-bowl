library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)

de <- read.csv("de_data_2.csv")
dt <- read.csv("dt_data_2.csv")
olb <- read.csv("olb_data_2.csv")
ilb <- read.csv("ilb_data_2.csv")
cb <- read.csv("cb_data_2.csv")
safety <- read.csv("safety_data_2.csv")

positions_data <- list(
  DE = de,
  DT = dt,
  OLB = olb,
  ILB = ilb,
  CB = cb,
  Safety = safety
)

# Assign processed data frames back to the positions_data list
positions_data <- lapply(positions_data, function(position_data) {
  position_data$last_name <- sapply(strsplit(position_data$displayName, " "), function(x) tail(x, 1))
  position_data <- position_data[order(position_data$last_name, position_data$displayName), ]
  return(position_data)
})

ui <- dashboardPage(
  dashboardHeader(title = "TacklrTrackr"),
  dashboardSidebar(
    selectInput("positionSelect", "Select a Position", choices = names(positions_data)),
    selectInput("playerSearch", "Select a Player", choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Player Metrics Radar Chart",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("radarChart")
      ),
      box(
        title = "Similar Players Table",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        tableOutput("similarPlayersTable")
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    # Update player choices based on the selected position
    updateSelectInput(session, "playerSearch", choices = positions_data[[input$positionSelect]]$displayName)
  })
  
  output$radarChart <- renderPlotly({
    req(input$positionSelect, input$playerSearch)
    
    # Get the selected position-specific data
    selected_position_data <- positions_data[[input$positionSelect]]
    
    # Check if the selected player exists in the current position's data
    if (input$playerSearch %in% selected_position_data$displayName) {
      selected_player <- selected_position_data[selected_position_data$displayName == input$playerSearch, ]
      
      radar_data <- selected_player[, c("avgImpulse", "avg_tackle_oe",
                                        "avgSTRAIN", "mean_tackle_value",
                                        "fatigue", "athleticism_score",
                                        "avgAyac", "tackle_range_metric")]
      
      colnames(radar_data) <- c("Force", "Tackling Over Expectation",
                                "STRAIN", "Tackle Value",
                                "Stamina", "Athleticism",
                                "Allowed Yards After Contact", "Tackling Range")
      
      plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        fill = 'toself',
        r = as.numeric(unlist(radar_data)),
        theta = colnames(radar_data),
        line = list(color = 'blue')
      ) %>%
        layout(
          title = paste("Player Metrics Radar Chart -", input$playerSearch),
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          legend = FALSE,
          dragmode = "lasso",
          margin = list(t = 50, l = 20, r = 20) 
        )
    } else {
      # Handle the case where the selected player doesn't exist in the current position's data
      plot_ly(
        type = 'scatter',
        mode = 'markers'
      ) 
    }
  })
  
  output$similarPlayersTable <- renderTable({
    req(input$positionSelect, input$playerSearch)
    
    # Get the selected position-specific data
    selected_position_data <- positions_data[[input$positionSelect]]
    
    # Check if the selected player exists in the current position's data
    if (input$playerSearch %in% selected_position_data$displayName) {
      selected_player <- selected_position_data[selected_position_data$displayName == input$playerSearch, ]
      
      selected_metrics <- selected_player[, c("avgImpulse", "avg_tackle_oe",
                                              "avgSTRAIN", "mean_tackle_value",
                                              "athleticism_score", "avgAyac",
                                              "fatigue", "tackle_range_metric")]
      
      distances <- apply(selected_position_data[, c("avgImpulse", "avg_tackle_oe",
                                                    "avgSTRAIN", "mean_tackle_value",
                                                    "athleticism_score", "avgAyac",
                                                    "fatigue", "tackle_range_metric")], 1, function(row) {
                                                      sqrt(sum((selected_metrics - row)^2))
                                                    })
      similar_players <- data.frame(player_name = selected_position_data$displayName, distance = distances)
      
      similar_players <- similar_players[order(similar_players$distance), ]
      similar_players <- similar_players[2:4, ]
      
      # Calculate percentage similarity
      similar_players$percent_similarity <- paste0(round(((sqrt(8*100^2) - similar_players$distance) /sqrt(8*100^2)) * 100, 2), "%")
      # Display displayName, position, and percent_similarity
      similar_players <- setNames(similar_players, c("Player Name", "Distance", "Percent Similarity"))
      similar_players[, c("Player Name", "Percent Similarity")]
    }
  })
}

shinyApp(ui, server)
