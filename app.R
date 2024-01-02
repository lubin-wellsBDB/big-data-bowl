library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)

ui <- fluidPage(
  titlePanel("Adjustable Tackle Metric Hub", div(style = "margin-top: -20px; margin-bottom: 20px;")),
  
  # Sliders, Reset button, and Download button in the same row
  fluidRow(
    column(
      width = 6,
      # Position selection
      selectInput("position", "Select Position", choices = c("All", "DE", "DT", "OLB", "ILB", "CB", "Safety")),
    ),
    column(
      width = 3,
      offset = -11,
      # Add a reset button for the weights
      actionButton("reset_weights", "Reset Weights")
    ),
    column(
      width = 3,
      # Add a download button for the result table
      downloadButton("download_btn", "Download Results")
    )
  ),
  
  # Sliders in the same row
  fluidRow(
    column(
      width = 3,
      sliderInput("weight_a", "STRAIN", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3, 
      sliderInput("weight_b", "Tackling Value", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3,
      sliderInput("weight_c", "Athleticism", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3, 
      sliderInput("weight_d", "Allowed Yards After Contact", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3, 
      sliderInput("weight_e", "Stamina", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3, 
      sliderInput("weight_f", "Force", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3, 
      sliderInput("weight_g", "Tackling Over Expectation", min = 0, max = 10, value = 5, width = "100%"),
    ),
    column(
      width = 3, 
      sliderInput("weight_h", "Tackle Range", min = 0, max = 10, value = 5, width = "100%"),
    )
  ),
  
  # Main panel for the result table
  mainPanel(
    DTOutput("result_table", width = "100%", height = "400px"), 
    
    div(style = "height: 20px;") 
  )
)


# Define server logic
server <- function(input, output, session) {
  
  all_data <- read.csv("Data/all_normalized_data2.csv")
  de_data <- read.csv("Data/de_data.csv")
  dt_data <- read.csv("Data/dt_data.csv")
  olb_data <- read.csv("Data/olb_data.csv")
  ilb_data <- read.csv("Data/ilb_data.csv")
  cb_data <- read.csv("Data/cb_data.csv")
  safety_data <- read.csv("Data/safety_data.csv")
  
  
  # Original weights
  original_weights <- c(5, 5, 5, 5, 5, 5, 5, 5)
  
  # Function to calculate the final metric
  calculate_metric <- function(position_data, weights) {
    position_data %>%
      mutate(final_metric = avgImpulse * (weights[6] / 10) + avg_tackle_oe * (weights[7] / 10) +
               avgSTRAIN * (weights[1] / 10) + mean_tackle_value * (weights[2] / 10) +
               athleticism_score * (weights[3] / 10) + avgAyac * (weights[4] / 10) +
               fatigue * (weights[5] / 10) + tackle_range_metric * (weights[8] / 10)) %>%
      select(displayName, final_metric)
  }
  
  # Reactive expression for selected data
  selected_data <- reactive({
    switch(input$position,
           "All" = all_data,
           "DT" = dt_data,
           "DE" = de_data,
           "OLB" = olb_data,
           "ILB" = ilb_data,
           "CB" = cb_data,
           "Safety" = safety_data
           )
  })
  
  # Reactive expression for calculated metric
  calculated_metric <- reactive({
    calculate_metric(selected_data(), c(input$weight_a, input$weight_b, input$weight_c,
                                        input$weight_d, input$weight_e, input$weight_f, input$weight_g, input$weight_h))
  })
  
  # Display a clean DataTable
  output$result_table <- renderDT({
    result_data <- calculated_metric()
    
    # Find the top and bottom 10 values
    top_10 <- head(result_data[order(result_data$final_metric, decreasing = TRUE), ], 10)
    bottom_10 <- head(result_data[order(result_data$final_metric), ], 10)
    
    # Combine the results into a data frame
    result_df <- data.frame("Top 10" = top_10$displayName, "Bottom 10" = bottom_10$displayName)
    
    # Create a DataTable with a different theme and customized column titles
    datatable(
      result_df,
      options = list(
        lengthChange = FALSE,
        dom = 't',
        ordering = FALSE
      ),
      # Customize column titles
      colnames = c("Top 10 Players", "Bottom 10 Players")
    )
  })
  
  # Add reset functionality for the weights
  observeEvent(input$reset_weights, {
    # Reset weights to the original values
    updateSliderInput(session, "weight_a", value = original_weights[1])
    updateSliderInput(session, "weight_b", value = original_weights[2])
    updateSliderInput(session, "weight_c", value = original_weights[3])
    updateSliderInput(session, "weight_d", value = original_weights[4])
    updateSliderInput(session, "weight_e", value = original_weights[5])
    updateSliderInput(session, "weight_f", value = original_weights[6])
    updateSliderInput(session, "weight_g", value = original_weights[7])
    updateSliderInput(session, "weight_h", value = original_weights[8])
  })
  
  # Add download functionality for the result table
  output$download_btn <- downloadHandler(
    filename = function() {
      paste("player_metrics_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calculated_metric(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui, server)
