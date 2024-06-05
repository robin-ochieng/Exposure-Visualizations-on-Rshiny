# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(ggrepel)

# Transform data from wide to long format
data_long <- Merged_Risk_Premium %>%
  pivot_longer(
    cols = matches("Freq_|Sev_"),
    names_to = c(".value", "year"),
    names_pattern = "(Freq_|Sev_)(\\d+)"
  )

# Define UI for application
ui <- fluidPage(
  titlePanel("Interactive Plot of Severity vs Frequency"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yearInput", "Select Year:", choices = unique(data_long$year))
    ),
    mainPanel(
      plotlyOutput("yearPlot", height = "550px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$yearPlot <- renderPlotly({
    # Filter data based on selected year
    year_data <- data_long %>% filter(year == input$yearInput)
    
    # Generate plot
    p <- ggplot(year_data, aes(x = Sev_, y = Freq_, label = Statutory_Class)) +
      geom_point(aes(color = Statutory_Class), size = 4, alpha = 0.8) +
      geom_label_repel(size = 3, box.padding = unit(0.35, "lines"),
                       point.padding = unit(0.5, "lines"),
                       label.padding = unit(0.2, "lines")) +
      scale_color_viridis_d() +
      labs(
        title = paste("Scatter Plot of Severity vs Frequency for the year", input$yearInput),
        x = "Severity",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    # Convert to interactive plotly object
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
