library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)

# Assuming Merged_Risk_Premium is already loaded and transformed
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
      # Increase size of the plot output
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
    p <- ggplot(year_data, aes(x = Sev_, y = Freq_, color = Statutory_Class, label = Statutory_Class)) +
      geom_point(size = 4, alpha = 0.8) +  # Increased point size for better visibility
      geom_text_repel(
        aes(label = Statutory_Class), 
        size = 3, 
        box.padding = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        max.overlaps = 10
      ) +
      scale_color_viridis_d() +
      labs(
        title = paste("Scatter Plot of Severity vs Frequency for the Year", input$yearInput),
        x = "Severity",
        y = "Frequency"
      ) +
      theme_minimal() +
      theme(legend.position = "none")  # Hide the legend as labels are directly on the plot
    
    # Convert to interactive plotly object
    ggplotly(p, tooltip = c("text"))  # tooltip modified to display the class name
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
