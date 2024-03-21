library(shiny)
library(ggplot2)
library(dplyr)

# Read the CSV file
data <- read.csv("F:/NEU/2nd Quarter/ALY 6070/Assignment 3 Group/th.csv")

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("PortfolioFrontier: Exploring the Landscape of Investment"),
  
  # Sidebar layout with an input panel and main panel
  sidebarLayout(
    sidebarPanel(
      # Checkbox group input
      radioButtons("radio", label = h3("Radio buttons"),
                   choices = list("Show Table" = "table", "Show Plot" = "plot"),
                   selected = "plot")
    ),
    
    mainPanel(
      uiOutput("dynamicUI")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Dynamically render UI based on radio button selection
  output$dynamicUI <- renderUI({
    if (input$radio == "table") {
      fluidRow(
        column(12, tableOutput("csvTable"))
      )
    } else if (input$radio == "plot") {
      fluidRow(
        column(6, plotOutput("barPlot1")),
        column(6, plotOutput("barPlot2"))
      )
    }
  })
  
  # Output the CSV data as a table
  output$csvTable <- renderTable({
    data
  })
  
  # Plot the data for number of listings by city
  output$barPlot1 <- renderPlot({
    data_by_city_1 <- data %>%
      group_by(city) %>%
      summarise(listings = sum(listings, na.rm = TRUE)) %>%
      arrange(desc(listings)) # Arrange by listings in descending order
    
    # Reorder the city factor levels based on listings
    data_by_city_1$city <- factor(data_by_city_1$city, levels = data_by_city_1$city)
    
    # Create a bar chart for 'listings'
    ggplot(data_by_city_1, aes(x=city, y=listings)) +
      geom_bar(stat="identity", fill="deepskyblue4") +
      labs(x = "City", 
           y = "Number of Listings",
           title = "Bar Chart of Number of Listings by City",
           subtitle = "Comparison Across Cities",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1))
  })
  
  # Plot the data for average median prices by city
  output$barPlot2 <- renderPlot({
    data_by_city_2 <- data %>%
      group_by(city) %>%
      summarise(median_price = mean(median, na.rm = TRUE))
    
    # Create a bar chart for 'median_price'
    ggplot(data_by_city_2, aes(x=reorder(city, -median_price), y=median_price)) +
      geom_bar(stat="identity", fill="deepskyblue4") +
      labs(x = "City", 
           y = "Average Median Price",
           title = "Bar Chart of Average Median Prices",
           subtitle = "Comparison Across Cities",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1))
  })
}


# Run the application
shinyApp(ui = ui, server = server)
