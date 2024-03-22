library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)

# Read the CSV file
data <- read.csv("th_new.csv")

# Define UI with a background image
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "body {
          background-image: url('https://resources.pollfish.com/wp-content/uploads/2020/11/MARKET_RESEARCH_FOR_REAL_ESTATE_IN_CONTENT_1.png');
          background-size: cover;
          background-repeat: no-repeat;
          background-attachment: fixed;
          background-position: center;
        }
        #contentArea {
          position: relative;
          background-color: rgba(255, 255, 255, 0.75);
        }"
      )
    )
  ),
  
  # Application title
  titlePanel("PortfolioFrontier: Exploring the Landscape of Investment"),
  
  # Create tabs with an ID for the content area
  div(id = "contentArea", 
      tabsetPanel(
        tabPanel("Dataset", tableOutput("csvTable")),
        tabPanel("City Listings", plotOutput("barPlot1")),
        tabPanel("Median Prices", plotOutput("barPlot4"),plotOutput("barPlot2")),
        tabPanel("Yearly sales", plotOutput("barPlot3")),
        tabPanel("Map", leafletOutput("map"))
      )
  )
)

# Define server logic
server <- function(input, output) {
  
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
  
  # Plot the data for median prices over time
  output$barPlot4 <- renderPlot({
    data_by_city_4 <- data %>%
      group_by(year) %>%
      summarise(median_price = sum(median, na.rm = TRUE))
    
    # Create a line chart for 'median_price'
    ggplot(data_by_city_4, aes(x=year, y=median_price)) +
      geom_line(color = "deepskyblue4", size = 1) +
      geom_point(color = "darkred", size = 3) +  
      geom_smooth(method="lm", se=FALSE, color="black", linetype = "dashed") + 
      geom_text(aes(label=median_price), vjust=0.5, hjust = -0.5, size =2) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", 
           y = "Median",
           title = "Median value over time",
           subtitle = "Annual Sales Over Time",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
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
  
  
  # Line Plot the time series plot of sales
  output$barPlot3 <- renderPlot({
    data_by_year <- data %>%
      group_by(year) %>%
      summarise(sales = sum(sales, na.rm = TRUE)) %>%
      arrange(desc(sales)) # Arrange by listings in descending order
    
    # Create a time series plot for 'sales'
    ggplot(data_by_year, aes(x=year, y=sales)) +
      geom_line(color = "deepskyblue4", size = 1) +
      geom_point(color = "darkred", size = 3) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", 
           y = "Sales",
           title = "Time Series Plot of Sales",
           subtitle = "Annual Sales Over Time",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1))
    
  })
  
  # Create a new data frame with distinct city names and their corresponding latitudes and longitudes
  distinct_cities <- data %>%
    select(city, latitude, longitude) %>%
    distinct()
  
  # Render the map
  output$map <- renderLeaflet({
    # Initialize the map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = mean(distinct_cities$longitude, na.rm = TRUE), lat = mean(distinct_cities$latitude, na.rm = TRUE), zoom = 7) # Set the initial view
    
    # Add markers for each location
    map <- map %>%
      addMarkers(lng = distinct_cities$longitude, lat = distinct_cities$latitude,
                 popup = paste("City:", distinct_cities$city, "<br>",
                               "Longitude:", distinct_cities$longitude, "<br>",
                               "Latitude:", distinct_cities$latitude))
    
    map  # Return the map
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)

