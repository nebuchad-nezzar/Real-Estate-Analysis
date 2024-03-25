library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(shinyWidgets)
library(tibble)
library(ggthemes)
library(tidyverse)
library(ggthemes)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(corrplot)
library(DescTools)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(visdat)
library(leaflet)
library(hrbrthemes)
library(RColorBrewer)
library(ggridges)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(GGally)
library(kableExtra)
library(RColorBrewer)
library(plotly)
library(visdat)
library(glue)
library(rootSolve)
library(glue)
library(readr) 
library(dplyr)
library(corrplot)
library(RColorBrewer)

data <- read.csv("th_new.csv")
data <- na.omit(data)

# Create a dataframe
bank_rates <- tibble(
  Bank = c("BMO", "CIBC", "NBC", "RBC", "Scotia", "TD", "Lowest"),
  `1Yr Fixed` = c(8.09, 7.54, 7.84, 7.94, 8.04, 7.84, 7.54),
  `2Yr Fixed` = c(7.69, 7.29, 7.84, 7.54, 7.64, 7.34, 7.29),
  `3Yr Fixed` = c(7.20, 7.09, 6.99, 7.15, 7.24, 7.14, 6.99),
  `4Yr Fixed` = c(6.99, 6.84, 6.79, 6.94, 7.04, 6.99, 6.79),
  `5Yr Fixed` = c(7.04, 6.94, 6.84, 6.99, 7.04, 6.84, 6.84),
  `7Yr Fixed` = c(7.35, 7.00, 7.09, 7.15, 7.20, 7.10, 7.00),
  `10Yr Fixed` = c(7.64, 7.74, 7.44, 8.00, 7.69, 7.25, 7.25)
)

# Extract years from column names of bank_rates
loan_years <- as.numeric(gsub("[^0-9]", "", names(bank_rates)[2:ncol(bank_rates)]))

# Define UI with a background image
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
  titlePanel(div(align = "center", "PortfolioFrontier: Exploring the Landscape of Investment")),
  
  # Filters
  fluidRow(
    column(3,
           sliderInput("slider1", label = h3("Sales"), min = 0, 
                       max = max(data$sales), value = max(data$sales))
    ),
    column(3,
           sliderInput("slider2", label = h3("Listing"), min = min(data$listings), 
                       max = max(data$listings), value = c(0, max(data$listings)))
    ),
    
    column(6,
           pickerInput(
             inputId = "my_select_box_1",
             label = "Select multiple Cities:",
             choices = unique(c(data$city)),
             multiple = TRUE,
             options = list(
               `actions-box` = TRUE,
               `selected-text-format` = "count > 3",
               `live-search` = TRUE,
               `selectAllText` = "Select All",
               `deselectAllText` = "Deselect All"
             )
           )
    ),
    column(6,
           pickerInput(
             inputId = "my_select_box_2",
             label = "Select multiple year:",
             choices = unique(c(data$year)),
             multiple = TRUE,
             options = list(
               `actions-box` = TRUE,
               `selected-text-format` = "count > 3",
               `live-search` = TRUE,
               `selectAllText` = "Select All",
               `deselectAllText` = "Deselect All"
             )
           )
    ),
    
    column(3,
           sliderInput("slider3", label = h3("Volume"), min = min(data$volume), 
                       max = max(data$volume), value = c(0, max(data$volume)))
    ),
    
    column(3,
           sliderInput("slider4", label = h3("Median"), min = min(data$median), 
                       max = max(data$median), value = c(0, max(data$median)))
    ),
    column(6,
           pickerInput(
             inputId = "my_select_box_3",
             label = "Select multiple month:",
             choices = unique(c(data$month)),
             multiple = TRUE,
             options = list(
               `actions-box` = TRUE,
               `selected-text-format` = "count > 3",
               `live-search` = TRUE,
               `selectAllText` = "Select All",
               `deselectAllText` = "Deselect All"
             )
           )
    )
  ),
  hr(),
  
  # Create tabs with an ID for the content area
  div(id = "contentArea", 
      tabsetPanel(
        tabPanel("Dashboard", fluidRow(column(6, plotOutput("barPlot1_1")), column(6, plotOutput("barPlot2_1")), column(6, plotOutput("barPlot4_1")),column(6, plotOutput("linePlot_5_1")),column(12, plotOutput("heatMap_1")))),
        tabPanel("City Listings", plotOutput("barPlot1"),plotOutput("heatMap")),
        tabPanel("Median Prices", plotOutput("barPlot2"), plotOutput("barPlot4")),
        tabPanel("Yearly sales", plotOutput("barPlot3"),plotOutput("linePlot_5")),
        tabPanel("Map", leafletOutput("map")),
        tabPanel("Dataset", tableOutput("csvTable")),
        tabPanel("Bank Rates", 
                 fluidPage(
                   titlePanel("Mortgage Rates Calculator"),
                   column(12, align = "center", h3("Bank Mortgage Rates")),
                   column(6, selectInput("select_bank", "Select Bank:", choices = bank_rates$Bank)),
                   column(6, selectInput("loan_term", "Loan Term (Years):", choices = loan_years)),
                   column(6, numericInput("loan_amount", "Loan Amount:", value = 100000)),
                   column(6, actionButton("calc_button", "Calculate")),
                   column(12, textOutput("loan_output")),
                   tableOutput("rates_table")
                 )
        )
      )
  ),
  fluidRow(
    column(12, align = "center", h5("Project made by - Areeb Salsabil, Vatsal, Sujata & Muskan"))
  )
)



# Define server logic
server <- function(input, output) {
  
  # Filtered data based on slider input
  filtered_data <- reactive({
    filtered <- data
    # Apply filter based on slider1
    filtered <- filtered[filtered$sales <= input$slider1, ]
    # Apply filter based on slider2
    filtered <- filtered[filtered$listings >= input$slider2[1] & filtered$listings <= input$slider2[2], ]
    # Apply filter based on pickerInput for cities
    if (!is.null(input$my_select_box_1)) {
      filtered <- filtered[data$city %in% input$my_select_box_1, ]
    }
    # Apply filter based on pickerInput for years
    if (!is.null(input$my_select_box_2)) {
      filtered <- filtered[data$year %in% input$my_select_box_2, ]
    }
    # Apply filter based on slider3 (Volume)
    filtered <- filtered[filtered$volume >= input$slider3[1] & filtered$volume <= input$slider3[2], ]
    # Apply filter based on slider4 (Median)
    filtered <- filtered[filtered$median >= input$slider4[1] & filtered$median <= input$slider4[2], ]
    # Apply filter based on pickerInput for months
    if (!is.null(input$my_select_box_3)) {
      filtered <- filtered[data$month %in% input$my_select_box_3, ]
    }
    
    return(filtered)
  })
  
  # Render the filtered data
  output$filteredTable <- renderTable({
    filtered_data()
  })
  
  # Render the dataset table
  output$csvTable <- renderTable({
    filtered_data()
  })
  # Define a reactive value for the loan output
  loan_output_value <- reactiveVal()
  
  # Render the bank rates table
  output$rates_table <- renderTable({
    bank_rates
  })
  
  # Plot the data for number of listings by city
  output$barPlot1 <- renderPlot({
    data_by_city_1 <- filtered_data() %>%
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
           title = "Top Cities by Listing",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  # Plot the data for number of listings by city (for Dashboard tab)
  output$barPlot1_1 <- renderPlot({
    data_by_city_1 <- filtered_data() %>%
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
           title = "Top Cities by Listing",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  
  # Plot the data for median prices over time
  output$barPlot4 <- renderPlot({
    data_by_city_4 <- filtered_data() %>%
      group_by(year) %>%
      summarise(median_price = median(median, na.rm = TRUE))
    
    # Create a line chart for 'median_price'
    ggplot(data_by_city_4, aes(x=year, y=median_price)) +
      geom_line(color = "deepskyblue4", size = 1) +
      geom_point(color = "darkred", size = 3) +  
      geom_smooth(method="lm", se=FALSE, color="black", linetype = "dashed") + 
      geom_text(aes(label=median_price), vjust=0.5, hjust = -0.5, size =4) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", 
           y = "Median",
           title = "Trend of Median Price over time",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  
  # Plot the data for median prices over time
  output$barPlot4_1 <- renderPlot({
    data_by_city_4 <- filtered_data() %>%
      group_by(year) %>%
      summarise(median_price = median(median, na.rm = TRUE))
    
    # Create a line chart for 'median_price'
    ggplot(data_by_city_4, aes(x=year, y=median_price)) +
      geom_line(color = "deepskyblue4", size = 1) +
      geom_point(color = "darkred", size = 3) +  
      geom_smooth(method="lm", se=FALSE, color="black", linetype = "dashed") + 
      geom_text(aes(label=median_price), vjust=0.5, hjust = -0.5, size =4) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", 
           y = "Median",
           title = "Trend of Median Price over time",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  
  # Plot the data for average median prices by city
  output$barPlot2 <- renderPlot({
    data_by_city_2 <- filtered_data() %>%
      group_by(city) %>%
      summarise(median_price = mean(median, na.rm = TRUE))
    
    # Create a bar chart for 'median_price'
    ggplot(data_by_city_2, aes(x=reorder(city, -median_price), y=median_price)) +
      geom_bar(stat="identity", fill="deepskyblue4") +
      labs(x = "City", 
           y = "Average Median Price",
           title = "Bar Chart of Average Median Prices",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  
  
  # Plot the data for average median prices by city
  output$barPlot2_1 <- renderPlot({
    data_by_city_2 <- filtered_data() %>%
      group_by(city) %>%
      summarise(median_price = mean(median, na.rm = TRUE))
    
    # Create a bar chart for 'median_price'
    ggplot(data_by_city_2, aes(x=reorder(city, -median_price), y=median_price)) +
      geom_bar(stat="identity", fill="deepskyblue4") +
      labs(x = "City", 
           y = "Average Median Price",
           title = "Cumulative Median Price of Cities",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1,
                                       size = 9)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  # Line Plot the time series plot of sales
  output$barPlot3 <- renderPlot({
    data_by_year <- filtered_data() %>%
      group_by(year) %>%
      summarise(sales = sum(sales, na.rm = TRUE)) %>%
      arrange(desc(sales)) # Arrange by listings in descending order
    
    # Create a time series plot for 'sales'
    ggplot(data_by_year, aes(x=year, y=sales)) +
      geom_line(color = "deepskyblue4", size = 1) +
      geom_point(color = "darkred", size = 3) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      geom_text(aes(label=sales), vjust=0.5, hjust = -0.5, size =4) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", 
           y = "Sales",
           title = "Time Series Plot of Sales",
           subtitle = "Data for 15 years",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  # Line Plot the time series plot of sales
  output$barPlot3_1 <- renderPlot({
    data_by_year <- filtered_data() %>%
      group_by(year) %>%
      summarise(sales = sum(sales, na.rm = TRUE)) %>%
      arrange(desc(sales)) # Arrange by listings in descending order
    
    # Create a time series plot for 'sales'
    ggplot(data_by_year, aes(x=year, y=sales)) +
      geom_line(color = "deepskyblue4", size = 1) +
      geom_point(color = "darkred", size = 3) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      geom_text(aes(label=sales), vjust=0.6, hjust = -0.5, size =4) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Year", 
           y = "Sales",
           title = "Time Series Plot of Sales",
           subtitle = "Annual Sales Over Time",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) 
  })
  
  # Plot the data for sales and listings over time
  output$linePlot_5 <- renderPlot({
    data_long <- filtered_data() %>%
      group_by(year) %>%
      summarise(listings = sum(listings, na.rm = TRUE),
                sales = sum(sales, na.rm = TRUE)) %>%
      pivot_longer(cols = -year, names_to = "Type", values_to = "Value")
    
    # Plot the data
    ggplot(data_long, aes(x = year, y = Value, group = Type)) +
      geom_line(aes(color = Type), size = 1) +  # Set color based on 'Type'
      geom_point(color = "darkred", size = 3) +
      scale_color_manual(values = c("deepskyblue4", "darkgreen")) +  # Define custom colors
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = Value), vjust = 0.8, hjust = -0.4, size = 4) +
      labs(x = "Year", 
           y = "Value",
           title = "Sales and Listings over Time",
           subtitle = "Annual Sales and Listings Over Time",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12))
  })
  
  # Plot the data for sales and listings over time
  output$linePlot_5_1 <- renderPlot({
    data_long <- filtered_data() %>%
      group_by(year) %>%
      summarise(listings = sum(listings, na.rm = TRUE),
                sales = sum(sales, na.rm = TRUE)) %>%
      pivot_longer(cols = -year, names_to = "Type", values_to = "Value")
    
    # Plot the data
    ggplot(data_long, aes(x = year, y = Value, group = Type)) +
      geom_line(aes(color = Type), size = 1) +  # Set color based on 'Type'
      geom_point(color = "darkred", size = 3) +
      scale_color_manual(values = c("deepskyblue4", "darkgreen")) +  # Define custom colors
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(labels = scales::comma) +
      geom_text(aes(label = Value), vjust = 0.8, hjust = -0.4, size = 4) +
      labs(x = "Year", 
           y = "Value",
           title = "Sales and Listings over Time",
           subtitle = "Annual Sales and Listings Over Time",
           caption = "Data: ALY-6070 dataset") +
      theme_few() +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12))
  })
  
  
  # Plot the heatmap of median price and sales
  output$heatMap <- renderPlot({
    # Group data by year and city and calculate median price and total sales
    data_heatmap <- filtered_data() %>%
      group_by(year, city) %>%
      summarise(median_price = median(median, na.rm = TRUE),
                total_sales = sum(sales, na.rm = TRUE)) %>%
      arrange(year, city) %>%
      head(10)
    
    # Convert city to factor to ensure all cities are included in the heatmap
    data_heatmap$city <- factor(data_heatmap$city)
    
    # Pivot data to wide format for creating the heatmap
    heatmap_data <- pivot_wider(data_heatmap, names_from = city, values_from = median_price)
    
    # Create the heatmap plot
    ggplot(data_heatmap, aes(x = year, y = city, fill = total_sales)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient fill for total sales
      labs(x = "Year", 
           y = "City",
           fill = "Total Sales",
           title = "Heatmap of Median Price and Total Sales",
           subtitle = "Median Price vs. Total Sales Across Cities and Years",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 1),
            legend.position = "right")
  })
  
  # Plot the heatmap of median price and sales
  output$heatMap_1 <- renderPlot({
    # Group data by year and city and calculate median price and total sales
    data_heatmap <- filtered_data() %>%
      group_by(year, city) %>%
      summarise(median_price = median(median, na.rm = TRUE),
                total_sales = sum(sales, na.rm = TRUE)) %>%
      arrange(year, city)%>%
      head(10)
    
    # Convert city to factor to ensure all cities are included in the heatmap
    data_heatmap$city <- factor(data_heatmap$city)
    
    # Pivot data to wide format for creating the heatmap
    heatmap_data <- pivot_wider(data_heatmap, names_from = city, values_from = median_price)
    
    # Create the heatmap plot
    ggplot(data_heatmap, aes(x = year, y = city, fill = total_sales)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient fill for total sales
      labs(x = "Year", 
           y = "City",
           fill = "Total Sales",
           title = "Heatmap of Median Price and Total Sales",
           subtitle = "Median Price vs. Total Sales Across Cities and Years",
           caption = "Data: ALY-6070 dataset") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.position = "right"
      )
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
  
  # Calculate and update the loan value when the button is clicked
  observeEvent(input$calc_button, {
    # Ensure the loan term is numeric
    loan_term <- as.numeric(input$loan_term)
    
    # Construct the column name and ensure it's a symbol
    rate_column <- sym(paste0(loan_term, "Yr Fixed"))
    
    # Pull the rate for the selected bank and ensure it's numeric
    bank_rate <- as.numeric(bank_rates %>% filter(Bank == input$select_bank) %>% pull(!!rate_column))
    
    # Ensure the loan amount is numeric
    loan_amount <- as.numeric(input$loan_amount)
    
    # Calculate the total loan value using compound interest
    total_loan_value <- loan_amount * (1 + bank_rate / 100)^(loan_term)
    
    # Display the result
    output$loan_output <- renderText({
      paste("Total Loan Value: $", formatC(total_loan_value, format = "f", big.mark = ","), sep = "")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
