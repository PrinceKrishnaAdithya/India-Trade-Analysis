library(shiny)
library(ggplot2)

# Import data
import_data <- read.csv('C:/Users/kizsu/Downloads/2018-2010_import.csv')

# Export data
export_data <- read.csv('C:/Users/kizsu/Downloads/2018-2010_export(1).csv')

# Extract unique countries and commodities for imports
unique_countries_imports <- unique(import_data$country)
unique_commodities_imports <- unique(import_data$Commodity)

# Extract unique countries and commodities for exports
unique_countries_exports <- unique(export_data$country)
unique_commodities_exports <- unique(export_data$Commodity)

# Define UI
ui <- fluidPage(
  titlePanel("India Import-Export Data Analysis (2010-2018)"),
  
  # Create navigation bar with two main tabs: Imports and Exports
  navbarPage("Analysis",
             
             # Main Imports Tab
             tabPanel("Imports",
                      tabsetPanel(
                        # Sub-tab 1: Trend Analysis for Imports
                        tabPanel("Trend Analysis for Imports",
                                 sidebarLayout(
                                   sidebarPanel(
                                     # Create select input for country
                                     selectInput(inputId = "selected_country_import", 
                                                 label = "Choose a country (Import):", 
                                                 choices = unique_countries_imports),
                                     
                                     # Create select input for commodity
                                     selectInput(inputId = "selected_commodity_import", 
                                                 label = "Choose a commodity (Import):", 
                                                 choices = unique_commodities_imports)
                                   ),
                                   
                                   mainPanel(
                                     # Display the selected country and commodity for feedback
                                     textOutput("selection_info_import"),
                                     
                                     # Output plot
                                     plotOutput("commodity_trend_import")
                                   )
                                 )
                        ),
                        
                        # Sub-tab 2: Most & Least Imported Commodities
                        tabPanel("Most & Least Imported",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "country_most_least_import", 
                                                 label = "Choose a country (Import):", 
                                                 choices = unique_countries_imports)
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Most Imported", tableOutput("most_imported")),
                                       tabPanel("Least Imported", tableOutput("least_imported"))
                                     )
                                   )
                                 )
                        ),
                        
                        # Sub-tab 3: Future Prediction for Imports
                        tabPanel("Future Prediction for Imports",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "selected_country_pred_import", 
                                                 label = "Choose a country (Import):", 
                                                 choices = unique_countries_imports),
                                     selectInput(inputId = "selected_commodity_pred_import", 
                                                 label = "Choose a commodity (Import):", 
                                                 choices = unique_commodities_imports)
                                   ),
                                   mainPanel(
                                     plotOutput("future_prediction_plot_import")
                                   )
                                 )
                        )
                      )
             ),
             
             # Main Exports Tab
             tabPanel("Exports",
                      tabsetPanel(
                        # Sub-tab 1: Trend Analysis for Exports
                        tabPanel("Trend Analysis for Exports",
                                 sidebarLayout(
                                   sidebarPanel(
                                     # Create select input for country
                                     selectInput(inputId = "selected_country_export", 
                                                 label = "Choose a country (Export):", 
                                                 choices = unique_countries_exports),
                                     
                                     # Create select input for commodity
                                     selectInput(inputId = "selected_commodity_export", 
                                                 label = "Choose a commodity (Export):", 
                                                 choices = unique_commodities_exports)
                                   ),
                                   
                                   mainPanel(
                                     # Display the selected country and commodity for feedback
                                     textOutput("selection_info_export"),
                                     
                                     # Output plot
                                     plotOutput("commodity_trend_export")
                                   )
                                 )
                        ),
                        
                        # Sub-tab 2: Most & Least Exported Commodities
                        tabPanel("Most & Least Exported",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "country_most_least_export", 
                                                 label = "Choose a country (Export):", 
                                                 choices = unique_countries_exports)
                                   ),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Most Exported", tableOutput("most_exported")),
                                       tabPanel("Least Exported", tableOutput("least_exported"))
                                     )
                                   )
                                 )
                        ),
                        
                        # Sub-tab 3: Future Prediction for Exports
                        tabPanel("Future Prediction for Exports",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "selected_country_pred_export", 
                                                 label = "Choose a country (Export):", 
                                                 choices = unique_countries_exports),
                                     selectInput(inputId = "selected_commodity_pred_export", 
                                                 label = "Choose a commodity (Export):", 
                                                 choices = unique_commodities_exports)
                                   ),
                                   mainPanel(
                                     plotOutput("future_prediction_plot_export")
                                   )
                                 )
                        )
                      )
             )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Tab 1: Display the selected country and commodity for imports
  output$selection_info_import <- renderText({
    paste("Country:", input$selected_country_import, "| Commodity:", input$selected_commodity_import)
  })
  
  # Tab 1: Render plot showing the value trend of the selected commodity over the years (imports)
  output$commodity_trend_import <- renderPlot({
    # Filter the data based on the selected country and commodity
    filtered_data_import <- subset(import_data, country == input$selected_country_import & Commodity == input$selected_commodity_import)
    
    # Create the plot
    ggplot(filtered_data_import, aes(x = year, y = value)) +
      geom_line() + geom_point() +
      labs(title = paste("Value Trend for", input$selected_commodity_import, "in", input$selected_country_import),
           x = "Year", 
           y = "Value") +
      theme_minimal()
  })
  
  # Tab 2: Most and Least Imported Commodities
  output$most_imported <- renderTable({
    # Filter the data for the selected country
    country_data_import <- subset(import_data, country == input$country_most_least_import)
    
    # Group by commodity and sum the values
    aggregate_data_import <- aggregate(value ~ Commodity, country_data_import, sum)
    
    # Sort and show the top 10 most imported commodities
    most_imported <- head(aggregate_data_import[order(-aggregate_data_import$value), ], 10)
    most_imported
  })
  
  output$least_imported <- renderTable({
    # Filter the data for the selected country
    country_data_import <- subset(import_data, country == input$country_most_least_import)
    
    # Group by commodity and sum the values
    aggregate_data_import <- aggregate(value ~ Commodity, country_data_import, sum)
    
    # Sort and show the 10 least imported commodities
    least_imported <- head(aggregate_data_import[order(aggregate_data_import$value), ], 10)
    least_imported
  })
  
  # Tab 3: Future Prediction for Imports using Linear Regression
  output$future_prediction_plot_import <- renderPlot({
    # Filter the data based on the selected country and commodity
    filtered_data_import <- subset(import_data, country == input$selected_country_pred_import & Commodity == input$selected_commodity_pred_import)
    
    # Check if there's enough data for linear regression (at least 2 points)
    if (nrow(filtered_data_import) >= 2) {
      # Fit the linear model
      model_import <- lm(value ~ year, data = filtered_data_import)
      
      # Predict future values for the next 7 years (2018-2024)
      future_years_import <- data.frame(year = seq(2018, 2024, by = 1))  # Predict from 2018 to 2024
      future_predictions_import <- predict(model_import, newdata = future_years_import)
      
      # Combine the future predictions with the years
      future_data_import <- data.frame(year = future_years_import$year, value = future_predictions_import)
      
      # Plot the original data and the predictions
      ggplot() +
        geom_line(data = filtered_data_import, aes(x = year, y = value), color = "blue") + 
        geom_point(data = filtered_data_import, aes(x = year, y = value), color = "blue") +
        geom_line(data = future_data_import, aes(x = year, y = value), color = "red", linetype = "dashed") +
        geom_point(data = future_data_import, aes(x = year, y = value), color = "red") +
        labs(title = paste("Value Trend and Predictions for", input$selected_commodity_pred_import, "in", input$selected_country_pred_import),
             x = "Year", 
             y = "Value") +
        theme_minimal()
    } else {
      ggplot() + 
        labs(title = "Insufficient data for selected combination", 
             x = NULL, y = NULL) +
        theme_void()
    }
  })
  
  # Tab 4: Display the selected country and commodity for exports
  output$selection_info_export <- renderText({
    paste("Country:", input$selected_country_export, "| Commodity:", input$selected_commodity_export)
  })
  
  # Tab 4: Render plot showing the value trend of the selected commodity over the years (exports)
  output$commodity_trend_export <- renderPlot({
    # Filter the data based on the selected country and commodity
    filtered_data_export <- subset(export_data, country == input$selected_country_export & Commodity == input$selected_commodity_export)
    
    # Create the plot
    ggplot(filtered_data_export, aes(x = year, y = value)) +
      geom_line() + geom_point() +
      labs(title = paste("Value Trend for", input$selected_commodity_export, "in", input$selected_country_export),
           x = "Year", 
           y = "Value") +
      theme_minimal()
  })
  
  # Tab 5: Most and Least Exported Commodities
  output$most_exported <- renderTable({
    # Filter the data for the selected country
    country_data_export <- subset(export_data, country == input$country_most_least_export)
    
    # Group by commodity and sum the values
    aggregate_data_export <- aggregate(value ~ Commodity, country_data_export, sum)
    
    # Sort and show the top 10 most exported commodities
    most_exported <- head(aggregate_data_export[order(-aggregate_data_export$value), ], 10)
    most_exported
  })
  
  output$least_exported <- renderTable({
    # Filter the data for the selected country
    country_data_export <- subset(export_data, country == input$country_most_least_export)
    
    # Group by commodity and sum the values
    aggregate_data_export <- aggregate(value ~ Commodity, country_data_export, sum)
    
    # Sort and show the 10 least exported commodities
    least_exported <- head(aggregate_data_export[order(aggregate_data_export$value), ], 10)
    least_exported
  })
  
  # Tab 6: Future Prediction for Exports using Linear Regression
  output$future_prediction_plot_export <- renderPlot({
    # Filter the data based on the selected country and commodity
    filtered_data_export <- subset(export_data, country == input$selected_country_pred_export & Commodity == input$selected_commodity_pred_export)
    
    # Check if there's enough data for linear regression (at least 2 points)
    if (nrow(filtered_data_export) >= 2) {
      # Fit the linear model
      model_export <- lm(value ~ year, data = filtered_data_export)
      
      # Predict future values for the next 7 years (2018-2024)
      future_years_export <- data.frame(year = seq(2018, 2024, by = 1))  # Predict from 2018 to 2024
      future_predictions_export <- predict(model_export, newdata = future_years_export)
      
      # Combine the future predictions with the years
      future_data_export <- data.frame(year = future_years_export$year, value = future_predictions_export)
      
      # Plot the original data and the predictions
      ggplot() +
        geom_line(data = filtered_data_export, aes(x = year, y = value), color = "blue") + 
        geom_point(data = filtered_data_export, aes(x = year, y = value), color = "blue") +
        geom_line(data = future_data_export, aes(x = year, y = value), color = "red", linetype = "dashed") +
        geom_point(data = future_data_export, aes(x = year, y = value), color = "red") +
        labs(title = paste("Value Trend and Predictions for", input$selected_commodity_pred_export, "in", input$selected_country_pred_export),
             x = "Year", 
             y = "Value") +
        theme_minimal()
    } else {
      ggplot() + 
        labs(title = "Insufficient data for selected combination", 
             x = NULL, y = NULL) +
        theme_void()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)