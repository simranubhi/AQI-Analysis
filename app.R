library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)

# Load cleaned data
aqi_data <- read.csv("data/processed/aqi_clean.csv")
aqi_data$Date <- as.Date(aqi_data$Date)



ui <- dashboardPage(
  dashboardHeader(title = "AQI Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Pollutant Analysis", tabName = "pollutant", icon = icon("flask")),
      menuItem("City-to-City Comparison",
               tabName = "city_compare",
               icon = icon("exchange-alt")),
      
      menuItem("AQI Trends & Health Impact", tabName = "health", icon = icon("heartbeat")),
      menuItem("Map Visualization", tabName = "map", icon = icon("map")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_cities"),
                valueBoxOutput("avg_aqi"),
                valueBoxOutput("highest_city"),
                valueBoxOutput("lowest_city")
              ),
              fluidRow(
                box(
                  title = "Cities Covered",
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  # Display cities as a comma-separated list
                  paste(unique(aqi_data$City), collapse = ", "),
                  style = "font-size: 14px; line-height: 1.5; color: #2c3e50;"
                )
              ),
              fluidRow(
                plotlyOutput("aqi_trend")
              )
      ),
      tabItem(tabName = "pollutant",
              fluidRow(
                column(4,
                       selectInput("pollutant_select",
                                   "Select Pollutant:",
                                   choices = c("PM2.5","PM10","NO2","SO2","CO","O3"),
                                   selected = "PM2.5")
                ),
                column(4,
                       selectInput("city_select",
                                   "Select City:",
                                   choices = unique(aqi_data$City),
                                   selected = "Delhi")
                ),
                column(4,
                       dateRangeInput("date_range",
                                      "Select Date Range:",
                                      start = min(aqi_data$Date),
                                      end = max(aqi_data$Date))
                )
              ),
              fluidRow(
                plotlyOutput("pollutant_city"),
                plotlyOutput("pollutant_city_month"),
                plotlyOutput("avg_aqi_by_city")
              )
      ),
      #---------------------new---
      tabItem(tabName = "city_compare",
              
              fluidRow(
                column(4,
                       selectInput(
                         "cc_cities",
                         "Select Cities (Max 3):",
                         choices = unique(aqi_data$City),
                         selected = c("Delhi", "Mumbai"),
                         multiple = TRUE
                       )
                ),
                column(4,
                       dateRangeInput(
                         "cc_date",
                         "Select Date Range:",
                         start = min(aqi_data$Date),
                         end = max(aqi_data$Date)
                       )
                )
              ),
              
              br(),
              
              fluidRow(
                column(12, plotlyOutput("cc_trend", height = "400px"))
              ),
              
              br(),
              
              fluidRow(
                column(12,
                       box(
                         title = "Comparative Insight",
                         width = 12,
                         status = "info",
                         solidHeader = TRUE,
                         uiOutput("cc_description")
                       )
                )
              )
      ),
      #----------------------------------
      tabItem(tabName = "health",
              fluidRow(
                column(6, div(plotlyOutput("avg_monthly_aqi", height = "400px"), style = "margin-bottom: 30px;")),
                column(6, plotlyOutput("unhealthy_days", height = "400px"))
              ),
              br(),
              fluidRow(
                column(12, plotlyOutput("aqi_category_percent",height = "400px")),
                br(),
                column(12, plotlyOutput("rolling_aqi", height="400px"))
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                column(4,
                       selectInput("map_pollutant",
                                   "Select Pollutant:",
                                   choices = c("PM2.5","PM10","NO2","SO2","CO","O3"),
                                   selected = "PM2.5")
                ),
                column(4,
                       dateRangeInput("map_date_range",
                                      "Select Date Range:",
                                      start = min(aqi_data$Date),
                                      end = max(aqi_data$Date))
                ),
                column(4,
                       selectInput("map_city",
                                   "Select City:",
                                   choices = c("All", unique(aqi_data$City)),
                                   selected = "All")
                )
                
              ),
              fluidRow(
                column(12, leafletOutput("aqi_map", height = "600px"))
              )
      ),
      
      tabItem(tabName = "table",
              fluidRow(
                column(4,
                       selectInput("table_city",
                                   "Select City:",
                                   choices = c("All", unique(aqi_data$City)),
                                   selected = "All")
                ),
                column(4,
                       selectInput("table_pollutant",
                                   "Select Pollutant:",
                                   choices = c("All", "PM2.5","PM10","NO2","SO2","CO","O3"),
                                   selected = "All")
                ),
                column(4,
                       downloadButton("download_data", "Download CSV")
                )
              ),
              fluidRow(
                column(12, DT::dataTableOutput("aqi_table"))
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       h2("AQI Dashboard"),
                       p("Created by: Simran Kaur"),
                       p("College: Guru Nanak Dev Engineering College, Ludhiana, Punjab"),
                       p("Guide: Dr. Amit Jain"),
                       p("Batch:2023-2027 "),
                       br(),
                       h4("Project Summary:"),
                       p("This dashboard provides a detailed analysis of air quality across major Indian cities from 2015-2020. Users can explore pollutant levels, overall AQI trends, unhealthy days, and more."),
                       br(),
                       h4("Data Sources:"),
                       p("CPCB (Central Pollution Control Board) and OpenAQ datasets were used for this analysis."),
                       br(),
                       h4("R Packages Used:"),
                       p("shiny, shinydashboard, tidyverse, lubridate, plotly, leaflet, DT"),
                       br(),
                       h4("Credits:"),
                       p("Special thanks to the contributors of the AQI datasets and the open-source R community.")
                )
              )
      )
      
      
    )
  )
)

server <- function(input, output) {
  
  # Total cities
  output$total_cities <- renderValueBox({
    valueBox(
      length(unique(aqi_data$City)),
      "Total Cities",
      icon = icon("city"),
      color = "purple"
    )
  })
  
  # Average AQI overall
  output$avg_aqi <- renderValueBox({
    valueBox(
      round(mean(aqi_data$AQI, na.rm=TRUE), 1),
      "Average AQI",
      icon = icon("smog"),
      color = "green"
    )
  })
  
  # City with highest AQI
  output$highest_city <- renderValueBox({
    city_max <- aqi_data %>%
      group_by(City) %>%
      summarise(avg = mean(AQI, na.rm=TRUE)) %>%
      arrange(desc(avg)) %>%
      slice(1)
    valueBox(
      paste(city_max$City, round(city_max$avg, 1)),
      "Highest AQI City",
      icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  # City with lowest AQI
  output$lowest_city <- renderValueBox({
    city_min <- aqi_data %>%
      group_by(City) %>%
      summarise(avg = mean(AQI, na.rm=TRUE)) %>%
      arrange(avg) %>%
      slice(1)
    valueBox(
      paste(city_min$City, round(city_min$avg, 1)),
      "Lowest AQI City",
      icon = icon("arrow-down"),
      color = "blue"
    )
  })
  
  # Rolling AQI Trend (Last 90 Days)
  output$aqi_trend <- renderPlotly({
    trend <- aqi_data %>%
      group_by(Date) %>%
      summarise(avg_aqi = mean(AQI, na.rm=TRUE))
    
    plot_ly(trend, x = ~Date, y = ~avg_aqi, type = 'scatter', mode = 'lines',
            line = list(color = 'orange')) %>%
      layout(title = "Overall AQI Trend", yaxis = list(title="AQI"), xaxis=list(title="Date"))
  })
  
  # Reactive filtered data
  pollutant_filtered <- reactive({
    req(input$pollutant_select, input$city_select, input$date_range)  # ensures inputs exist
    
    aqi_data %>%
      filter(City == input$city_select,
             Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      select(Date, City, all_of(input$pollutant_select))  # select only the chosen pollutant
  })
  
  
  output$pollutant_city <- renderPlotly({
    df <- pollutant_filtered()  # get filtered data
    
    plot_ly(df, x = ~Date, y = as.formula(paste0("~`", input$pollutant_select, "`")),
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue')) %>%
      layout(title = paste(input$pollutant_select, "Trend in", input$city_select),
             yaxis = list(title = input$pollutant_select),
             xaxis = list(title = "Date"))
  })
  
  pollutant_monthly <- reactive({
    req(input$pollutant_select, input$city_select, input$date_range)
    
    aqi_data %>%
      filter(City == input$city_select,
             Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%  # convert Date to Year-Month
      group_by(YearMonth) %>%
      summarise(Pollutant_Avg = mean(get(input$pollutant_select), na.rm = TRUE))
  })
  output$pollutant_city_month <- renderPlotly({
    df <- pollutant_monthly()
    
    plot_ly(df, x = ~YearMonth, y = ~Pollutant_Avg,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'darkgreen')) %>%
      layout(title = paste(input$pollutant_select, "Monthly Average in", input$city_select),
             yaxis = list(title = input$pollutant_select),
             xaxis = list(title = "Month"))
  })
  
  # --------------- City-to-City Comparison Data ----
  cc_data <- reactive({
    req(input$cc_cities, input$cc_date)
    
    if (length(input$cc_cities) > 3) {
      showNotification(
        "Please select a maximum of 3 cities only.",
        type = "warning"
      )
      return(NULL)
    }
    
    aqi_data %>%
      filter(
        City %in% input$cc_cities,
        Date >= input$cc_date[1],
        Date <= input$cc_date[2]
      ) %>%
      group_by(Date, City) %>%
      summarise(AQI = mean(AQI, na.rm = TRUE), .groups = "drop")
  })
  output$cc_trend <- renderPlotly({
    df <- cc_data()
    req(df)
    
    plot_ly(
      df,
      x = ~Date,
      y = ~AQI,
      color = ~City,
      type = "scatter",
      mode = "lines"
    ) %>%
      layout(
        title = "AQI Trend Comparison Across Selected Cities",
        yaxis = list(title = "AQI"),
        xaxis = list(title = "Date")
      )
  })
  output$cc_description <- renderUI({
    df <- cc_data()
    req(df)
    
    summary_df <- df %>%
      group_by(City) %>%
      summarise(Avg_AQI = mean(AQI, na.rm = TRUE)) %>%
      arrange(desc(Avg_AQI))
    
    highest <- summary_df$City[1]
    lowest <- summary_df$City[nrow(summary_df)]
    
    if (nrow(summary_df) == 2) {
      tags$ul(
        tags$li(paste(highest, "shows consistently higher AQI compared to", lowest)),
        tags$li("The AQI trends of both cities follow a similar seasonal pattern")
      )
    } else {
      tags$ul(
        tags$li(paste(highest, "records the highest average AQI among the selected cities")),
        tags$li(paste(lowest, "shows comparatively better air quality")),
        tags$li("Overall AQI trends indicate clear inter-city pollution differences")
      )
    }
  })
  #--------------------------------------------------------
  
  # Reactive dataset for average AQI by city
  aqi_by_city_filtered <- reactive({
    req(input$date_range)
    
    aqi_data %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      group_by(City) %>%
      summarise(Avg_AQI = mean(AQI, na.rm=TRUE)) %>%
      arrange(desc(Avg_AQI))
  })
  
  output$avg_aqi_by_city <- renderPlotly({
    df <- aqi_by_city_filtered()
    
    plot_ly(df, x = ~City, y = ~Avg_AQI,
            type = 'bar', marker = list(color = 'orange')) %>%
      layout(title = "Average AQI by City",
             yaxis = list(title = "Average AQI"),
             xaxis = list(title = "City", tickangle = -45))
  })
  # Reactive dataset: Average monthly AQI
  monthly_aqi <- reactive({
    aqi_data %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(Avg_AQI = mean(AQI, na.rm=TRUE))
  })
  output$avg_monthly_aqi <- renderPlotly({
    df <- monthly_aqi()
    
    plot_ly(df, x = ~YearMonth, y = ~Avg_AQI,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'purple')) %>%
      layout(title = "Average Monthly AQI (2015–2020)",
             yaxis = list(title = "AQI"),
             xaxis = list(title = "Month"))
  })
  
  unhealthy_days_data <- reactive({
    aqi_data %>%
      mutate(YearMonth = floor_date(Date, "month")) %>%
      group_by(YearMonth) %>%
      summarise(Unhealthy_Days = sum(AQI > 200, na.rm = TRUE))
  })
  output$unhealthy_days <- renderPlotly({
    df <- unhealthy_days_data()
    
    plot_ly(df, x = ~YearMonth, y = ~Unhealthy_Days,
            type = 'bar', marker = list(color = 'red')) %>%
      layout(title = "Unhealthy AQI Days per Month",
             yaxis = list(title = "Number of Unhealthy Days"),
             xaxis = list(title = "Month"))
  })
  
  aqi_category_percent_data <- reactive({
    req(input$date_range)
    
    aqi_data %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2]) %>%
      group_by(AQI_Category) %>%
      summarise(Days = n()) %>%
      mutate(Percent = round(Days / sum(Days) * 100, 1))
  })
  output$aqi_category_percent <- renderPlotly({
    df <- aqi_category_percent_data()
    
    plot_ly(df, labels = ~AQI_Category, values = ~Percent, type = 'pie',
            textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = "% of Days by AQI Category")
  })
  
  rolling_aqi_data <- reactive({
    aqi_data %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>%
      arrange(Date) %>%
      mutate(
        Roll_7 = zoo::rollmean(AQI, k = 7, fill = NA, align = "right"),
        Roll_30 = zoo::rollmean(AQI, k = 30, fill = NA, align = "right")
      )
  })
  output$rolling_aqi <- renderPlotly({
    df <- rolling_aqi_data()
    
    plot_ly(df, x = ~Date) %>%
      add_lines(y = ~Roll_7, name = "7-day Rolling AQI", line = list(color = "blue")) %>%
      add_lines(y = ~Roll_30, name = "30-day Rolling AQI", line = list(color = "red")) %>%
      layout(title = "Rolling 7-day & 30-day AQI",
             yaxis = list(title = "AQI"),
             xaxis = list(title = "Date"))
  })
  
  city_coords <- data.frame(
    City = c("Ahmedabad","Delhi","Lucknow","Chandigarh","Jaipur","Hyderabad","Mumbai","Kolkata"),
    Lat = c(23.03, 28.61, 26.85, 30.74, 26.92, 17.38, 19.07, 22.57),
    Lon = c(72.57, 77.2, 80.95, 76.78, 75.85, 78.47, 72.87, 88.36)
  )
  
  map_data <- reactive({
    req(input$pollutant_select)
    
    aqi_clean %>%
      filter(City %in% city_coords$City) %>%  # only these 8 cities
      group_by(City) %>%
      summarise(
        Avg_AQI = mean(get(input$pollutant_select), na.rm = TRUE),
        Major_Pollutant = input$pollutant_select
      ) %>%
      left_join(city_coords, by = "City") %>%
      filter(!is.na(Lat) & !is.na(Lon))
  })
  
  # ---- 3. Leaflet Map Output ----
  output$aqi_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.9629, lat = 20.5937, zoom = 5)  # Center on India
  })
  
  # ---- 4. Update map dynamically ----
  observe({
    df <- map_data()
    
    leafletProxy("aqi_map", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~Lon,
        lat = ~Lat,
        radius = ~sqrt(Avg_AQI / max(Avg_AQI, na.rm = TRUE) * 30) + 5,  # scale size
        color = ~ifelse(Avg_AQI < 50, "green",
                        ifelse(Avg_AQI < 100, "yellow",
                               ifelse(Avg_AQI < 200, "orange",
                                      ifelse(Avg_AQI < 300, "red", "purple")))),
        fillOpacity = 0.7,
        label = ~paste0("<b>", City, "</b><br>",
                        "Avg AQI: ", round(Avg_AQI,1), "<br>",
                        "Major Pollutant: ", Major_Pollutant),
        labelOptions = labelOptions(style = list("font-weight" = "bold", padding = "3px 8px"),
                                    textsize = "13px", direction = "auto")
      ) %>%
      clearControls() %>% 
      addLegend(position = "bottomright",
                colors = c("green","yellow","orange","red","purple"),
                labels = c("Good","Moderate","Poor","Very Poor","Severe"),
                title = "AQI Category")
  })
  
  
  map_data <- reactive({
    req(input$map_date_range)
    
    df <- aqi_data %>%
      filter(Date >= input$map_date_range[1],
             Date <= input$map_date_range[2])
    
    # Filter by selected city if not "All"
    if(input$map_city != "All"){
      df <- df %>% filter(City == input$map_city)
    }
    
    df <- df %>%
      group_by(City) %>%
      summarise(
        Avg_AQI = mean(AQI, na.rm = TRUE),
        Major_Pollutant = names(which.max(colMeans(across(c("PM2.5","PM10","NO2","SO2","CO","O3")), na.rm=TRUE)))
      ) %>%
      left_join(city_coords, by = "City")
    
    return(df)
  })
  table_data <- reactive({
    df <- aqi_data
    
    # Filter by city
    if(input$table_city != "All"){
      df <- df %>% filter(City == input$table_city)
    }
    
    # Filter by pollutant (show only selected pollutant + basic info)
    if(input$table_pollutant != "All"){
      df <- df %>% select(City, Date, AQI, AQI_Category, input$table_pollutant)
    }
    
    return(df)
  })
  output$aqi_table <- DT::renderDataTable({
    DT::datatable(
      table_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  output$download_data <- downloadHandler(
    filename = function() {
      paste("AQI_data_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_data(), file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)