library(shiny)
library(dplyr)
library(ggplot2)

# Set working directory
##setwd("/Users/phu/Documents/info201_assignment/PS6-Assignment/PS6")
df <- read.delim("UAH-lower-troposphere-long.csv.bz2")

region_name <- df %>% 
  distinct(region)

# Define UI function
ui <- function() {
  fluidPage(
    titlePanel("Temperature by Region"),
    # Define tabs
    tabsetPanel(
      tabPanel("About",
               fluidRow(
                 column(width = 12,
                        h4("Sample of the UHA - Dataset"),
                        p(" Here is just a sample of 7/14310 of the data."),
                        verbatimTextOutput("sample")
                 )
               )
      ),
      tabPanel("Plot",
               sidebarLayout(
                 sidebarPanel(
                   p("This plot shows the temperature data for the selected region over time."),
                   selectInput("region", "Select a region:",
                               choices = region_name),
                   checkboxGroupInput("years", "Select years:",
                                      choices = unique(df$year),
                                      selected = c("1978", "1979", "1980", "1981", "1982"))
                 ),
                 mainPanel(
                   plotOutput("plot")
                 )
               )
      ),
      tabPanel("Select date",
               sidebarLayout(
                 sidebarPanel(
                   p("The table set on the right tells you the average temperature for each period. "),
                   radioButtons("period", "Average over:",
                                choices = list("Month",
                                               "Year",
                                               "Decade"),
                                selected = "Month")
                 ),
                 mainPanel(
                   tableOutput("summary")
                 )
               )
      )
    )
  )
}

server <- function(input, output) {
  # Subset data based on selected region and years
  data_subset <- reactive({
    filter(df, region == input$region, year %in% input$years)
  })
  
  # Display sample data
  sample_data <- head(df, 7)
  output$sample <- renderPrint({
    print(sample_data, row.names = FALSE)
  })
  
  # Create scatter plot with the reactive feature
  output$plot <- renderPlot({
    ggplot(data_subset(), aes(x = month, y = temp, color = year)) +
      geom_point() +
      labs(title = paste0("Temperature in ", input$region))
  })
  
  
  # Calculate average temperature by period
  output$summary <- renderTable ({
    if (input$period == "Month") {
      data_subset() %>%
        group_by(month) %>%
        summarize(avg_temp = mean(temp))
    } else if (input$period == "Year") {
      data_subset() %>%
        group_by(year) %>%
        summarize(avg_temp = mean(temp)) %>%
        rename(period = year)
    } else {
      data_subset() %>%
        mutate(decade = paste0(substr(year, 1, 3), "0s")) %>%
        group_by(decade) %>%
        summarize(avg_temp = mean(temp)) %>%
        rename(period = decade)
    }
  })
}


# Run the app
shinyApp(ui, server)
