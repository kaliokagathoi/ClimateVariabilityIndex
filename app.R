library(shiny)
library(dplyr)
library(ggplot2)

# Loading Data
data <- read.csv("complete.csv")
data$date <- lubridate::ym(data$date)
data$aaci <- as.numeric(data$aaci)
data$aaci.ma <- as.numeric(data$aaci.ma)

# Define UI
ui <- fluidPage(
  titlePanel("Climate Index for Customisable GRDC Regions"),
  sidebarLayout(
    sidebarPanel(
      tags$h4("Please Select A Region or Individual Location:"),
      selectInput("region_or_location", "Region or Individual Location:", choices = c("Region", "Individual Location")),
      conditionalPanel(
        condition = "input.region_or_location == 'Region'",
        selectInput("region", "Choose a Region:", choices = c("All Regions", "North", "South", "West"))
      ),
      conditionalPanel(
        condition = "input.region_or_location == 'Individual Location'",
        selectInput("location", "Choose a Location:", choices = c("Albany", "Bairnsdale", "Ballarat", "Bendigo", "Bordertown", "Bothwell", "Bourke",
                                                                  "Bundaberg", "Coorow", "Dubbo", "Esperance", "Geraldton", "Goondiwindi", "Griffith",
                                                                  "Katanning", "Launceston", "Mackay", "Manjimup", "Margaret River", "Merredin", "Mildura",
                                                                  "Mount Gambier", "Narrogin", "Northam", "Ouyen", "Port Augusta", "Ravensthorpe", "Rolleston",
                                                                  "Roma", "Southern Cross", "Streaky Bay", "Tamworth", "Wagga Wagga", "Waroona", "Wongan Hills"))
      ),
      helpText(HTML("<h4 style='color:black;'><b>Interpretation:</b></h4>
                    <p style='color:black;'>An increase of 1 unit in the index can be interpreted as an additional observation above/below (where relevant) the 90th/10th percentile for all four sources of data. This could also be caused by four observations over expected frequency for a single metric etc.</p>
                    <h4 style='color:black;'><b>Methodology:</b></h4>
                    <p style='color:black;'>This index measures the frequency of climate data observed values that are classified as extreme. The index considers four sources of data:</p>
                    <h5 style='color:black;'><b>Maximum Temperature:</b></h5>
                    <p style='color:black;'>Defined as the frequency of observations greater than the 90th percentile of maximum temperatures in the reference period for the relevant period</p>
                    <h5 style='color:black;'><b>Minimum Temperature:</b></h5>
                    <p style='color:black;'>Defined as the frequency of observations less than the 10th percentile of minimum temperatures in the reference period for the relevant period</p>
                    <h5 style='color:black;'><b>Rainfall:</b></h5>
                    <p style='color:black;'>Defined as the frequency of observations greater than the 90th percentile of cumulative 5-day rainfall in the reference period for the relevant period</p>
                    <h5 style='color:black;'><b>Wind:</b></h5>
                    <p style='color:black;'>Defined as the frequency of observations greater than the 90th percentile of wind power (adjusted from wind speed data) in the reference period for the relevant period</p>
                    <h4 style='color:black;'><b>References:</b></h4>
                    <p style='color:black;'>This data was collected from Open-Meteo.com which provides free and accessible historical climate data. This is available at https://open-meteo.com/en/docs/historical-weather-api.</p>
                    <p style='color:black;'>Methodology inspired by existing work done by the Australian Actuaries Climate Index and the Actuaries Climate Index (US + Canada) and rebuilt from scratch for analysis of the presented regions with adjusted methodology to adjust for presence of skewed data.</p>
                    <p style='color:black;'>Grain Research & Development Corporation (GRDC) regions are accessible at: https://grdc.com.au/about/our-industry/growing-regions and the list of locations selected from each region is below</p>
                    <h5 style='color:black;'><b>Northern Region:</b></h5>
                    <p style='color:black;'>Bourke, Bundaberg, Dubbo, Goondiwindi, Griffith, Mackay, Mildura, Rolleston, Roma, Tamworth, Wagga Wagga</p>
                    <h5 style='color:black;'><b>Southern Region:</b></h5>
                    <p style='color:black;'>Bairnsdale, Ballarat, Bendigo, Bordertown, Bothwell, Launceston, Mount Gambier, Ouyen, Port Augusta, Streaky Bay</p>
                    <h5 style='color:black;'><b>Western Region:</b></h5>
                    <p style='color:black;'>Albany, Coorow, Esperance, Geraldton, Katanning, Manjimup, Margaret River, Merredin, Narrogin, Northam, Ravensthorpe, Southern Cross, Waroona, Wongan Hills</p>
                    <h4 style='color:black;'><b>About & Contact:</b></h4>
                    <p style='color:black;'>Version 1.2 - Updated for Individual Locations</p>
                    <p style='color:black;'>Future Version 2.0 will allow viewing individual metrics (temperature, rain, wind)</p>
                    <p style='color:black;'><b>LinkedIn: </b>https://www.linkedin.com/in/tom-coates-tgc/</p>
                    <p style='color:black;'><b>X: </b>https://twitter.com/deltainvesting</p>
                    <h5 style='color:black;'><b>Created By Tom Coates | Please Credit for Use</b></h5>"))
    ),
    mainPanel(
      plotOutput("regionPlot", height = "800px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$regionPlot <- renderPlot({
    # Filter data based on selection
    if (input$region_or_location == "Region" & !is.null(input$region)) {
      df <- subset(data, region == input$region)
    } else if (input$region_or_location == "Individual Location" & !is.null(input$location)) {
      df <- subset(data, region == input$location)
    } else {
      df <- subset(data, region == "All Regions")  # or however you want to handle this case
    }
    
    # Create plot
    ggplot(df, aes(x = date)) + 
      geom_col(aes(y = aaci, fill = ifelse(aaci < 0, "below", "above"))) +
      scale_fill_manual(values = c("below" = "blue", "above" = "red")) +
      geom_line(aes(y = aaci.ma), color = "black", size = 2) + 
      scale_x_date(date_labels = "%Y", date_breaks = "5 years", breaks = "5 years") +
      xlab("  ") + 
      ylab("Observed Anomaly") + 
      ggtitle("Climate Variability in Selected Agricultural Region") +
      labs(subtitle = "Monthly Data and 5 Year Average Presented from 1950-2023 with 1950-1980 Reference Period | Source: Open-Meteo.com") +  
      theme(legend.position = "none",
            plot.background = element_rect(fill = "#F6F6F6"),
            panel.background = element_rect(fill = "#F6F6F6"),
            panel.grid.major.x = element_line(color = "lightgrey"),
            panel.grid.minor.x = element_blank(),  
            panel.grid.major.y = element_line(color = "lightgrey"),  
            panel.grid.minor.y = element_line(color = "lightgrey"))
  })
}

shinyApp(ui = ui, server = server)
