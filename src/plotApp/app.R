library(tidyverse)
library(googlesheets4)
library(googledrive)
library(openxlsx)
library(RColorBrewer)
library(shiny)
library(plotly)


# Get the data from file
sheet <- "1ev0xA8pSKBMj77ufOBwppfJWWba26nMmBn5BgMsokCA"
online_data <- drive_get(id = sheet) 
final_data <- googlesheets4::read_sheet(online_data,sheet = "merged")

o <- final_data %>%
    mutate(Country = str_replace(Country, "United States of America", "USA"))

# Define key data for VIS
years <- as.character(unique(o$Year))
categories <- unique(o$Type)

# Define UI for app that draw a plot ----
ui <- fluidPage(

    # App title ----
    titlePanel("Power Infrastructure Vulnerability in Extreme Heat Events"),

    # Sidebar layout with input and output definitions ----
    sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(

            fluidRow(

                p("This panel lets you choose which of the datasets to visualise on the map!"),
                br(),
                
                # Selector for observation
                selectInput(
                    inputId = "obs",
                    label = "Choose the Data Type:",
                    choices = categories,
                    selected = categories[1]
                ),

                # Input: Radio buttons for the year ----
                radioButtons(
                    inputId = "year",
                    label = "Choose the Year:",
                    choices = years,
                    selected = years[1]
                )
           )
        ),

        # Main panel for displaying outputs ----
        mainPanel(
            # Output: Plot ----
            plotOutput(outputId = "mapPlot")
        )
    )
)

# Define server logic required to draw plot ----
server <- function(input, output) {

  # This expression that generates the plot is wrapped in a call
  # to renderPlot to indicate that:
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs change
  # 2. Its output type is a plot

  
  output$mapPlot <- renderPlot({
      
    # Load in a sample file
    YEAR <- substr(input$year, 1, 4)
    print(YEAR)
    DATA.TYPE <- input$obs

    # Find the data for this year
    plot.data <- o %>% 
        filter(Year == YEAR & Type == DATA.TYPE)

    ggplot() +
        geom_map(
            data = world, map = world,
            aes(long, lat, map_id = region),
            color="#2b2b2b", fill=NA, size=0.15
        ) + 
        geom_map(
            data = plot.data, map = world,
            aes(fill = Value, map_id=Country)
        ) + 
        scale_fill_gradient(low="yellow", high="red")
    }, height = 700, width = 1100)

}

# Get useful data ready
FOLDER <- "../new/"
world <- map_data("world")
files <- list.files(FOLDER, patter=".csv")

shinyApp(ui = ui, server = server)