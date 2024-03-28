#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# packages
library(shiny)
library(dplyr)
library(googlesheets4)

# dataset
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1JuRPvEEaBwA2nWU8ptUF7Uq8jrtwlt3etAj5PvRcT40/"
dat <- read_sheet(sheet_id, skip = 1, col_types = "c")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Open Science Surveys"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year conducted:",
                        min = 2010,
                        max = 2025,
                        value = c(2018, 2024),
                        step = 1, 
                        sep = NULL,
                        round = TRUE)
            ),
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("table")
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


    
    # my_data <- readxl::read_excel("./dashboard/data/OpenScienceSurveys.xlsx", skip = 1)
    output$table <- renderTable({
        dat |> 
            mutate(
                start_year = as.numeric(`Start data collection`),
                end_year = as.numeric(`End data collection`)
                ) |>
            filter(
                (is.na(start_year) | start_year >= input$year[1]) & 
                    (is.na(end_year) | end_year <= input$year[2]) 
            )
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
