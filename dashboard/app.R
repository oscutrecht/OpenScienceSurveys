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
dat_raw <- read_sheet(sheet_id, skip = 1, col_types = "c")
cols_bin <- apply(dat_raw, 2, unique) |> 
    sapply(function(x) length(x) <= 3) |> 
    which() |> 
    names()
dat_num <- dat_raw |> 
    mutate(
        start_year = as.numeric(`Start data collection`),
        end_year = as.numeric(`End data collection`)
    ) |>
    mutate(across(all_of(cols_bin), as.numeric))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Open Science Surveys"),
    checkboxInput("filtering", "Filter surveys", value = FALSE),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxInput("o_access", "Open Access", value = TRUE),
            checkboxInput("o_preprint", "Pre-printing", value = TRUE),
            checkboxInput("o_peer", "Open Peer-review", value = TRUE),
            checkboxInput("o_data", "Open Data", value = TRUE),
            checkboxInput("o_code", "Open Code", value = TRUE),
            checkboxInput("o_prereg", "Pre-registration", value = TRUE),
            checkboxInput("o_er", "Open Educational Resources", value = TRUE),
            checkboxInput("o_pe", "Public Engagement", value = TRUE),
            
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
    output$table <- renderTable({
        if (input$filtering) {
            dat_out <- dat_num |> filter(
                (is.na(start_year) | start_year >= input$year[1]) & 
                    (is.na(end_year) | end_year <= input$year[2]) & 
                    (`Open access` == input$o_access) & 
                    (`Pre-printing` == input$o_preprint) & 
                    (`Open peer-review` == input$o_peer) & 
                    (`Open data` == input$o_data) & 
                    (`Open code` == input$o_code) & 
                    (`Pre-registration` == input$o_prereg) & 
                    (`OER` == input$o_er) & 
                    (`Public engagement` == input$o_pe)
            )} else {dat_out <- dat_num}
        return(dat_out)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
