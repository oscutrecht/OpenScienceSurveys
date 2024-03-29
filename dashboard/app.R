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
library(DT)

# dataset
gs4_deauth()
sheet_id <- "https://docs.google.com/spreadsheets/d/1JuRPvEEaBwA2nWU8ptUF7Uq8jrtwlt3etAj5PvRcT40/"
dat_raw <- read_sheet(sheet_id, skip = 1, col_types = "c")
cols_bin <- apply(dat_raw, 2, unique) |> 
    sapply(function(x) length(x) <= 3) |> 
    which() |> 
    names()
dat_mut <- dat_raw |> 
    mutate(
      `Start data collection` = as.numeric(`Start data collection`),
      `End data collection` = as.numeric(`End data collection`)
    ) |>
    mutate(across(all_of(cols_bin), ~case_when(. == 1 ~ "Included in survey" , . == 0 ~ "Not included")))
dat_mut <- dat_mut[, c(1, 6:(ncol(dat_mut)-1), 2:5, ncol(dat_mut))]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Open Science Surveys"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("o_access", "Open Access", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),    
            selectInput("o_preprint", "Pre-printing", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            selectInput("o_peer", "Open Peer-review", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            selectInput("o_data", "Open Data", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            selectInput("o_code", "Open Code", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            selectInput("o_prereg", "Pre-registration", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            selectInput("o_er", "Open Educational Resources", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            selectInput("o_pe", "Public Engagement", choices = c("Included in survey", "Not included", "No filter"), selected = "No filter"),
            
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
            DTOutput("table")
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- renderDT({
            dat_out <- dat_mut |> filter(
                (is.na(`Start data collection`) | `Start data collection` >= input$year[1]) & 
                    (is.na(`End data collection`) | `End data collection` <= input$year[2]))
                    
            if (input$o_access != "No filter") {
              dat_out <- filter(dat_out, `Open access` == input$o_access)
            }
            if (input$o_preprint != "No filter") {
              dat_out <- filter(dat_out, `Pre-printing` == input$o_preprint)
            }
            if (input$o_peer != "No filter") {   
              dat_out <- filter(dat_out, `Open peer-review` == input$o_peer)
            }
            if (input$o_data != "No filter") {
              dat_out <- filter(dat_out, `Open data` == input$o_data)
            }
            if (input$o_code != "No filter") {
              dat_out <- filter(dat_out, `Open code` == input$o_code)
            }
            if (input$o_prereg != "No filter") {
              dat_out <- filter(dat_out, `Pre-registration` == input$o_prereg)
            }
            if (input$o_er != "No filter") {
              dat_out <- filter(dat_out, `OER` == input$o_er)
            }
            if (input$o_pe != "No filter") {
              dat_out <- filter(dat_out, `Public engagement` == input$o_pe)
            }
            dat_out <- select(dat_out, !all_of(cols_bin))
            bar_graph select(dat_out, all_of(cols_bin))
            
        return(dat_out)
        }, options = list(
          pageLength = 20), rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
