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
library(tidyr)
library(ggplot2)

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
          DTOutput("table"),
          plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # filtered data
  filtered <- reactive({dat_out <- dat_mut |> filter(
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
  return(dat_out)
  })
  # table
    output$table <- renderDT({
        select(filtered(), !all_of(cols_bin))
        }, options = list(
          pageLength = 20), rownames = FALSE)
    # graph
    output$plot <- renderPlot({
    dat_long <- tidyr::pivot_longer(filtered(), cols = cols_bin) #select(dat_out, all_of(cols_bin))
    ggplot(dat_long, aes(y = ifelse(value == "Included in survey", 1, 0), x = name, color = name)) + 
      geom_jitter(height = 0) +
      labs(x = "Open Science Practice", y = "Included in survey", title = "Open Science Practices in Surveys") + 
      theme_classic()
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
