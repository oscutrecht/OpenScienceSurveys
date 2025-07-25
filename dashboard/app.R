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
dat_raw <- read_sheet(sheet_id, skip = 1, col_types = "c", trim_ws = TRUE)
# cols_bin <- apply(dat_raw, 2, unique) |>
#   sapply(function(x)
#     length(x) <= 2) |>
#   which() |>
#   names()
OS_practices <- c(
  "Open access",
  "Open code",
  "Open data",
  "Open educational resources",
  "Open peer-review",
  "Pre-printing",
  "Pre-registration",
  "Public engagement"
)
dat_mut <- dat_raw |>
  mutate(
    `Start data collection` = as.numeric(`Start data collection`),
    `End data collection` = as.numeric(`End data collection`)
  ) |>
  mutate(across(
    all_of(OS_practices), as.numeric#~case_when(. == 1 ~ "Included in survey" , . == 0 ~ "Not included")
  ))
# dat_mut <- dat_mut[, c(1, 6:(ncol(dat_mut) - 1), 2:5, ncol(dat_mut))]

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Open Science Surveys"),
  # h3("Operationalisation of Open Science in Survey Research"),
  em("What is asked in surveys investigating the uptake of Open Science? Use this dashboard to find out, and reuse questionnaire items!"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # h4("Studies"),
      sliderInput(
        "year",
        "Year conducted:",
        min = 2010,
        max = 2025,
        value = c(2010, 2025),
        step = 1,
        sep = NULL,
        round = TRUE
      ),
      radioButtons(
        "discipline",
        "Academic discipline",
        choices = c("Any", "Social sciences only", "STEM only"),
        selected = c("Any")
      ),
      radioButtons(
        "region",
        "Region",
        choices = c("Any", "EU/UK only", "USA only"),
        selected = c("Any")
      ),
      radioButtons(
        "target",
        "Target population",
        choices = c("Any", "Academic only", "ECR only"),
        selected = c("Any")
      ),
      # checkboxGroupInput(
      #   "practices",
      #   "Open Science practices",
      #   choices = c(
      #     "Open Access",
      #     "Pre-printing",
      #     "Open Peer-review",
      #     "Open Data",
      #     "Open Code",
      #     "Pre-registration",
      #     "Open Educational Resources",
      #     "Public Engagement"
      #   ),
      #   selected = c(
      #     "Open Access",
      #     "Pre-printing",
      #     "Open Peer-review",
      #     "Open Data",
      #     "Open Code",
      #     "Pre-registration",
      #     "Open Educational Resources",
      #     "Public Engagement")
      #   )
      ),
        
          # Show a plot of the generated distribution
       mainPanel(
            tabsetPanel(
              tabPanel("Summary",
                # "Summary",
                plotOutput("plot")
                ),
              tabPanel("Study details",
                # "Studies",
                DTOutput("table"))
              )
          )
      ),
  p("Research conducted at the ", a(href="https://openscienceretreat.eu/", "Open Science Retreat 2024"), " by Lotte van Burgsteden, Tanya van Goch, Bogdana Huma, Anne Marie Meijer, Iris Smal and Hanne Oberman. Materials available via ", a(href="https://zenodo.org/records/10932819", "Zenodo"), ".")
)
      
      # Define server logic required to draw a histogram
      server <- function(input, output) {
        # filtered data
        filtered <- reactive({
          dat_out <- dat_mut |> filter(
            # year
            (is.na(`Start data collection`) |
                `Start data collection` >= input$year[1]
            ) & (is.na(`End data collection`) |
                  `End data collection` <= input$year[2]
              )) 
          
          # # %>% 
          #     input$region == "Any" |
          #       `Region` %in% input$region ifelse(input$discipline == "Any", ., filter(., discipline_cat == input$discipline))

          # if (input$discipline != "Any") {
          #   dat_out <- filter(dat_out, discipline_cat == input$discipline)
          #   # region_cat == input$region,
          #   # target_cat == input$target
          #   
          # }
            
        })
        
        # table
        output$table <- renderDT({
          dat_subset <- select(
            filtered(),
            c(
              "Citation",
              "Start data collection",
              "End data collection",
              all_of(OS_practices),
              "OS themes",
              "Open survey materials",
              "Open survey data",
              "Supplementary files"
            ) 
          ) |>
          mutate(across(c(all_of(OS_practices),
                 "Open survey materials",
                 "Open survey data"),
                 ~case_when(. == 1 ~ "yes" , . == 0 ~ "no"))) 
          
                 # ifelse(1, "yes", "no")))
          # dat_subset[dat_subset == "1"] <- "yes"
          # dat_subset[dat_subset == "0"] <- "no"
        }, options = list(pageLength = 20), rownames = FALSE)
        # graph
        output$plot <- renderPlot({
          dat_props <- data.frame(practice = OS_practices, prop = colMeans(filtered()[, OS_practices]) * 100)
          dat_props <- sort_by(dat_props, dat_props$prop) 
          dat_props$ordered <- factor(dat_props$practice, levels = dat_props$practice, ordered = TRUE)
          
          ggplot(dat_props, aes(y = ordered, x = prop, fill = practice)) +
            geom_bar(stat = "identity") +
            labs(x = "Inclusion rate (%)", y = "Open Science practice", fill = NULL) +
            theme_minimal()
           
          # dat_long <- dat_mut |># dat_long <- dat_mut |>NULL
          #   tidyr::pivot_longer(cols = all_of(OS_practices), names_to = "OS_practice", values_to = "practice_included", values_transform = as.numeric)
          # props <- dat_long |> group_by(OS_practice) |> summarise(m = mean(practice_included))
          # order <- sort_by(props, props$m)$OS_practice
          # dat_long <- mutate(dat_long,
          #                    OS_practice = factor(OS_practice, levels = order, ordered = TRUE))
          # ggplot(dat_long, aes(x = practice_included, y = OS_practice, fill = OS_practice)) +
          #   geom_jitter() # +
          
          
            # scale_y_continuous(labels=percent_format())
          # 
          # dat_long <- tidyr::pivot_longer(filtered(), cols = cols_bin) #select(dat_out, all_of(cols_bin))
          # ggplot(dat_long, aes(
          #   y = ifelse(value == "Included in survey", 1, 0),
          #   x = name,
          #   color = name
          # )) +
          #   geom_jitter(height = 0) +
          #   labs(x = "Open Science Practice", y = "Included in survey", title = "Open Science Practices in Surveys") +
          #   theme_classic()
        })
      }
      
      # Run the application
      shinyApp(ui = ui, server = server)
      