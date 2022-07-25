library(shiny)
library(shinyBS)
library(flexdashboard)
library(dplyr)
source("tachometer_data_prep.R")

all_data <- read.table(file = "Data_Nevena_OneRow.csv", head = TRUE, sep = ",")

get_factor_values <- function(factor) {
  all_factors <- read.table(file = "factors.csv", head = TRUE, sep = ",")
  fv <- dplyr::filter(all_factors, category == factor)
  fv <- fv[order(fv$value), ]

  return(split(fv$value, fv$description))
}

list_vectors <-
  get_factor_values("Branche")

create_tachometer <-
  function(data) {
    ui <- fluidPage(
      titlePanel(h1("Benchmarks", align = "center")),
      fluidRow(
        br(),
        br(),
        column(3),
        column(
          3,
          selectInput(
            "Branche", "Industry:",
            get_factor_values("Branche")
          )
        ),
        column(
          3,
          selectInput(
            "Groe", "Organizational Size:",
            get_factor_values("Groe")
          )
        ),
        column(3)
      ),
      br(),
      br(),
      fluidRow(
        column(
          3,
          gaugeOutput("gauge_satisfaction")
        ),
        bsTooltip("gauge_satisfaction", "Item for Job Satisfaction",
          placement = "bottom", trigger = "hover",
          options = NULL
        ),
        column(
          3,
          gaugeOutput("gauge_motivation")
        ),
        bsTooltip("gauge_motivation", "Item for Job Motivation",
          placement = "bottom", trigger = "hover",
          options = NULL
        ),
        column(
          3,
          gaugeOutput("gauge_recommendation")
        ),
        bsTooltip("gauge_recommendation", "Item for Job Recommendation",
          placement = "bottom", trigger = "hover",
          options = NULL
        ),
        column(
          3,
          gaugeOutput("gauge_retention")
        ),
        bsTooltip("gauge_retention", "Item for Job Retention",
          placement = "bottom", trigger = "hover",
          options = NULL
        ),
      )
    )
    server <- function(input, output) {
      output$gauge_satisfaction <- renderGauge({
        rate <- calculate_col_mean_with_filter(all_data, "satisfaction", list(list("Branche", input$Branche), list("Groe", input$Groe)))
        gauge(round(rate, digits = 2),
          min = 1,
          max = 7,
          label = "Job Satisfaction",
          sectors = gaugeSectors(
            danger = c(1, 3),
            warning = c(3, 5),
            success = c(5, 7)
          )
        )
      })
      output$gauge_motivation <- renderGauge({
        rate <- calculate_col_mean_with_filter(all_data, "motivation", list(list("Branche", input$Branche), list("Groe", input$Groe)))
        gauge(round(rate, digits = 2),
          min = 1,
          max = 7,
          label = "Job Motivation",
          sectors = gaugeSectors(
            danger = c(1, 3),
            warning = c(3, 5),
            success = c(5, 7)
          )
        )
      })
      output$gauge_recommendation <- renderGauge({
        rate <- calculate_col_mean_with_filter(all_data, "recommendation", list(list("Branche", input$Branche), list("Groe", input$Groe)))
        gauge(round(rate, digits = 2),
          min = 1,
          max = 7,
          label = "Job Recommendation",
          sectors = gaugeSectors(
            danger = c(1, 3),
            warning = c(3, 5),
            success = c(5, 7)
          )
        )
      })
      output$gauge_retention <- renderGauge({
        rate <- calculate_col_mean_with_filter(all_data, "retention", list(list("Branche", input$Branche), list("Groe", input$Groe)))
        gauge(round(rate, digits = 2),
          min = 1,
          max = 7,
          label = "Retention",
          sectors = gaugeSectors(
            danger = c(1, 3),
            warning = c(3, 5),
            success = c(5, 7)
          )
        )
      })
    }

    shiny::shinyApp(ui = ui, server = server)
  }

create_tachometer()
