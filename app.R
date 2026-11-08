library(shiny)

ui <- navbarPage("Production Planning - Group 18",
  tabPanel("Learning Curve", learning_ui("learning")),
  forecasting_ui("forecasting"),   # navbarMenu: Forecasting > Data, Stationary, Trend, Seasonal, Box-Jenkins
  tabPanel("Aggregate Planning", aggregate_planning_ui("aggplan")))

server <- function(input, output, session) {
  learning_server("learning")
  forecasting_server("forecasting")
  aggregate_planning_server("aggplan")}

shinyApp(ui, server)
