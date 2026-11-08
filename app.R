library(shiny)

ui <- navbarPage("Production Planning - Group 18",
  tabPanel("Learning Curve", learning_ui("learning")),
  forecasting_ui("forecasting")   # navbarMenu: Forecasting > Data, Stationary, Trend, Seasonal, Box-Jenkins
)

server <- function(input, output, session) {
  learning_server("learning")
  forecasting_server("forecasting")
}

shinyApp(ui, server)
