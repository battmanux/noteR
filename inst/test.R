library(shiny)

ui <- fluidPage(
  shiny::includeHTML("test.html"),
textInput(inputId = "toto", label = "jelo")
)

server <- function(input, output, session) {
  observe({
    invalidateLater(1000)
    insertUI(selector = "#nowhere", where = "beforeEnd", ui = list())
  })
}


shinyApp(ui, server )
