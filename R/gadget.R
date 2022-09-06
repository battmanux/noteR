library(shiny)

ui <- fluidPage(

  textOutput(outputId = "file"),
  textOutput(outputId = "txt")
)

server <- function(input, output, session) {

  observe({
    invalidateLater(1000)
    l_ctx <- rstudioapi::getSourceEditorContext()
    output$file <- renderText(l_ctx$path)
    output$txt <- renderText(l_ctx$contents)
  })
}


app <- shinyApp(ui, server )

if ( is.null(gViewer))
  gViewer <- options()$viewer
shiny::runGadget(app, viewer = gViewer)
