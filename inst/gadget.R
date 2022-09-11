library(shiny)

ui <- fluidPage(
  shiny::includeHTML("nav.html"),
  htmlOutput(outputId = "content")
)

server <- function(input, output, session) {

  gProjectFolder <- "/home/ebatt/Rstudio/notes/"

  gState <- reactiveValues(
    documentId = "",
    documentType = "",
    documentPath = "",
    state = "idle",
    selection = "",
    ctx = 0
  )

  gState_ctx <- list()

  observe({
    gState_ctx <<- rstudioapi::getSourceEditorContext()
    invalidateLater(500)
    if ( gState$documentId != gState_ctx$id  ) {
      gState$ctx <- gState$ctx + 1
    }
  })

  observeEvent(gState$ctx, {
    str(gState)
    l_ctx <- gState_ctx
    if ( gState$documentId != l_ctx$id ) {
      gState$documentId <- l_ctx$id
      gState$documentPath <- l_ctx$path
      gState$documentType <- tolower(gsub(pattern = "^.*\\.([a-zA-Z]+)$", "\\1", x=l_ctx$path))
      gState$selection <- rstudioapi::selectionGet(id=l_ctx$id)$value

      if ( length(l_ctx$contents) > 0 ) {
        gState$documentContent <- l_ctx$contents
      } else {
        rstudioapi::documentSave(l_ctx$id)
        gState$documentContent <-readLines( gState$documentPath, warn = F)
      }
    } else {
      if ( length(l_ctx$selection) > 0 ) {
        l_new_sel <- l_ctx$selection[[1]]$text
      } else {
        l_new_sel <- rstudioapi::selectionGet(id=l_ctx$id)$value
      }

      if ( gState$selection != l_new_sel ) {
        gState$selection <- l_new_sel
      }
    }
  })

  observeEvent(gState$selection, {
    updateTextInput(inputId = "search", value = gState$selection )
  })

  observeEvent(gState$documentId, {
    ## Parse doc
    if (gState$documentType == "qmd" ) {
      output$content <- renderUI({
        div(
          tags$h2("title"),
          tags$ul(
            tags$li("toto")
          )
        )
      })
    } else {
      output$content <- renderUI({
        div(
          tags$h2("other doc"),
          tags$ul(
            tags$li("some doc")
          )
        )
      })

    }
  })

  observeEvent(input$search, {

    if (!is.null(input$search) && nchar(input$search) > 2) {
      suppressWarnings({
        l_res <- system(command = paste0("grep -R -i -I -n -H '", input$search, "' ",gProjectFolder," ") ,intern = T, ignore.stderr = T, timeout = 100)
      })

      output$content <- renderUI({
        tags$button(type="button", class="list-group-item",
               lapply(strsplit(l_res[1:min(10, length(l_res))], ":"), function(x) {
                 l_elem <- tags$a(class="list-group-item", target=x[[1]],
                     tags$h4(basename(x[[1]]), ' ', class="list-group-item-heading"),
                     tags$p(paste(x[3:length(x)], collapse = ":"), class="list-group-item-text" )
                   )
                 return(l_elem)
              } )
        )
      } )
    }
  } )

  ## TODO: reactive value : document id
  ## on link creation, if visual, inset {create link to} + save + replace + reload
  # rstudioapi::selectionSet("fdf", id = "25EDE74F")

  observeEvent(input$new, {
    l_new_file <-  paste0(input$new_file_name, ".qmd")
    l_content <- readLines("~/saved_data/templates/default.qmd", warn = F)
    cat(file = l_new_file, sep = "\n",
        gsub(pattern = "\\{title\\}", replacement = input$new_file_name, x = l_content)
    )
    rstudioapi::navigateToFile(l_new_file)
  })
}

if ( exists("gViewer")==T) {
  l_viewer <- gViewer
} else {
  l_viewer <- options()$viewer
}

shiny::runGadget(shinyApp(ui, server ), viewer = l_viewer )
