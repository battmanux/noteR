library(shiny)

ui <- fluidPage(
  htmlOutput(outputId = "navbar", inline = T),
  htmlOutput(outputId = "search_bar", inline = T)
)

l_ctx <- NULL
server <- function(input, output, session) {

  gLast <- list(
    Id="",
    Path="",
    Content="",
    line=0,
    column=0,
    start_column=0)

  gMode <- "empty"

  observe({
    invalidateLater(500)
    l_ctx <- rstudioapi::getSourceEditorContext()

    lNew <- list(
      Id=l_ctx$id,
      Path=l_ctx$path,
      Content=paste(l_ctx$contents, collapse = "\n"),
      line=0,
      column=0,
      start_column=gLast$start_column
      )

    if ( length(l_ctx$selection) > 0 ) {
      lNew$line    <- l_ctx$selection[[1]]$range$start[["row"]]
      lNew$column  <- l_ctx$selection[[1]]$range$start[["column"]]
      l_line       <- l_ctx$contents[[ lNew$line ]]
      l_prev_char <- substr(l_line,
                            start = lNew$column-1,
                            stop =  lNew$column-1)
    } else {
      lNew$line   <- 0
      lNew$column <- 0
      l_line <- ""
      l_prev_char <- ""
    }

    if ( all( as.character(gLast) == as.character(lNew) ) ||
         length(l_ctx$contents) == 0 )
      return()

    lNewMode<- gMode

    ## decide in what mode we are
    if ( gMode == "empty" &&
         l_prev_char == "["
    ) {
      lNew$start_column <- lNew$column
      lNewMode<- "search"
    }

    if ( gMode == "search" &&
         gLast$line != lNew$line ||
                lNew$column < lNew$start_column  ) {
      lNew$start_column <- 0
      lNewMode <- "empty"
    }

    # str(lNew)
    # str(gLast)
    # str(list(gMode=gMode, lNewMode=lNewMode))

    gLast <<- lNew
    gMode <<- lNewMode

    ## act as per the current mode
    if ( lNewMode == 'search' ) {
      l_new_file <-   substr(x = gsub(pattern = "].*$", "", l_line),
                         start = lNew$start_column,
                         stop = 32)
      
      output$search_bar <- renderUI({
        shiny::tags$ul(
          shiny::tags$li("[search]"),
        shiny::tags$li(
          shiny::actionLink(
                            inputId = "new", 
                            label = "create "),
          shiny::textInput(inputId = "new_file_name", label = NULL, value = l_new_file)
          )
        )
      })
    }

    if ( lNewMode == "empty" ) {
      output$search_bar <- renderUI(shiny::div())
    }
    output$navbar <- renderText(l_ctx$path)
  })
  
  observeEvent(input$new, {
    l_new_file <-  paste0(input$new_file_name, ".qmd")
    l_content <- readLines("~/saved_data/templates/default.qmd", warn = F)
    cat(file = l_new_file, sep = "\n",
        gsub(pattern = "\\{title\\}", replacement = input$new_file_name, x = l_content)
    )
    rstudioapi::navigateToFile(l_new_file)
  })
}


app <- shinyApp(ui, server )

shiny::runGadget(app, viewer = ifelse(exists("gViewer"), gViewer, options()$viewer))
