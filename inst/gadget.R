library(shiny)
library(data.table)
library(stringdist)

ui <- fluidPage(
  shiny::includeHTML("nav.html"),
  htmlOutput(outputId = "content")
)

searchString <- function(input, output, gState) {
  {
    if ( input$search == "" && gState$documentType == "qmd" ) {
      output$content <- renderUI(docParameters(gState$documentPath, gState))
      return()
    }

    rstudioapi::documentSaveAll()

    if (!is.null(input$search) && nchar(input$search) > 2) {
      suppressWarnings({
        l_res <- system(command = paste0("grep -R -i -I -n -H '", input$search, "' ",gState$projectFolder,"/* ") ,intern = T, ignore.stderr = T, timeout = 100)
      })

      if ( length(l_res) == 0 )
        return()

      l_list <- strsplit(l_res, ":")
      l_data <- data.table(
        path= unlist(lapply(l_list, function(x) x[[1]])),
        line= unlist(lapply(l_list, function(x) x[[2]])),
        match= unlist(lapply(l_list, function(x) paste(collapse = ":", x[3:length(x)]) ) )
      )

      l_data$file_type <- gsub(pattern = "^.*\\.([a-zA-Z]+)$", replacement = "\\1", x =l_data$path )



      l_data[,title:=getTitleFromQmdv(path)]
      l_data[,filename:= gsub(pattern = "^(.*)\\.[a-zA-Z]+$", replacement = "\\1", x = basename(path) )]

      l_data <- l_data[,.SD[1,], by=.(path)]

      l_data[,order:=0]
      l_data[,order:=order+(stringdist::stringdist(title, input$search)/nchar(title)) ]
      l_data[file_type=="qmd",order:=order-5]


      setkeyv(l_data, "order")

      output$content <- renderUI({

        if (nrow(l_data) > 0) {
          l_found <- tags$button(type="button", class="list-group-item",
                                 lapply(seq_len(nrow(l_data)), function(i) {
                                   x <-  l_data[i,]
                                   l_elem <- tags$a(class="list-group-item", target=x[,path],
                                                    onClick=paste0("Shiny.setInputValue('openfile', '",x[,path],"', {priority : 'event'})"),
                                                    tags$h4(x[,title], ' ', class="list-group-item-heading"),
                                                    tags$a(style="float:right;",
                                                           onClick=paste0("Shiny.setInputValue('link_to_file', '",x[,path],"', {priority : 'event'})"),
                                                           "link"),
                                                    tags$small(tags$p(x[,path])),
                                                    tags$p(x[,match], class="list-group-item-text")
                                   )
                                   return(l_elem)
                                 } )
          )
        } else {
          l_found <- list()
        }
        l_create <- tags$button(type="button", class="list-group-item",
                                tags$a(class="list-group-item", target=input$search,
                                       onClick=paste0("Shiny.setInputValue('new_file_name', '", input$search,"', {priority : 'event'})"),
                                       tags$h4(paste0("Create '", input$search, "'"),  class="list-group-item-heading"),
                                       tags$p(paste("Use default template"), class="list-group-item-text" )
                                )
        )
        tags$small(l_found, l_create)
      } )
    }
  }
}

getTitleFromQmd <-function(x) {
  l_content <- readLines(x)
  l_first_match <- grep(pattern = "^title: .*$", value = T, x = l_content)

  if (length(l_first_match) == 0)
    return( gsub(pattern = "^(.*)\\.[a-zA-Z]+$", replacement = "\\1", x = basename(x) ))

  l_title <- gsub("title: *", "", l_first_match[[1]])

  return(l_title)
}

docParameters <- function(path, gState) {
  l_params <- getParametersFromQmd(path)
  l_qmd <- l_params$qmd
  l_params$qmd <- NULL
  if (!is.null(l_params$logo)) {
    l_logo <- tags$img(src=l_params$logo, style='max-height:30px;margin-left: 10px;')
  } else {
    l_logo <- icon("file", style='max-height:30px;margin-left: 10px;')
  }
  l_backlinks <-  system(command = paste0("grep -R -i -l '](",normalizePath(path), ")' ",gState$projectFolder,"/* | grep 'qmd$' ") ,intern = T, ignore.stderr = T, timeout = 100)
  return(
    div(
      tags$h2(l_logo , l_qmd$title),
      tags$table(class="table",
                 lapply(names(l_params), function(n) {
                   tags$tr(tags$td(n), tags$td( l_params[[n]] ))
                 }
                 ) ),
      shiny::tags$p("backlinks"),
      shiny::tags$ul(
        lapply(l_backlinks, function(x) {
          shiny::tags$li(shiny::tags$a(href="#", onClick=paste0("Shiny.setInputValue('openfile', '",x,"', {priority : 'event'})"),
                                       getTitleFromQmd(x)))
        })
        ),
      shiny::HTML('
        <style>
            #drop_zone {
              border: 1px dash grey;
              width:  200px;
              height: 100px;
              margin: 10px;
            }
        </style>
        <div id="drop_zone" ondrop="dropHandler(event);" ondragover="dragOverHandler(event);">
          <p>Drag one or more files to this <i>drop zone</i>.</p>
        </div>
        <script>
function dropHandler(ev) {
  console.log(\'File(s) dropped\');

  // Prevent default behavior (Prevent file from being opened)
  ev.preventDefault();

  if (ev.dataTransfer.items) {
    // Use DataTransferItemList interface to access the file(s)
    [...ev.dataTransfer.items].forEach((item, i) => {
      // If dropped items aren\'t files, reject them
                    if (item.kind === \'file\') {
                      const file = item.getAsFile();
                      console.log(`… file[${i}].name = ${file.name}`);
                    }
  });
} else {
  // Use DataTransfer interface to access the file(s)
  [...ev.dataTransfer.files].forEach((file, i) => {
    console.log(`… file[${i}].name = ${file.name}`);
  });
}
}

function dragOverHandler(ev) {
  console.log(\'File(s) in drop zone\');

  // Prevent default behavior (Prevent file from being opened)
  ev.preventDefault();
}

        </script>')
    )
  )
}

getParametersFromQmd <-function(x) {
  l_content <- readLines(x)
  l_first_match <- which(grepl(pattern = "^(\`\`\`.*yaml)|(\\-\\-\\-)", x = l_content))
  l_last_match <- which(grepl(pattern = "^(\`\`\`)|(\\-\\-\\-)", x = l_content))

  l_out <- list()
  l_state <- 'out'
  for ( i in l_first_match) {
    if ( l_content[i] == "---" && l_state == 'out') {
      l_state <- "in"
    } else if ( l_content[i] == "---" && l_state == 'in') {
      l_state <- "out"
      next
    }

    e <- l_last_match[l_last_match>i][[1]]
    l_yaml <- yaml::yaml.load(
      paste(collapse = "\n", l_content[(i+1):(e-1) ])
    )

    if ( l_state == 'in' ) {
      l_out[[i]] <- list(qmd=l_yaml)
    } else {
      l_out[[i]] <- l_yaml
    }
  }

  l_ret <- do.call(c, l_out)
  return(l_ret)
}

getTitleFromQmdv <- Vectorize(getTitleFromQmd)

server <- function(input, output, session) {

  gProjectFolder <- rstudioapi::getActiveProject()
  if ( gProjectFolder == "" ) {
    gProjectFolder <- getwd()
  }
  cat("Folder: ", gProjectFolder, "\n")

  gState <- reactiveValues(
    projectFolder = gProjectFolder,
    documentId = "",
    documentType = "",
    documentPath = "",
    state = "idle",
    selection = "",
    gotofile = "",
    ctx = 0
  )

  observeEvent(input$show_prm,{

    l_ctx <- rstudioapi::getSourceEditorContext()
    gState$documentId <- l_ctx$id
    gState$documentPath <- l_ctx$path
    gState$documentType <- tolower(gsub(pattern = "^.*\\.([a-zA-Z]+)$", "\\1", x=l_ctx$path))

    updateTextInput(inputId = "search", value = "")

    if (gState$documentType == "qmd" ) {
      output$content <- renderUI(docParameters(gState$documentPath, gState))
    } else {
      output$content <- renderUI(list())
    }
  })


  observeEvent(input$search, {
    if ( nchar(input$search) >= 3){
      searchString(input, output, gState)
    }
  } )

  observeEvent(input$import_search, {
    if (input$search == "") {
      updateTextInput(inputId = "search", value = rstudioapi::selectionGet()$value)
    } else {
      searchString(input, output, gState)
    }
  })

  observeEvent(input$openfile, {
    if ( file.exists(input$openfile)) {
      gState$documentType <- gsub(pattern = "^.*\\.([a-zA-Z0-9]+)$", "\\1", input$openfile)
      gState$documentPath <- input$openfile
      rstudioapi::navigateToFile(input$openfile)

      updateTextInput(inputId = "search", value = "")

      if (gState$documentType == "qmd" ) {
        output$content <- renderUI(docParameters(gState$documentPath, gState))
      } else {
        output$content <- renderUI(list())
      }
    }
  })

  observeEvent(input$new_file_name, {
    l_name <- gsub(pattern = "[^a-zA-Z0-9]", replacement = "_", x = input$new_file_name)
    l_name <-  paste0(l_name, ".qmd")
    l_path <- paste0(gState$projectFolder,"/",l_name)
    l_content <- c(
      "---",
      "title: {title}",
      "format: html",
      "editor: visual",
      "---",
      "",
      "\`\`\`{yaml}",
      "Creation time: {now}",
      "\`\`\`",
      "",
      "## {title}",
      "")

    l_content <- gsub(pattern = "\\{title\\}", replacement = input$new_file_name, x = l_content)
    l_content <- gsub(pattern = "\\{now\\}", replacement = format(Sys.time()), x = l_content)

    cat(file = l_path, sep = "\n", l_content)

    rstudioapi::selectionSet(value = paste0("[",input$new_file_name,"](",l_path,")"))
    output$content <- renderUI(docParameters(l_path, gState))

    rstudioapi::navigateToFile(l_path)
    updateTextInput(inputId = "search", value = "")
  })

  observeEvent(input$link_to_file, {
    rstudioapi::selectionSet(value = paste0("[",rstudioapi::selectionGet(),"](",input$link_to_file,")"))
  })

}

if ( exists("gViewer")==T) {
  l_viewer <- gViewer
} else {
  l_viewer <- options()$viewer
}

shiny::runGadget(shinyApp(ui, server ), viewer = l_viewer )
