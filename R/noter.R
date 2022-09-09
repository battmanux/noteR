#  noteR
#
# This provides YAML parsing and editing using shiny app
#



knit_yaml_data <- function (options) {

  dotdoc  <- "
    <html>
    <head></head>
    <body>
    <h1><yamlCode/></h1>
    </body>
    </html>
    "

  knit_print <- get("knit_print", envir = asNamespace("knitr"))
  engine_output <- get("engine_output", envir = asNamespace("knitr"))

  if (identical(.Platform$GUI, "RStudio") )  {
    widget <- htmltools::HTML(gsub(
      pattern = "<yamlCode/>",
      replacement = paste0(collapse = "\n", options$code),
      x = dotdoc))
  } else {
    widget <- knitr:::one_string(c("```{yaml}", options$yaml.code,
                                   options$code, "```"))
  }

  if (identical(.Platform$GUI, "RStudio")) {
    knitr::opts_current$set(engine=NULL)
    print(widget)
    list()
  }
  else {
    code <- options$code
    out <- widget
    knitr:::one_string(
      c(
        if (length(options$echo) > 1L || options$echo)
          c("``` {.yaml}", options$yaml.code, options$code, "``` "),
        if (options$results != "hide" && !knitr:::is_blank(out) ) {
          c("```{yaml}", options$yaml.code, options$code, "```")
        }
      )
    )
  }
}

gValues <- new.env(parent = emptyenv())

stopApp <- function() {
  if ( gValues$gLastStartId != "" ) {
    try({
      rstudioapi::jobRemove(gValues$gLastStartId)
    })
  }
  gValues$gLastStartUrl <- ""
}

startApp <- function() {
  tmpfile <- tempfile(fileext = ".url")

  #dir.create(gsub(pattern = "/[^/]*$", replacement = "", x = tmpfile), recursive = T, showWarnings = F)

  on.exit({unlink(tmpfile)})
  .GlobalEnv$gViewer <- function (url, height = NULL) {
    cat(url, file = tmpfile)
  }
  if ( gValues$gLastStartId != "" ) {
    try({
      rstudioapi::jobRemove(gValues$gLastStartId)
    })
    gValues$gLastStartUrl <- ""
  }

  path <- system.file("gadget.R", package="noteR")
  #path <- "inst/gadget.R"

  gValues$gLastStartId <- rstudioapi::jobRunScript(path, importEnv = T)

  Sys.sleep(0.1)
  for ( i in 1:10) {
    if ( file.exists(tmpfile))
      break
    else
      Sys.sleep(0.1)
  }
  l_url <- readLines(tmpfile, warn = F)

  w<-options()$viewer
  w(l_url)
  gValues$gLastStartUrl <- l_url
}


