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


startApp <- function() {
  tmpfile <- tempfile(fileext = ".url")
  on.exit({unlink(tmpfile)})
  gViewer <- function (url, height = NULL) {
    writeChar(url, con = tmpfile)
  }
  rstudioapi::jobRunScript("R/gadget.R", importEnv = T)
  l_url <- readLines(tmpfile, warn = F)
  for ( i in 1:10) {
    if ( file.exists(tmpfile))
      break
    else
      Sys.sleep(0.1)
  }
  w<-options()$viewer
  w(l_url)
}
