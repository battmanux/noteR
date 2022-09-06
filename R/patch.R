

knit_mermaid_554 <- function (options) {
  mmdoc <- "
  <!DOCTYPE html>
  <html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
  </head>
  <body>
    <div class=\"mermaid\" style=\"text-align: center;\">
    <mmCode/>
    </div>
   <script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>
   <script>mermaid.initialize({startOnLoad:true});
  </script>
  </body>
  </html>"

  knit_print <- get("knit_print", envir = asNamespace("knitr"))
  engine_output <- get("engine_output", envir = asNamespace("knitr"))

  if (identical(.Platform$GUI, "RStudio") )  {
    widget <- htmltools::HTML(gsub(
      pattern = "<mmCode/>",
      replacement = paste0(collapse = "\n", options$code),
      x = mmdoc))
  } else {
    widget <- knitr:::one_string(c("```{mermaid}", options$yaml.code,
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
          c("``` {.r}", options$yaml.code, options$code, "``` "),
        if (options$results != "hide" && !knitr:::is_blank(out) ) {
          c("```{mermaid}", options$yaml.code, options$code, "```")
        }
      )
    )
  }
}


knit_dot_554 <- function (options) {

  dotdoc  <- "
    <html>
    <head></head>
    <body>
    <script src=\"https://github.com/mdaines/viz.js/releases/download/v2.1.2/viz.js\"></script>
    <script src=\"https://github.com/mdaines/viz.js/releases/download/v2.1.2/full.render.js\"></script>
      <div id=\"dotContainer\" style=\"text-align: center;\">
        <script>
          var viz = new Viz();

          viz.renderSVGElement(`<dotCode/>`)
          .then(function(element) {
            document.getElementById(\"dotContainer\").appendChild(element);
          })
          .catch(error => {
            // Create a new Viz instance (@see Caveats page for more info)
            viz = new Viz();

            // Possibly display the error
            console.error(error);
          });
        </script>
      </div>
    </body>
    </html>
    "

  knit_print <- get("knit_print", envir = asNamespace("knitr"))
  engine_output <- get("engine_output", envir = asNamespace("knitr"))

  if (identical(.Platform$GUI, "RStudio") )  {
    widget <- htmltools::HTML(gsub(
      pattern = "<dotCode/>",
      replacement = paste0(collapse = "\n", options$code),
      x = dotdoc))
  } else {
    widget <- knitr:::one_string(c("```{dot}", options$yaml.code,
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
          c("``` {.r}", options$yaml.code, options$code, "``` "),
        if (options$results != "hide" && !knitr:::is_blank(out) ) {
          c("```{dot}", options$yaml.code, options$code, "```")
        }
      )
    )
  }
}


