
.onLoad <- function(libname, pkgname) {

  if (Sys.getenv("USER") == 'root') {
    system("cp /usr/lib/rstudio-server/www/js/panmirror/panmirror.js /usr/lib/rstudio-server/www/js/panmirror/panmirror.js.backup")
    system("sed -i \"s/e.ui.display.openURL(e.link.href)/e.link.href.match(\\\':\\\\/\\\\/\\\') ? e.ui.display.openURL(e.link.href) : e.ui.display.navigateToXRef(e.link.href, {file:e.link.href, type: \\\'\\\', id: \\\'\\\', suffix: \\\'\\\', title: \\\'\\\'})/\" /usr/lib/rstudio-server/www/js/panmirror/panmirror.js")
  }

  if ( identical(.Platform$GUI, "RStudio") ) {

    l_version <- rstudioapi::versionInfo()

    if ( l_version$long_version == "2022.07.1+554" ) {
      knitr::knit_engines$set(`mermaid`=knit_mermaid_554)
      knitr::knit_engines$set(`dot`=knit_dot_554)
    }
  }

  knitr::knit_engines$set(`yaml`=knit_yaml_data)

}
