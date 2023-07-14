# init.R
#
# Initialize
# Install libraries
#

my_packages = c("shiny", "shinydashboard", "ggplot2", "shinyBS", "fontawesome", "dplyr", "plotly", "magrittr", "igraph", "visNetwork")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
