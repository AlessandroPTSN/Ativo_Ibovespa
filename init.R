# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("tidyverse","rvest","ggfortify","ggdendro","plotly","dplyr","shiny","shinyWidgets","readxl","tidyr","shinythemes","stringr","kableExtra","highcharter","ggplot2")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))


