library(shiny)
library(rvest)
library(ggfortify)
library(ggdendro)
library(plotly)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(readxl)
library(tidyr)
library(shinythemes)
library(stringr)
library(kableExtra)
library(highcharter)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(DT)
library(rdrop2)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)
