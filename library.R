# Install required packages
options(repos = c(CRAN = "https://cran.rstudio.com/"))
install.packages(c("shiny", "httr", "jsonlite", "base64enc", "jpeg", "imager", "stringi", "DT", "shinycssloaders"))

# Load required libraries
library(shiny)
library(httr)
library(jsonlite)
library(base64enc)
library(jpeg)
library(imager)
library(stringi)
library(DT)
library(shinycssloaders)
