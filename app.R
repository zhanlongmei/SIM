
library(shiny)
library(shinythemes)
library(VIM)
library(mice)
library(tidyverse)
library(pheatmap)
library(pcaMethods)
library(VennDiagram)
library(MissMech)
library(missForest)
library(MASS)
library(impute)
library(BaylorEdPsych)
library(cluster)
library(factoextra)
library(imputeLCMD)
library(FactoMineR)
library(reshape2)

source(file.path("tools", "tools.R"),  local = TRUE)

ui <- navbarPage(
  title = "SIM: Stepwise Imputation for Metabolomics Data",
  source(file.path("ui", "Ututorial.R"),  local = TRUE)$value,
  source(file.path("ui", "UuploadVisiualization.R"),  local = TRUE)$value,
  source(file.path("ui", "UBiological_induced_missing_imputation.R"),  local = TRUE)$value,
  source(file.path("ui", "Uremaining_missing_imputation.R"),  local = TRUE)$value,
  source(file.path("ui", "Uimputation_evaluation.R"),  local = TRUE)$value
)

server <- function(input, output, session) {
  source(file.path("server", "Stutotrial.R"),  local = TRUE)$value
  source(file.path("server", "SuploadVisiualization.R"),  local = TRUE)$value
  source(file.path("server", "SBiological_induced_missing_imputation.R"),  local = TRUE)$value
  source(file.path("server", "Sremaining_missing_imputation.R"),  local = TRUE)$value
  source(file.path("server", "Simputation_evaluation.R"),  local = TRUE)$value
  
}

shinyApp(ui = ui, server = server)


