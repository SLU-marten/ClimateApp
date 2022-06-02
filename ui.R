library(shiny)
library(dplyr)
library(ggridges)
library(ggplot2)
library(tidyr)
library(bs4Dash)
library(shinyjs)

source("Modules/ui_module.R")

shinyUI(
  dashboardPage(
    dark = NULL,
    header = dashboardHeader(),
    sidebar = dashboardSidebar(disable = T),
    body = dashboardBody(
      tabsetPanel(
      tabPanel(
        title = "Nursery areas",
        value = "nurseryTab",
        sidebarUI(
          id = "nursery", 
          modelInput = "nursery"
        )
      ),
      tabPanel(
        title = "Potential spawning areas",
        value = "potSpawnTab",
        sidebarUI(
          id = "potSpawn", 
          modelInput = "potSpawn"
        )
      )
      )
    )
  )
)
