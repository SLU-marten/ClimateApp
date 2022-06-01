library(shiny)
library(dplyr)
source("Modules/ui_module.R")
shinyUI(
  navbarPage(
    title = "",
    theme = bslib::bs_theme(4),
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
