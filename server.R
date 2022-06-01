
library(shiny)

# Define server logic required to draw a histogram
shinyServer(
  function(input, output) {
    sidebarServer(
      id = "nursery", 
      modelInput =  "nursery"
    )
    sidebarServer(
      id = "potSpawn", 
      modelInput =  "potSpawn"
    )
  }
)
