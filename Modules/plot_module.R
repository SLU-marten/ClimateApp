source("Modules/download_module.R")

plotboxUI <- function(id, title) {
  ns <- NS(id)
  tagList(
    box(
      id = ns("plotbox"),
      label = downloadUI(id = ns(title)),
      title = title,
      collapsible = F,
      closable = F,
      maximizable = T,
      elevation = 2,
      plotOutput(
        outputId = ns("plot"),
        width = "100%",
        height = "600px"
      )
    )
  )
}

plotboxServer <- function(id, plot, title) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        plot
      })
      downloadServer(
        id = title,
        species = "Abborre",
        plotname = "plot1", 
        plot = plot
      )
    }
  )
}