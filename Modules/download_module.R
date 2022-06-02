downloadUI <- function(id) {
  ns <- NS(id)
  tagList(
    boxLabel(
      downloadButton(
        outputId = ns("downloadPNG"), 
        label = "Download",
        class = "btn-xs"), 
      status = "info"
    )
  )
}
downloadServer <- function(id, species, plotname, plot) {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadPNG <- downloadHandler(
        filename = function(){
          paste0(plotname, "_", species, ".png")
          },
        content = function(file){
          ggsave(file, plot = plot)
        }
      )
    }
  )
}

