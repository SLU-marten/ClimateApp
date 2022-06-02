source("Logic/plotFunctions.R")
source("Modules/plot_module.R")

sidebarUI <- function(id, modelInput) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("species"),
          label = "Species",
          choices = get(modelInput) |> 
            select(Species) |> 
            unique()
        ),
        if(modelInput == "potSpawn"){
          uiOutput(outputId = ns("stockUI"))
        },
        checkboxGroupInput(
          inputId = ns("climateModel"),
          label = "Models",
          choices = get(modelInput) |> 
            select(Mod) |> 
            unique() |> 
            as.matrix() |> 
            c(),
          selected = get(modelInput) |> 
            select(Mod) |> 
            unique() |> 
            as.matrix() |> 
            c(),
        ),
        checkboxGroupInput(
          inputId = ns("climateScenario"),
          label = "Climate models",
          choices = get(modelInput) |> 
            select(RCP) |> 
            unique() |> 
            as.matrix() |> 
            c(),
          selected = get(modelInput) |> 
            select(RCP) |> 
            unique() |> 
            as.matrix() |> 
            c()
        ),
        radioButtons(
          inputId = ns("nutrientScenario"),
          label = "Nutrient scenario",
          choices = "BSAP"
        ),
        sliderInput(
          inputId = ns("years"),
          label = "Year",
          min = 2006,
          max = 2059,
          value = c(2006, 2059)
        ),
        actionButton(
          inputId = ns("choose"),
          label = "Choose"
        )
      ),
      column(
        width = 8,
        fluidRow(
          plotboxUI(
            id = ns("plot1"),
            title = "title1"
          ),
          plotboxUI(
            id = ns("plot2"),
            title = "title2"
          ),
          plotboxUI(
            id = ns("plot3"),
            title = "title3"
          ),
          box(id=ns("monthlyBox1"),
              title = "Monthly",
              collapsible = F,
              closable = F,
              maximizable = T,
              elevation = 2,
              plotOutput(
                outputId = ns("plot22"),
                width = "100%"
              )
          ),
        )
      )
    )
  )
}
sidebarServer <- function(id, modelInput) {
  moduleServer(
    id,
    function(input, output, session) {
      # Variables
      data <- reactiveValues(filter = NULL)
      # Stock selectInput if potSpawn
      output$stockUI <- renderUI({
        selectInput(
          inputId = session$ns("stock"),
          label = "Stock",
          choices = potSpawn |>
            filter(Species == input$species) |>
            select(Stock) |>
            unique(),
          multiple = T,
          selected = potSpawn |>
            filter(Species == input$species) |>
            select(Stock) |>
            unique(),
        )
      })
      # list of input values
      filterValues <- reactive({
        print(input$stock)
        list(
          species = input$species,
          model = input$climateModel,
          RCP = input$climateScenario,
          nutrient = input$nutrientScenario,
          year = input$years,
          stock = input$stock
        )
      })
      # Filtered data
      observeEvent(input$choose,{
        filtVal <- filterValues()
        df_filter <- get(modelInput) |>
          filter(
            Species %in% filtVal$species,
            Mod %in% filtVal$model,
            RCP %in% filtVal$RCP,
            between(Year, filtVal$year[1], filtVal$year[2])
          )
        if(modelInput == "potSpawn"){
          df_filter |> 
            filter(Stock %in% filtVal$stock)
        }
        data$filter <- df_filter
        plotboxServer(
          id = "plot1",
          plot =  plotMonthly(data$filter, modelInput),
          title = "title1"
        )
        plotboxServer(
          id = "plot2",
          plot =  plotMonthly2(data$filter, modelInput),
          title = "title2"
        )
        plotboxServer(
          id = "plot3",
          plot =  plotSpiral(data$filter, modelInput),
          title = "title3"
        )
        
      })
      # # Table
      # output$table1 <- renderTable({
      #   # data$filter
      # })



      # Plot 1
      output$plot22 <- renderPlot({
        req(input$choose)
        plotMonthly(data$filter, modelInput)
      })
      observeEvent(input$monthlyBox1$maximized, {
        if(input$monthlyBox1$maximized){
          runjs('var plot = document.querySelector("#nursery-plot22")
            plot.style.setProperty("height", "90vh", "important");
            plot.style.setProperty("width", "10%", "important");')
        } else {
          runjs('var nursery-plot22 = document.querySelector("#nursery-plot22")
            nursery-plot22.style.setProperty("height", "400px", "important");
            nursery-plot22.style.setProperty("width", "10%", "important");')
        }
      })
    }
  )
}