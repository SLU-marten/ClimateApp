
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
            c()
        ),
        checkboxGroupInput(
          inputId = ns("climateScenario"),
          label = "Climate models",
          choices = get(modelInput) |> 
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
          column(
            width = 6,
            tableOutput(
              outputId = ns("table1")
            ),
            plotOutput(
              outputId = ns("plot2")
            )
          ),
          column(
            width = 6,
            plotOutput(
              outputId = ns("plot3")
            ),
            dataTableOutput(
              outputId = ns("plot4")
            )
          )
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
          inputId = "stock",
          label = "Stock",
          choices = potSpawn |>
            filter(Species == input$species) |>
            select(Stock) |>
            unique()
        )
      })
      # list of input values
      filterValues <- reactive(
        list(
          species = input$species,
          model = input$climateModel,
          RCP = input$climateScenario,
          nutrient = input$nutrientScenario,
          year = input$years,
          stock = input$stock
        )
      )
      # Filtered data
      observeEvent(input$choose,{
        filtVal <- filterValues()
        df_filter <- get(modelInput) |>
          filter(
            Species == filtVal$species,
            Mod == filtVal$model,
            RCP == filtVal$RCP,
            between(Year, filtVal$year[1], filtVal$year[2])
          )
        if(modelInput == "potSpawn"){
          df_filter |> 
            filter(stock == filtVal$stock)
        }
        data$filter <- df_filter
      })
      # Table
      output$table1 <- renderTable({
        data$filter
      })
    }
  )
}