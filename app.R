library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "bioviewer"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dot Plots", tabName = "dotplots", icon = icon("th"))
    ), width = 150
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = "dotplots",
              fluidRow(
                box(title = "Data Upload", width = 12, solidHeader = T, status = "primary",
                    h5("Please upload a csv file with a header row and one column for each variable"),
                    h6("Files should be less than 200 MBs"),
                    fileInput(inputId = "file", label = NULL, multiple = F, accept = "text/csv"),
                    h5("-OR-"),
                    actionButton(inputId = "runExample", label = "Try example"),
                    actionButton(inputId = "downloadExample", label = "Download example csv")
                    )
              ),
              fluidRow(
                box(width = 9, status = "warning",
                    verbatimTextOutput(outputId = "warning")),
                box(width = 3, status = "success")
              )
      )
    )
  )
)

  
  
  
server <- function(input, output) {
  
  #initialize reactive variable to store data
  value <- reactiveValues()
  
  observeEvent(input$file, {
    value$data <- read.csv(input$file$datapath)
  })
  
  observeEvent(input$runExample, {
    value$data <- iris
  })
  
   
  output$warning <- renderPrint({
    #value$data <- getExampleData()
    str(value$data)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

