library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

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
                    downloadButton(outputId = "downloadExample", label = "Download example"),
                    uiOutput(outputId = "videoLink")
                    )
              ),
              fluidRow(
                column(width = 9,
                       box(status = "warning", height = 800, width = NULL,
                           verbatimTextOutput(outputId = "warning", placeholder = F),
                           plotlyOutput("dotplot", height = "750px"))
                       ),
                column(width = 3,
                       box(status = "success", height = 325, width = NULL,
                           selectInput(inputId = "xLabel", label = "Select X", choices = ""),
                           selectInput(inputId = "yLabel", label = "Select Y", choices = ""),
                           selectInput(inputId = "colorLabel", label = "Select Color", choices = ""),
                           selectInput(inputId = "sizeLabel", label = "Select Size", choices = "")),
                       box(status = "primary", height = 475, width = NULL,
                           selectizeInput(inputId = "colorPalette", label = "Select Color Palette",
                                          choices = list(
                                            Diverging = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                                                          "RdYlBu", "RdYlGn", "Spectral"),
                                            Qualitative = c("Accent", "Dark2", "Paired", "Pastel1", 
                                                            "Pastel2", "Set1", "Set2", "Set3"),
                                            Sequential = c("Blues", "BuGn", "BuPu", "GnBu", "Greens",
                                                           "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd",
                                                           "Purples", "RdPu", "Reds", "YlGn", "YlGnBu",
                                                           "YlOrBr", "YlOrRd")),
                                          selected = "Blues"),
                           checkboxInput(inputId = "colorInvert", label = "Invert Color Scale"),
                           textInput(inputId = "title", label = "Graph Title"), 
                           textInput(inputId = "xlab", label = "X label"),
                           textInput(inputId = "ylab", label = "Y label"),
                           textInput(inputId = "colorlab", label = "Color scale label"))
                       )
                
              )
      )
    )
  )
)

  
  
  
server <- function(input, output, session) {
  
  #make link to youtube video
  output$videoLink <- renderUI({
    url <- a("tutorial", href= "https://youtu.be/mVt5gOZfsN4")
    tagList("For more information please see the ", url)})
  
  #initialize reactive variable to store data
  value <- reactiveValues()
  
  #input data from user file
  observeEvent(input$file, {
    value$data <- read.csv(input$file$datapath)
  })
  
  #input example data from github
  observeEvent(input$runExample, {
    value$data <- read.csv("https://raw.githubusercontent.com/bicbioeng/bioviewer/master/example.csv", 
                           header=T, fileEncoding="UTF-8-BOM")
  })
  
  #download example data from github
  output$downloadExample <- downloadHandler(filename = "example.csv", content = function(file){
    write.csv(read.csv("https://raw.githubusercontent.com/bicbioeng/bioviewer/master/example.csv", 
                           header=T, fileEncoding="UTF-8-BOM"), file, row.names = F)})
  
  #warning if data cannot be coerced to data frame
  output$warning <- renderPrint({
    if(class(try(as.data.frame(value$data))) != "data.frame"){
      value$warning = T
      print("Please try reorganizing data")
    } 
  })
  
  #update select buttons with values from data
  observe({
    x <- c(NaN, colnames(value$data))
    n <- length(x)
    
    if (is.null(x))
      x <- character(0)
    
    updateSelectInput(session, "xLabel",
                      choices = x[-1],
                      selected = x[2]
    )
    updateSelectInput(session, "yLabel",
                      choices = x[-1],
                      selected = x[3]
    )
    updateSelectInput(session, "colorLabel",
                      choices = x,
                      selected = NULL
    )
    updateSelectInput(session, "sizeLabel",
                      choices = x,
                      selected = NULL
    )
  })
  
  #draw dotplot
  output$dotplot <- renderPlotly({
    
    #size has to have a value - so set to 1 when no value selected
    if(input$sizeLabel == "NaN" || input$sizeLabel == ""){s <- 1
    } else {s <- as.data.frame(value$data)[,input$sizeLabel]}
    
  
    #if there is a dataset, plot
    if(!is.null(value$data)){
      
      #if the color is factor, plot discretely (up to 7), otherwise plot color scale continuously
      if(!is.nan(input$colorLabel) && 
         try(class(as.data.frame(value$data)[,input$colorLabel]), silent = T) == "factor"){
        c <- scale_color_brewer(palette = input$colorPalette, 
                                direction = ifelse(input$colorInvert, -1, 1))
      } else {
        c <- scale_color_distiller(palette = input$colorPalette,
                                   direction = ifelse(input$colorInvert, -1, 1))
      }
      
      
      plot <- ggplot(as.data.frame(value$data), aes_string(x = input$xLabel, y = input$yLabel, 
                                                           color = input$colorLabel)) +
        c +
        geom_point(aes(size = s)) +
        labs(title = input$title, x = input$xlab, y = input$ylab, color = input$colorlab) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.15)) 
      ggplotly(plot)
    }
    
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

