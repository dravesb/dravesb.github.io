library(shinydashboard)
library(shiny)
library(igraph)
source("~/Documents/Work/github/dravesb.github.io/blog-files/code/plot_matrix.R")

#-------------------------------------------------------------------
#
#                       User Interface
#
#-------------------------------------------------------------------

ui <- dashboardPage(skin = "purple", 
  dashboardHeader(title = "StaticNet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Visuals", tabName = "visuals", icon = icon("line-chart")),
      menuItem("Statistics", tabName = "statistics", icon = icon("table")),
      menuItem("Modeling", tabName = "models", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    tabItems(
      #Home tab content --------------------------------------
       tabItem(tabName = "home",
            fluidRow( 
                     box(title = "Upload Your Network",
                       fileInput("file", "Choose CSV File", 
                               multiple = FALSE, 
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")
                               ),
                     
                       # Input: Checkbox if file has header ----
                       checkboxInput("header", "Header", FALSE),
                     
                       # Input: Select separator ----
                       radioButtons("sep", "Separator",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ","),
                     
                       # Input: Select quotes ----
                       radioButtons("quote", "Quote",
                                  choices = c(None = "",
                                              "Double Quote" = '"',
                                              "Single Quote" = "'"),
                                  selected = '')
                        )
                    )
      ),
      
      #Visuals tab content--------------------------------------
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Network Plot", plotOutput("plot1")),
                box(title = "Adjacency Matrix Plot", plotOutput("plot2"))
              )
      ),
      #Statistics tab content--------------------------------------
      tabItem(tabName = "statistics",
              fluidRow(
                infoBoxOutput("NV"),
                infoBoxOutput("NE"),
                infoBoxOutput("density")
              ), 
              fluidRow(
                box(plotOutput("plot3")),
                box(plotOutput("plot4"))
              )
      ),
      #Modeling tab content
      tabItem(tabName = "models",
              fluidRow(
                dataTableOutput("table")
                       )
              
      )
    )
  )
)
#-------------------------------------------------------------------
#
#                       Server
#
#-------------------------------------------------------------------
#set up server stuff
server <- function(input, output) {
  
  data <- eventReactive(input$file,{
    as.matrix(read.csv(input$file$datapath, 
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote))
  })
  
   
  #visuals tab ------------------------------------------------------------------
  #update plot 1 
  output$plot1 <- renderPlot({
        net = graph_from_adjacency_matrix(data(), mode = "undirected", weighted = TRUE)
        V(net)$color = c(rep("purple", n))
        V(net)$label = ""
        E(net)$color = "#55555555"
        E(net)$arrow.mode = 0 
        plot(net)
      })
      
  #update plot 2
  output$plot2 <- renderPlot({
    plot_matrix(data(), col = "purple", main = "")
  })
  

  #statistics tab ------------------------------------------------------------------
  output$NV <- renderInfoBox({
    infoBox("# Vertices", nrow(data()), color = "red", fill = TRUE)
  })
  
  output$NE <- renderInfoBox({
    infoBox("# Edges", sum(data()[upper.tri(data())]), color = "red", fill = TRUE)
  })
  
  output$density <- renderInfoBox({
    infoBox("Density", paste0(100*round(sum(data()[upper.tri(data())])/(nrow(data())*(nrow(data())-1)/2), 2), "%"), color = "red", fill = TRUE)
  })
  
  #make centrality plot(s)
  shadeCol = function(x){adjustcolor("purple", alpha.f = x)}
  output$plot3 <- renderPlot({
    net = graph_from_adjacency_matrix(data(), mode = "undirected", weighted = TRUE)
    shade = eigen_centrality(net)$vector
    V(net)$color = sapply(shade/max(shade), shadeCol)
    V(net)$label = ""
    E(net)$color = "#55555555"
    E(net)$arrow.mode = 0 
    plot(net, main = "Eigen-Centrality Plot")
  })
  
  output$plot4 <- renderPlot({
    net = graph_from_adjacency_matrix(data(), mode = "undirected", weighted = TRUE)
    shade = closeness(net)
    V(net)$color = sapply(shade/max(shade), shadeCol)
    V(net)$label = ""
    E(net)$color = "#55555555"
    E(net)$arrow.mode = 0 
    plot(net, main = "Closeness-Centrality Plot")
  })
  
  #dynamic visuals tab ------------------------------------------------------------------
  #update plot 4 
  output$table <- renderDataTable({data()})  
    
    
  
}

shinyApp(ui, server)
