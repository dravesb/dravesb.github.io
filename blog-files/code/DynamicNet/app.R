library(shinydashboard)
library(shiny)
library(igraph)
source("~/Documents/Work/github/dravesb.github.io/blog-files/code/plot_matrix.R")

ui <- dashboardPage(skin = "purple", 
  dashboardHeader(title = "DynamicNet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Statistics", tabName = "statistics", icon = icon("table")),
      menuItem("Dynamic Visuals", tabName = "dynamic_visuals", icon = icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(
                actionButton("go", "New Sample"),
                box(plotOutput("plot1"))
              )
      ),
      # Second tab content
      tabItem(tabName = "statistics",
              fluidRow(
                box(plotOutput("plot2", height = 250))
              )
      ),
      # Third tab content
      tabItem(tabName = "dynamic_visuals",
              fluidRow(
                box(plotOutput("plot3", height = 250))
              )
      )
    )
  )
)

#set up server stuff
server <- function(input, output) {
  set.seed(2018)
  
  #set up 
  update_dashboard = eventReactive(input$go, 
    {
      #get new sampled matrix
      A = matrix(rbinom(100, 1, 1/2), nrow = 10, ncol = 10); 
      diag(A) = 0; 
      A[lower.tri(A)] = t(A)[lower.tri(A)]; 
    
      #update plot 1 
      output$plot1 <- renderPlot({
        net = graph_from_adjacency_matrix(get_A(), mode = "undirected", weighted = TRUE)
        V(net)$color = c(rep("red", 10))
        V(net)$label = ""
        E(net)$color = "#55555555"
        E(net)$arrow.mode = 0 
        plot(net)
      })
      
      #update plot 2
      output$plot2 <- renderPlot({
        matrix_plot(get_A())
      })
      
      #update plot 3 
      output$plot3 <- renderPlot({
        plot(A)
      })    

      }
    
    )
  
  
}

shinyApp(ui, server)
