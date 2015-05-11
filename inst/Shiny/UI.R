dashboardPage(
  dashboardHeader(title = "DataK depends Graphs"),
  dashboardSidebar(
    selectizeInput('Pack', 'Package(s)', choices = installed.packages()[,1], multiple = T),
    actionButton("GOb", "Launch"),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    actionButton("zoom", "Launch zoom on :"),
    h3(textOutput({"zoomin2"}))
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        h3("Depends between package(s)"),
        visNetworkOutput("main_plot", width = "100%")
      ),
      
      box(
        h3("Depends between package(s)"),
        dataTableOutput("tabledep")
      )  
      
      
      
    ),
    fluidRow(
      box(
        h3(textOutput({"zoomin"})),
        visNetworkOutput("main_plot1", width = "100%")
      ),
      box(
        h3(textOutput({"info"})),
        dataTableOutput("datatable2")
        
      )
    ),
    
    fluidRow(
      box(
       uiOutput("help"),width = 12
      )
    )
  )
)




