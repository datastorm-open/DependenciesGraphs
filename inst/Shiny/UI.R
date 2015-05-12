dashboardPage(
  dashboardHeader(title = "DataK depends Graphs"),
  dashboardSidebar( disable = TRUE
                   
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(tags$link(rel='stylesheet', type='text/css', href='style.css')),
    tabsetPanel(id = "Tabsetpan",  title = "Packages exploration", width = "100%",
             # The id lets us use input$tabset1 on the server to find the current tab 
             tabPanel("Packages",
                      fluidRow(
                        box(
                          div(
                          selectizeInput('Pack', h3('Package(s)'), choices = installed.packages()[,1], multiple = T),
                          actionButton("GOb", "Launch",icon = icon("line-chart")),align="center"
                          )
                          ),
                        box(
                          div(
                            h3("Zoom"),
                            actionButton("zoom", "Launch zoom on :",icon = icon("line-chart")),
                            h3(textOutput({"zoomin2"})),align="center"
                          )
                         
                          ),
                        
                        box(
                          h3("Depends between package(s)"),
                          visNetworkOutput("main_plot", width = "100%",height = "600px")
                          ,width = 12),
                        
                        box(
                          div(
                          h3("Depends between package(s)"),
                          dataTableOutput("tabledep")
                          ,align="center"
                          )
                          ,width = 12
                        )  
                      )
                      
                      
             ),
             tabPanel("Functions",
                      fluidRow(
                        box(
                          h3(textOutput({"zoomin"})),
                          visNetworkOutput("main_plot1", width = "100%",height = "600px")
                          ,width = 12),
                        box(
                          div(
                          h3(textOutput({"info"})),
                          dataTableOutput("datatable2")
                          ,align="center"
                          ),width = 12
                        )
                      ),
                      
                      fluidRow(
                        box(
                          uiOutput("help"),width = 12
                        )
                      )
             )
    )
  )
)



