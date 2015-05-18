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
                           
                           
                           column(
                             div(
                               selectizeInput('Pack', h3('Package(s)'), choices = installed.packages()[,1], multiple = T),
                               actionButton("GOb", "Launch",icon = icon("line-chart")),align="center"
                             )
                             ,width = 12),
                           
                           fluidRow(
                             column(
                               box(
                                 h3("Depends between package(s)"),
                                 visNetworkOutput("main_plot", width = "100%",height = "600px")
                                 ,width = 12),
                               width=8),
                             
                             column(
                               div(
                                 uiOutput("Groupebutton")
                               ),
                               
                               
                               div(
                                 
                                 h3( textOutput("titledatatabel")),
                                 dataTableOutput("tabledep")
                                 ,align="center"
                               )
                               , 
                               width=4)
                           )
                         )
                         
                         
                ),
                tabPanel("Functions",
                         fluidRow(
                           box(
                             h3(textOutput({"zoomin"})),
                             visNetworkOutput("main_plot1", width = "100%",height = "600px")
                             ,width = 8),
                           column(
                             div(
                               h3(textOutput({"info"})),
                               dataTableOutput("datatable2")
                               ,align="center"
                             ),width = 4
                           )
                         ),
                         
                         fluidRow(
                           box(
                             uiOutput("help"),width = 12
                           )
                         )
                ),
                tabPanel("Script",
     
               
                fluidRow(
                  box(
                    fileInput('file1', 'Choose R File',
                              accept=NULL),
                    visNetworkOutput("plotscript", width = "100%",height = "700px")
                    ,width = 12)
                )
                )
    )
  )
)



