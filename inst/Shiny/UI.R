dashboardPage(
  dashboardHeader(title = "DependenciesGraphs"),
  dashboardSidebar( disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tags$head(tags$link(rel='stylesheet', type='text/css', href='style.css')),
    
    tabsetPanel(id = "Tabsetpan",  title = "Packages exploration", width = "100%",
                # The id lets us use input$tabset1 on the server to find the current tab 
                tabPanel("Packages",
                         fluidRow(
                           column(4, div(h3('Package(s) :'), align = "center")),
                           column(4, br(), selectizeInput('packages', NULL, choices = installed.packages()[,1], multiple = T)),
                           column(4, br(), div(actionButton("GOPackage", "Launch",icon = icon("line-chart")), align = "center"))
                         ),
                         hr(),
                         
                         fluidRow(
                           column(
                             box(
                               h3("Dependencies between package(s)"),
                               visNetworkOutput("main_plot", width = "100%",height = "600px")
                               ,width = 12
                             ),
                             width=8),
                           
                           column(
                             div(
                               h3(textOutput("titledatatabel")),
                               dataTableOutput("tabledep"),
                               uiOutput("Groupebutton"),
                               align="center"
                             ), 
                             width=4)
                         )
                ),
                tabPanel("Functions",
                         
                         fluidRow(
                           column(4, div(h3('Package :'), align = "center")),
                           column(4, br(), selectizeInput('package', NULL, choices = installed.packages()[,1], multiple = FALSE)),
                           column(4, br(), div(actionButton("GOFunc2", "Launch",icon = icon("line-chart")), align = "center"))
                         ),
                         hr(),
                         
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



