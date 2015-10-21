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
                           column(3, div(h3('Package(s) selection :'), align = "center")),
                           column(6, br(), selectInput('packages', NULL, choices = installed.packages()[,1], multiple = T, width = "100%")),
                           column(3, br(), div(actionButton("GOPackage", "Launch",icon = icon("line-chart")), align = "center"))
                         ),
                         hr(),
                         
                         fluidRow(
                           box(
                             solidHeader = TRUE, collapsible = TRUE, title = "Dependencies between package(s)",
                             status = "primary",
                             visNetworkOutput("main_plot", width = "100%",height = "750px"),
                             br()
                             ,width = 12
                           ),
                           box(
                             solidHeader = TRUE, collapsible = TRUE, title = "Informations",
                             status = "primary",
                             div(
                               dataTableOutput("tabledep"),
                               uiOutput("Groupebutton"),
                               align="center"
                             ), 
                             width=12)
                         )
                         
                ),
                tabPanel("Functions",
                         
                         fluidRow(
                           column(4, div(h3('Package :'), align = "center")),
                           column(4, br(), selectInput('package', NULL, choices = installed.packages()[,1], multiple = FALSE, width = "100%")),
                           column(4, br(), div(actionButton("GOFunc2", "Launch",icon = icon("line-chart")), align = "center"))
                         ),
                         hr(),
                         
                         fluidRow(
                           box(
                             solidHeader = TRUE, collapsible = TRUE, title = "Dependencies between functions",
                             status = "primary",
                             div(h4(textOutput("zoomin")), align = "center"),
                             visNetworkOutput("main_plot1", width = "100%",height = "750px"),
                             br()
                             ,width = 12
                           ),
                           box(
                             solidHeader = TRUE, collapsible = TRUE, title = "Informations",
                             status = "primary",
                             div(
                               # h4(textOutput("info")),
                               dataTableOutput("datatable2")
                               ,align="center"
                             ),
                             width=12)
                         ),
                         
                         fluidRow(
                           box(
                             uiOutput("help"),width = 12
                           )
                         )
                ),
                #                 tabPanel("Script",
                #                          
                #                          
                #                          fluidRow(
                #                            box(
                #                              fileInput('file1', 'Choose R File',
                #                                        accept=NULL),
                #                              visNetworkOutput("plotscript", width = "100%",height = "700px")
                #                              ,width = 12)
                #                          )
                #                 ),
                
                tabPanel("Custom",
                         
                         
                         fluidRow(
                           box(
                             fluidRow(
                               column(width=4,
                                      selectizeInput(inputId = "packageslist" , "Package(s) :", choices = installed.packages()[,1], multiple = TRUE)
                               ),
                               column(width=2, 
                                      br(), div(actionButton("chargedf", "Find functions", style = "padding: 8px 20px 8px 20px;"),align="center")
                               ),
                               column(width=4,
                                      selectizeInput(inputId = "functionlist" , "Function(s) :", choices = NULL, multiple = TRUE)
                               ),
                               column(width=2, 
                                      br(), div(actionButton("makegraph", "Make graph", style = "padding: 8px 20px 8px 20px;"),align = "center")
                               )
                             ),
                             
                             hr(),
                             visNetworkOutput("chossefunctionplot", width = "100%",height = "750px"),
                             br(),
                             width = 12)
                         )
                )
                
                
    )
  )
)



