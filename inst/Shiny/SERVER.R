optionsDT_fixe <- list(paging = FALSE, searching = FALSE, bInfo = FALSE, search.caseInsensitive = TRUE)


shinyServer(function(input, output, session) {
    observe({
        input$GOPackage
        isolate({
            # print(input$Pack)
            if (length(input$packages) > 0) {
                data <- Pck.load.to.vis(input$packages)
                
                func <- c(input$packages)
                # print(func)
                
                
                nb.func.slave = NULL
                nb.func.master = NULL
                for (i in 1:length(func)) {
                  
                  id.call <- as.numeric(as.character(data$Nomfun$id[which(func[i] == data$Nomfun$label)]))
                  
                  id.call.slave <- as.numeric(as.character(data$fromto$from[which(id.call == data$fromto$to)]))
                  id.call.master <- as.numeric(as.character(data$fromto$from[which(id.call == data$fromto$from)]))
                  
                  nb.call <- length(as.character(data$Nomfun$label[id.call.slave]))
                  nb.func.slave[i] = nb.call
                  
                  nb.call <- length(as.character(data$Nomfun$label[id.call.master]))
                  nb.func.master[i] = nb.call
                  
                }
                
                optionsDT_fixe$drawCallback <- I("function( settings ) {document.getElementById('tabledep').style.width = '400px';}")
                ## Output first graph
                df <- data.frame(Function = func, Import = nb.func.master, `Imported by` = nb.func.slave)
                
                
                
                output$tabledep <- renderDataTable({
                  df
                }, options = optionsDT_fixe)
                # print(data)
                output$main_plot <- renderVisNetwork({
                  plot(data, block = TRUE)
                })
                curentd1 <<- data
                output$titledatatabel <- renderText({
                  "Dependencies between package(s)"
                })
                
            }
        })
    })
    
    
    observe({
        current.package <- input$main_plot_selected
        current.package <- as.character(curentd1$Nomfun[as.numeric(current.package), "label"])
        updateSelectizeInput(session, "package", NULL, choices = installed.packages()[, 1], selected = current.package)
    })
    
    observe({
        input$GOFunc2
        isolate({
            if (input$package != "" && input$GOFunc2 > 0) {
                
                func <- input$package
                # print(func)
                func
                
                if (!func %in% installed.packages()[, 1]) {
                  install.packages(func)
                }
                library(func, character.only = TRUE)
                dep1 <- envirDependencies(paste0("package:", func))
                nb.fun <- length(dep1$Nomfun$label)
                
                
                updateTabsetPanel(session, "Tabsetpan", selected = "Functions")
                optionsDT_fixe$drawCallback <- I("function( settings ) {document.getElementById('datatable2').style.width = '100px';}")
                output$datatable2 <- renderDataTable(data.frame(Number.of.functions = nb.fun), options = optionsDT_fixe)
                
                output$zoomin <- renderText(paste("Zoom on package ", func))
                output$info <- renderText(paste("Information on package ", func))
                curentd3 <<- func
                
                output$main_plot1 <- renderVisNetwork({
                  plot(dep1, block = TRUE)
                })
                curentd2 <<- dep1
            }
        })
    })
    
    observe({
        input$GOFunc1
        isolate({
            if (!is.null(input$main_plot_selected) && input$main_plot_selected != "" && input$GOFunc1 > 0) {
                
                func <- as.character(curentd1$Nomfun$label[input$main_plot_selected == curentd1$Nomfun$id])
                # print(func)
                func
                
                if (!func %in% installed.packages()[, 1]) {
                  install.packages(func)
                }
                library(func, character.only = TRUE)
                dep1 <- envirDependencies(paste0("package:", func))
                nb.fun <- length(dep1$Nomfun$label)
                
                
                updateTabsetPanel(session, "Tabsetpan", selected = "Functions")
                optionsDT_fixe$drawCallback <- I("function( settings ) {document.getElementById('datatable2').style.width = '100px';}")
                output$datatable2 <- renderDataTable(data.frame(Number.of.functions = nb.fun), options = optionsDT_fixe)
                
                output$zoomin <- renderText(paste("Zoom on package : ", func))
                output$info <- renderText(paste("Information on : ", func))
                curentd3 <<- func
                
                output$main_plot1 <- renderVisNetwork({
                  plot(dep1, block = TRUE)
                })
                curentd2 <<- dep1
            }
        })
    })
    
    ### chossefunction
    
    observe({
        input$chargedf
        isolate({
            input$packageslist
            sapply(input$packageslist, function(x) {
                library(x, character.only = TRUE)
            })
            allFun <- unique(unlist(sapply(input$packageslist, function(x) {
                allFunctionEnv(paste0("package:", x))
            })))
            
            updateSelectizeInput(session, inputId = "functionlist", choices = allFun)
        })
    })
    
    output$chossefunctionplot <- renderVisNetwork({
        input$makegraph
        
        isolate({
            if (input$makegraph >= 1) {
                
                plot(allDepFunction(input$packageslist, unlist(strsplit(input$functionlist, split = ";"))), block = TRUE)
            }
        })
        
    })
    
    observe({
        
        if (!is.null(input$main_plot1_selected) && input$main_plot1_selected != "") {
            isolate({
                pck <- curentd3
                
                # print(pck)
                
                func <- as.character(curentd2$Nomfun$label[input$main_plot1_selected == curentd2$Nomfun$id])
                # print(func)
                try(add.html.help(pck, func), TRUE)
                
                if (length(htmlTreeParse(paste0(getwd(), "/temp.html"))$children$html) > 0) {
                  output$help <- renderUI(includeHTML(paste0(getwd(), "/temp.html")))
                  
                } else {
                  output$help <- renderUI("Not available help for this function")
                }
            })
        } else {
            
            output$help <- renderUI("Select a function")
        }
        
    })
    
    observe({
        
        if (!is.null(input$main_plot_selected) && input$main_plot_selected != "") {
            
            func <- as.character(curentd1$Nomfun$label[input$main_plot_selected == curentd1$Nomfun$id])
            
            output$Groupebutton <- renderUI({
                
                div(hr(), actionButton("GOFunc1", paste0("Launch zoom on : ", func), icon = icon("line-chart")), align = "center")
                
            })
        } else {
            output$Groupebutton <- renderUI({
                NULL
            })
        }
        
    })
    
    
    
    observe({
        
        input$GObott
        # input$file1 will be NULL initially. After the user selects and uploads a file, it will be a data frame with 'name', 'size',
        # 'type', and 'datapath' columns. The 'datapath' column will contain the local filenames where the data can be found.
        
        inFile <- input$file1
        
        if (!is.null(inFile)) {
            dep <- data.graph.script(inFile$datapath)
            output$plotscript <- renderVisNetwork({
                plot(dep, block = TRUE)
            })
        }
    })
})
