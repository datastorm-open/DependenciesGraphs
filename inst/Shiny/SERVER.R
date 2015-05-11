optionsDT_fixe <- list(paging=F, searching=F, Info=F, search.caseInsensitive=T)


shinyServer(function(input, output,session) {
  
  
  
  observe({
    input$GOb
    isolate({
      print(input$Pack)
      if(length(input$Pack)>0)
      {
        data<-Pck.load.to.vis(input$Pack)
        names(data$fromto)[3]<-"title"
        data$fromto$title<-paste0("<p>",data$fromto$title,"</p>")
        func<-c(input$Pack)
        print(func)
        nb.func.master=NULL
        for(i in 1:length(func))
        {
          id.call.master<-as.numeric(as.character(data$Nomfun$id[which(func[i]==data$Nomfun$label)]))
          
          id.call.slave<-as.numeric(as.character(data$fromto$to[which(id.call.master==data$fromto$from)]))
          
          nb.call<-length(as.character(data$Nomfun$label[id.call.slave]))
          nb.func.master[i]=nb.call
        }
        
        nb.func.slave=NULL
        for(i in 1:length(func))
        {
          
          id.call.master<-as.numeric(as.character(data$Nomfun$id[which(func[i]==data$Nomfun$label)]))
          
          id.call.slave<-as.numeric(as.character(data$fromto$from[which(id.call.master==data$fromto$to)]))
          
          nb.call<-length(as.character(data$Nomfun$label[id.call.slave]))
          nb.func.slave[i]=nb.call
        }
        

        ##Output first graph
        output$tabledep <- renderDataTable({data.frame(Function=func,"Import"=nb.func.master,"Imported by"=nb.func.slave)
        },options=optionsDT_fixe)
        output$main_plot <- renderVisNetwork({plot(data,block=TRUE)})
        curentd1<<-data
      }
    })
  })
  
  
  
  
  observe({
    input$zoom
    isolate({
      if(!is.null(input$main_plot_selected) && input$main_plot_selected!="")
      {
        
        func<-as.character(curentd1$Nomfun$label[input$main_plot_selected==curentd1$Nomfun$id])
        print(func)
        func
        
        if(!func%in%installed.packages()[,1])
        {
          install.packages(func)
        }
        library(func,character.only = TRUE)
        dep1 <- envirDependencies(paste0("package:",func))
        nb.fun<-length(dep1$Nomfun$label)
        
        output$datatable2<-renderDataTable(data.frame(Nb=nb.fun),options=optionsDT_fixe)
        
        output$zoomin<-renderText(paste("Zoom on package : ",func))
        output$info<-renderText(paste("Information on : ",func))
        
        
        output$main_plot1<-renderVisNetwork({plot(dep1)})
        curentd2<<-dep1
      }
    })
  })
  
  
  
  
  observe({
    input$zoom
    
    func=NULL
    if(length(input$Pack)>0 & length(input$main_plot_selected)>0)
    {
      
      func<-as.character(curentd1$Nomfun$label[input$main_plot_selected==curentd1$Nomfun$id])
      output$zoomin2<-renderText(paste(func))
      
    }
    
  })
  
  
  observe({
    
    if(!is.null(input$main_plot1_selected) && input$main_plot1_selected!="")
    {
      isolate({
      pck=NULL
      print(input$Pack)
      if(length(input$Pack)>0 & length(input$main_plot_selected)>0)
      {
      
        pck<-as.character(curentd1$Nomfun$label[input$main_plot_selected==curentd1$Nomfun$id])
      }
      
      
      
      func<-as.character(curentd2$Nomfun$label[input$main_plot1_selected==curentd2$Nomfun$id])
      try(add.html.help(pck,func),TRUE)
      
      output$help<-renderUI(
        includeHTML(paste0(getwd(),"/temp.html"))
      )
      })
    }
    
  })
  
  
  
  
})


