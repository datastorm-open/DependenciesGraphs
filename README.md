devtools::install_github("TitouanRobert/DependenciesGraphs")

##Usage

library("ibr")


##For all package :
####Prepare data
data<-VisFuns("package:ibr")
####Graph
visNetwork(data[[1]], data[[2]]) %>% visEdges(style = "arrow")

##For one fonction (iterchoiceS1)
####Prepare data
data<-VisFun("package:ibr","iterchoiceS1")
####Graph
visNetwork(data[[1]], data[[2]]) %>% visEdges(style = "arrow")
