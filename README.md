devtools::install_github("TitouanRobert/DependenciesGraphs")

##Usage

library("ibr")


##For all package :
data<-VisFuns("package:ibr")
visNetwork(data[[1]], data[[2]]) %>% visEdges(style = "arrow") %>% visOptions(highlightNearest=T)

##For one fonction (iterchoiceS1)
data<-VisFun("package:ibr","iterchoiceS1")
visNetwork(data[[1]], data[[2]]) %>% visEdges(style = "arrow") %>% visOptions(highlightNearest=T)
