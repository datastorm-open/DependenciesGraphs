devtools::install_github("DataKnowledge/DependenciesGraphs")

##Usage

library("ibr")

#### For all package :
##### Prepare data
dep <- envirDependencies("package:ibr")
##### visualization
plot(dep)

#### For one function
##### visualization
dep <- funDependencies("package:ibr","iterchoiceS1")
##### visualization
plot(dep)


####Shiny app
launch.app()

####More exemple 
http://dataknowledge.github.io/Dependencies/