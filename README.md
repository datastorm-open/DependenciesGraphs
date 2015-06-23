# DependenciesGraphs : an R package for dependencies visualization between packages and functions

##Installation

Package is actually only available on github : 

````
devtools::install_github("DataKnowledge/DependenciesGraphs")
````

##Usage

For use directly in R, you must first load package(s) using ````library```` function :

````
library("ibr")
````

#### All dependencies between functions in an environment

````
# Prepare data
dep <- envirDependencies("package:ibr")

# visualization
plot(dep)
````
#### All dependencies from a function in an environment

````
# Prepare data
dep <- funDependencies("package:ibr","iterchoiceS1")

# visualization
plot(dep)
````

####Explore your R with the shiny app
````
launch.app()
````

####More exemple 
http://dataknowledge.github.io/DependenciesGraphs/