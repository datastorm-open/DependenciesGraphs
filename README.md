# DependenciesGraphs : an R package for dependencies visualization between packages and functions

## Installation

Package is actually only available on github : 

````R
devtools::install_github("datastorm-open/DependenciesGraphs")
````

## Usage

For use directly in R, you must first load package(s) using ````library```` function :

````R
library("ibr")
````

#### All dependencies between functions in an environment

````R
# Prepare data
dep <- envirDependencies("package:ibr")

# visualization
plot(dep)
````
#### All dependencies from a function in an environment

````R
# Prepare data
dep <- funDependencies("package:ibr","iterchoiceS1")

# visualization
plot(dep)
````

#### Explore your R with the shiny app
````R
launch.app()
````

#### More example 
http://datastorm-open.github.io/DependenciesGraphs/
