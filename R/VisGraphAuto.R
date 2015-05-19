#' Given a environment, can be a package, returns a vector of included functions in this environment
#' @param envir : Environment, try "search()" to obtain a list of loaded environments
#' @return Names of included functions in this environment.
#'
#' @export
allFunctionEnv <- function(name,envir){
  functions.name <- as.vector(ls.str(name,envir, mode = "function"))
  return(functions.name)
}

#' Given a matrix, returns a dataframe which two columns 'master'and 'slave'
#' @param Mat : matrix of dependencies, should be named (cols and rows), should be square matrix, dependencies are noted 1.
#' @return Dataframe with two columns 'master'and 'slave'
#'
#' @export
mastersSlaves <- function(Mat)
{
  M_s <- apply(Mat, 1, function(ligne){
    names(which(ligne == 1))
  })
  M_s <- data.frame(master = rep(names(M_s), as.vector(unlist(lapply(M_s,length)))),
                    slave = unlist(M_s, use.names = F))
  return(M_s)
}

#' For a function, give all dependencies
#' @param envir : environment where the function should search dependencies.
#' @param name.function : function name (character)
#' @return Dataframe with two columns, 'master'and 'slave'
#' 
#' @importFrom mvbutils foodweb
#' 
#' @export
linksForOne <- function (envir, name.function){
  
  current.warn.option <- options("warn")$warn
  options(warn = -1)
  
  graphfun <- mvbutils::foodweb(where = envir, prune = name.function, descendents = F, plotting = F, ancestors = T)$funmat
  
  while(length(which(rowSums(as.matrix(graphfun)) == 0)) != 1 & length(which(rowSums(as.matrix(graphfun)) == 0)[-which(name.function == names(which(rowSums(as.matrix(graphfun)) == 0)))])>0)
  {
    ligneasupr <- which(rowSums(as.matrix(graphfun)) == 0)[-which(name.function == names(which(rowSums(as.matrix(graphfun)) == 0)))]
    graphfun <- graphfun[-ligneasupr, -ligneasupr]
  }
  
  ancestors <- mastersSlaves(as.matrix(graphfun))
  graphfun <- mvbutils::foodweb(where = envir, prune = name.function, descendents = T, plotting = F, ancestors = F)$funmat
  
  while (length(which(colSums(as.matrix(graphfun)) == 0)) != 1 & length(which(colSums(as.matrix(graphfun)) == 0)[-which(name.function == names(which(colSums(as.matrix(graphfun)) == 0)))])>0)
  {
    ligneasupr <- which(colSums(as.matrix(graphfun)) == 0)[-which(name.function==names(which(colSums(as.matrix(graphfun)) == 0)))]
    graphfun <- graphfun[-ligneasupr,-ligneasupr]
  }
  
  descendents<-mastersSlaves(as.matrix(graphfun))
  
  if(length(ancestors) == 1 || length(ancestors) == 0){ancestors=NULL}
  if(length(descendents) == 1 || length(descendents) == 0){descendents=NULL}
  
  ancdsc <- data.frame(rbind(ancestors,descendents))
  
  options(warn = current.warn.option)
  if(length(ancdsc)!=0)
  {
    colnames(ancdsc)=c("Master","Slaves")
    return(ancdsc)
  }
  else{
    return(NULL)
  }
}



#' For a environnement, give all dependencies
#' @param envir : environment where the function should search dependencies.
#' 
#' @return Dataframe with two columns, 'master'and 'slave'
#' 
#' @importFrom mvbutils foodweb
#' 
#' @export
linksForAll <- function (envir){
  
  current.warn.option <- options("warn")$warn
  options(warn = -1)
  
  graphfun <- mvbutils::foodweb(where = envir, descendents = F, plotting = F, ancestors = T)$funmat
  
  
  
  ancestors <- mastersSlaves(as.matrix(graphfun))
  graphfun <- mvbutils::foodweb(where = envir, descendents = T, plotting = F, ancestors = F)$funmat
  
  
  descendents<-mastersSlaves(as.matrix(graphfun))
  
  if(length(ancestors) == 1 || length(ancestors) == 0){ancestors=NULL}
  if(length(descendents) == 1 || length(descendents) == 0){descendents=NULL}
  
  ancdsc <- data.frame(rbind(ancestors,descendents))
  
  options(warn = current.warn.option)
  if(length(ancdsc)!=0)
  {
    colnames(ancdsc)=c("Master","Slaves")
    return(ancdsc)
  }
  else{
    return(NULL)
  }
}


#' Prepare data for graph visNetwork
#' @param  link : Dataframe, two colums, the first is master, the second is slaves
#' @param functions : All functions to includes in graph (default : union(masters & slaves))
#' @return List contain elements needed for visNetwork visualization
#'
#' @export
prepareToVis <- function(link, functions.list = NULL){
  Visdata <- list()
  if(is.null(functions.list))
  {
    Nomfun <- unique(as.vector(unlist(c( link))))
    Nomfun <- data.frame(cbind(id=1:length(Nomfun),label=Nomfun))
  }else{
    Nomfun <- functions.list
    Nomfun <- data.frame(cbind(id=1:length(Nomfun),label=Nomfun))
  }
  
  func.link<-sort(unique(c(as.character(link[, 1]),as.character(link[, 2]))))
  func.nom<-sort(unique(as.character(Nomfun[, 2])))
  if(!is.null(Nomfun))
  {
    func.prob<-func.link[which(!func.link%in%func.nom)]
    
    if(length(func.prob)>0)
    {
      link <- link[-unique(c(which(link[, 1] %in% func.prob), which(link[, 2] %in% func.prob))), ]
    }
  }
  if(!is.null(link))
  {
  fromto <- matrix(0,ncol=dim(link)[2],nrow=dim( link)[1])
  if(length(fromto)>0)
  {
    for(i in 1:dim(link)[1])
    {
      fromto[i,1] <- which(as.character( link[i,2])==Nomfun[,2])
      fromto[i,2] <- which(as.character( link[i,1])==Nomfun[,2])
      if(dim( link)[2]>2)
      {
        fromto[i,3:length(link[i,])] <- link[i,3:length(link[i,])]
      }
    }
  }
  }else{
    fromto<-cbind(0,0)
  }
  fromto <- data.frame(fromto)
  names(fromto) <- c("from","to")
  Visdata$Nomfun <- Nomfun
  Visdata$fromto <- fromto
  return(Visdata)
}


#' Return all dependencies from a function in an environment
#' @param envir : environment where you want to scherch dependencies
#' @param name.function : Function name (in character)
#' @return List with nodes and edges informations. Needed for visNetwork visualization.
#'
#' @examples
#' 
#' dep <- funDependencies("package:ibr","iterchoiceS1")
#' plot(dep)
#' 
#' @export
funDependencies <- function(envir, name.function)
{
  visdata <- prepareToVis(linksForOne(envir, name.function))
  class(visdata) <- "dependenciesGraphs"
  return(visdata)
}


#' Return all dependencies between functions in an environment
#' @param envir : environment on which you want to search dependencies
#' @return List with nodes and edges informations. Needed for visNetwork visualization.
#' @examples
#' 
#' dep <- envirDependencies("package:ibr")
#' plot(dep)
#' 
#' @export
envirDependencies <- function(envir)
{
  name.functions <- allFunctionEnv(envir)
  if(length(name.functions)>1)
  {
    toutfonc <- linksForAll(envir)
  }else{
    toutfonc<-data.frame(1,1)
  }
  nofunc=FALSE
  if(length(name.functions)==0){
    nofunc=TRUE
    name.functions=NULL}
  
  visdata <- prepareToVis(unique(toutfonc), name.functions)
  if(nofunc)
  {
    visdata$Nomfun$label="No function found"
  }
  class(visdata) <- "dependenciesGraphs"
  return(visdata)
}


#' Return all dependencies between elements of a matrix
#' @param Mat : matrix of dependencies, should be named (cols and rows), should be square matrix, dependencies are noted 1.
#' @return List with nodes and edges informations. Needed for visNetwork visualization.
#'
#' @export
VisFunsmatrice <- function(Mat)
{
  
  data.mat <- mastersSlaves(Mat)
  
  visdata <- prepareToVis(unique(data.mat), colnames(Mat))
  class(visdata) <- "dependenciesGraphs"
  return(visdata)
}

#' Plot network for dependenciesGraphs object
#' 
#' Plot network for dependenciesGraphs object. Using visNetwork package.
#' 
#' Plot network for dependenciesGraphs object. Using visNetwork package.
#' 
#' @param object : dependenciesGraphs object. 
#' 
#' @examples
#' 
#' dep <- funDependencies("package:ibr","iterchoiceS1")
#' plot(dep)
#' 
#' @export 
#' @method plot dependenciesGraphs
plot.dependenciesGraphs <- function(object,block=FALSE){
  visNetwork(object[[1]], object[[2]]) %>% visEdges(style = "arrow") %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, dragNodes = !block)
}

#' Launch shiny app
#' 
#' @param path.install.package : Where you have install package DependenciesGraphs. If null, search in current library
#'  
#' @export   
launch.app <- function(path.install.package=NULL){
  if(is.null(path.install.package))
  {
  path.install.package<-.libPaths()

  
  i<-1
  while(!"DependenciesGraphs"%in%list.files(path.install.package[i]))
  {
    i <- i + 1
  }
  }
  app.path<-paste0(path.install.package[i],"/DependenciesGraphs/Shiny")
  shiny::runApp(app.path,launch.browser = TRUE)
  
}
