require(visNetwork)
require(mvbutils)


#' Given a environment, can be a package, returns a vector of fonctions includes in this environment
#' @param Envi : Environment, try fonction "search()" to obtain a list of charged environments
#' @return nomfun : Names of fonctions includes in this environment.
#'
#'
AllFunctionEnv<-function(Envi){
nomfun<-as.vector(ls.str(Envi,mode="function"))
return(nomfun)
}

#' Given a matrix, returns a dataframe which two colums, first : master, second : slave
#' @param Mat : matrix of dependencies, should be named (cols and rows), should be square matrix, dependencies are noted 1.
#' @return M_s : Dataframe which two colums, first : master, second : slave
#' @usage
#' \code{require("mvbutils")}
#' \code{graphfun<-foodweb( where="package:mvbutils",prune="foodweb",descendents=T,plotting =F,ancestors=F)$funmat}
#' \code{MastersSlaves(graphfun)}
#'
MastersSlaves <- function(Mat)
{
  M_s <- apply(Mat,1, function(ligne){
    names(which(ligne == 1))
  })
  M_s<-data.frame(master=rep(names(M_s),as.vector(unlist(lapply(M_s,length)))),slave=unlist(M_s,use.names = F))
  return(M_s)
}

#' For one fonction, give all dependencies
#' @param Enviro : environment where the function should search dependencies.
#' @param Nomfonciton : function name (character)
#' @return ancdsc : Dataframe which two colums, first : master, second : slave
#' @usage
#' \code{require("mvbutils")}
#' \code{LinksForOne(Enviro="package:mvbutils",Nomfonciton="foodweb")}
#'
LinksForOne <- function (Enviro,Nomfonciton){

  graphfun<-mvbutils::foodweb( where=Enviro,prune=Nomfonciton,descendents=F,plotting =F,ancestors=T)$funmat

  while (length(which(rowSums(as.matrix(graphfun))==0))!=1)
     {
       ligneasupr<-which(rowSums(graphfun)==0)[-which(Nomfonciton==names(which(rowSums(graphfun)==0)))]
       graphfun<-graphfun[-ligneasupr,-ligneasupr]
     }

  ancestors<-MastersSlaves(as.matrix(graphfun))
  graphfun<-mvbutils::foodweb( where=Enviro,prune=Nomfonciton,descendents=T,plotting =F,ancestors=F)$funmat

      while (length(which(colSums(as.matrix(graphfun))==0))!=1)
      {
      ligneasupr<-which(colSums(graphfun)==0)[-which(Nomfonciton==names(which(colSums(graphfun)==0)))]
      graphfun<-graphfun[-ligneasupr,-ligneasupr]
    }


  descendents<-MastersSlaves(as.matrix(graphfun))

  if(length(ancestors)==1 || length(ancestors)==0)
  { ancestors=NULL}
  if(length(descendents)==1 || length(descendents)==0)
  { descendents=NULL}

  ancdsc<-data.frame(rbind(ancestors,descendents))

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
#' @param Liens : Dataframe, two colums, the first is master, the second is slaves
#' @param Nomfonctions : All fonctions to includes in graph (default : union(masters & slaves))
#' @return visdata : List contain elements tu do a visNetwork graph
#'
PrepareToVis <- function(liens,Nomfonctions=NULL){
  Visdata<-list()
  if(is.null(Nomfonctions))
  {
  Nomfun<-unique(as.vector(unlist(c(liens))))
  Nomfun<-data.frame(cbind(id=1:length(Nomfun),label=Nomfun))
  }else{
    Nomfun<-Nomfonctions
    Nomfun<-data.frame(cbind(id=1:length(Nomfun),label=Nomfun))
  }
  fromto<-matrix(0,ncol=2,nrow=dim(liens)[1])

  for(i in 1:dim(liens)[1])
  {
    fromto[i,1]<-which(as.character(liens[i,2])==Nomfun[,2])
    fromto[i,2]<-which(as.character(liens[i,1])==Nomfun[,2])
  }
  fromto<-data.frame(fromto)
  names(fromto)<-c("from","to")
  Visdata$Nomfun<-Nomfun
  Visdata$fromto<-fromto
  return(Visdata)
}


#' Return all dependencies between a function and others functions in an envorinnement
#' @param Enviro : envorinnement where you want to scherch dependencies
#' @param Func : Function name (in character)
#' @return visdata : List contain elements tu do a visNetwork graph
#'
VisFun<- function(Enviro,Func)
{
  visdata<-PrepareToVis(LinksForOne(Enviro,Func))
  return(visdata)
}


#' Return all dependencies between fonctions in an envorinnement
#' @param Enviro : envorinnement where you want to scherch dependencies
#' @return visdata : List contain elements to do a visNetwork graph
#'
VisFuns<- function(Enviro)
{
  Nomfonctions<-AllFunctionEnv(Enviro)
  toutfonc<-NULL
  for(ww in 1:length(Nomfonctions))
  {
    print(ww)
    toutfonc<-rbind(toutfonc,LinksForOne(Enviro,Nomfonctions[ww]))
  }
  visdata<-PrepareToVis(unique(toutfonc),Nomfonctions)
  return(visdata)
}


#' Return all dependencies between elements of a matrix
#' @param Mat : matrix of dependencies, should be named (cols and rows), should be square matrix, dependencies are noted 1.
#' @return visdata : List contain elements to do a visNetwork graph
#'
VisFunsmatrice<- function(Mat)
{
  Nomfonctions<-AllFunctionEnv(Enviro)
  toutfonc<-MastersSlaves(Mat)

  visdata<-PrepareToVis(unique(toutfonc),colnames(Mat))
  return(visdata)
}
