cleanExtract<-function(x){
  temp<-gsub(" ","",unlist(strsplit(x,"\\(")))
  supr<-grep(")",temp)
  if(length(supr)>0)
  {
    temp<-temp[-supr]
  }
  temp<-gsub("\n","",temp)
  temp<-gsub("^R$","",temp)
  if(length(-which(nchar(temp)==0))>0)
  {
    temp<-temp[-which(nchar(temp)==0)]
  }
  if(length(temp)==0 | is.na(temp)[1]){temp=NULL}
  temp
}

CleanList<-function(Tree.fun,extractName)
{
  Tree.fun<-rlist::list.clean(Tree.fun)
  
  current.warn.option <- options("warn")$warn
  options(warn = -1)
  
  Tree.fun<-rlist::list.rbind(Tree.fun)
  
  row.nam<-rownames(Tree.fun)
  
  
  Tree.fun<-cbind(as.vector(Tree.fun),row.nam)
  
  options(warn = current.warn.option)
  
  Tree.fun<-unique(Tree.fun)
  
  Tree.fun<-data.frame(Tree.fun)
  names(Tree.fun)<-c("from","to")
  Tree.fun$group<-extractName
  colnames(installed.packages())
  Tree.fun
  
}

extraxtToList<-function(extractName)
{
  x<-installed.packages()[,extractName]
  x<-strsplit(x,",")
  Tree.fun<-lapply(x,cleanExtract)
  CleanList(Tree.fun,extractName)
}

#' Given dependencies between all install packages
#' 
#' @return List of dependencies
#'
#' @export
Pck.load<-function(){
  imp <- extraxtToList("Imports")
  sug <- extraxtToList("Suggests")
  lin <- extraxtToList("LinkingTo")
  dep <- extraxtToList("Depends")
  Tree.fun<-rbind(imp,sug,lin,dep)
  return(Tree.fun)
}


#' Given dependencies between all install packages
#' 
#' @param Packages : Names of packages to includes to extract
#' 
#' @return List to graph
#'
#' @export
Pck.load.to.vis<-function(Packages="All"){
  link<-Pck.load()
  
  if(Packages[1]=="All")
  {
    packages.view<-installed.packages()[,1]
  }else{
    packages.view<-c(Packages,as.character(link[which(link[,1]%in%Packages),2]),as.character(link[which(link[,2]%in%Packages),1]))
  }
  
  visdata<-prepareToVis(link, unique(packages.view))
  class(visdata) <- "dependenciesGraph"
  return(visdata)
}
