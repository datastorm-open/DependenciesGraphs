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
#' @param color.table : Name of color to use
#' @return List to graph
#'
#' @export
Pck.load.to.vis<-function(Packages="All",color.table=c("#0B0B3B","#0404B4","#5858FA","#A9A9F5")){
  link<-Pck.load()
  
  if(Packages[1]=="All")
  {
    packages.view<-installed.packages()[,1]
  }else{
    packages.view<-c(Packages,as.character(link[which(link[,1]%in%Packages),2]),as.character(link[which(link[,2]%in%Packages),1]))
  }
  
  visdata<-prepareToVis(link, unique(packages.view))
  
  names(visdata$fromto)[3]<-"title"
  visdata$fromto$title<-paste0("<p>",visdata$fromto$title,"</p>")
  visdata$fromto$color<-as.numeric(as.factor(visdata$fromto$title))
  
  for(i in 1:length(unique(visdata$fromto$color)))
  {
    visdata$fromto$color[which(as.character(visdata$fromto$color)==as.character(i))]<-color.table[i]
  }
  
  class(visdata) <- "dependenciesGraphs"
  return(visdata)
}



#' Given html help for a function
#' 
#' @param package : Names of package to extract help
#' @param func : Name of function to extract help
#' @param tempsave : Tempory file to save help
#'
#' @export
add.html.help <- function(package,func,tempsave=paste0(getwd(),"/temp.html"))
{
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(package), 'help', package))
  topics = names(pkgRdDB)
  rdfunc<-pkgRdDB[[func]]
  tools::Rd2HTML(pkgRdDB[[func]],out =tempsave)
}

