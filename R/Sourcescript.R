

package.load<-function(path)
{
  script<-read.table(path,sep="\n")
  
  row.charg.pck <- as.character(script$V1[c(which(regexpr("library",script$V1)!=-1),which(regexpr("require",script$V1)!=-1))])
  row.charg.pck <- unlist(strsplit(row.charg.pck,";"))
  row.charg.pck <- as.character(row.charg.pck[c(which(regexpr("library",row.charg.pck)!=-1),which(regexpr("require",row.charg.pck)!=-1))])
  row.charg.pck <- unlist(strsplit(row.charg.pck,","))
  row.charg.pck <- as.character(row.charg.pck[c(which(regexpr("library",row.charg.pck)!=-1),which(regexpr("require",row.charg.pck)!=-1))])
  
  row.charg.pck <- unlist(strsplit(row.charg.pck,"[(]"))
  row.charg.pck <- unlist(strsplit(row.charg.pck,"[)]"))
  
  
  row.charg.pck <- gsub(" ","",x=row.charg.pck)
  row.charg.pck <- gsub("\t","",x=row.charg.pck)
  row.charg.pck <- row.charg.pck[-which(row.charg.pck%in%(c("require","library")))]
  row.charg.pck <- unique(row.charg.pck)
  return(row.charg.pck)
}


script.like.fun <- function(path,path.temp)
{
  script<-read.table(path,sep="\n")$V1
  scriptfun<-rep("",length(script)+2)
  scriptfun[1]<-"glob <- function(){"
  scriptfun[2:(length(script)+1)]=as.character(script)
  scriptfun[length(script)+2]="}"
  write.table(as.factor(scriptfun),path.temp,row.names=FALSE,col.names=FALSE,quote=FALSE)
}



#' Dependencies for scripts
#' 
#' @param path : path of R Script
#' 
#' @examples
#' 
#' dep <- data.graph.script("MyRScrpt.R")
#' plot(dep)
#' 
#' @export 
data.graph.script<-function(path)
{
  
  e<-new.env()
  path.temp=paste0(getwd(),"/temp.file.dep.graph.R")
  source(path,local=e)
  script.like.fun(path,path.temp)
  source(path.temp,local=e)
  fcte<-allFunctionEnv(e)
  file.remove(path.temp)
  
  
  ls(e)
  all.pck<-package.load(path)
  dep<-list()
  for(j in 1:length(all.pck))
  {
    
    all.obj<-ls.str(paste0("package:",all.pck[j]),mode="function")
    
    for(i in 1:length(all.obj))
    {
      assign(all.obj[i],get(all.obj[i], envir = as.environment(paste0("package:",all.pck[j]))),envir = e)
    }
    
  }
  
  dep <- funDependencies(e,fcte)
  
  
  
  dep$Nomfun$group<-"YourScript"
  for(j in 1:length(all.pck))
  {
    
    all.obj<-ls.str(paste0("package:",all.pck[j]),mode="function")
    
    all.obj<-as.character(all.obj)
    dep$Nomfun$group[which(dep$Nomfun$label%in%all.obj)]=all.pck[j]
    
  }
  
  dep$Nomfun$group[which(dep$Nomfun$label=="glob")]="script"
  
  rm(e)
  class(dep) <- "dependenciesGraphs"
  return(dep)
  
}






