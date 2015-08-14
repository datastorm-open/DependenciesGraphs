#' For a function, give all dependencies
#' @param envir : environments where the function should search dependencies.
#' @param name.function : function name (character)
#' 
#' @return object of class dependenciesGraphs
#' @importFrom mvbutils foodweb
#' 
#' @export
allDepFunction <- function(envir, name.functions) {
    envir <- paste0("package:", envir)
    toutfonc <- linksForOne(envir, name.functions)
    
    link <- toutfonc
    functions.list <- unique(as.character(unlist(c(toutfonc))))
    
    Visdata <- list()
    
    Nomfun <- functions.list
    Nomfun <- data.frame(cbind(id = 1:length(Nomfun), label = Nomfun))
    
    
    func.link <- sort(unique(c(as.character(link[, 1]), as.character(link[, 2]))))
    func.nom <- sort(unique(as.character(Nomfun[, 2])))
    
    if (!is.null(link)) {
        fromto <- matrix(0, ncol = dim(link)[2], nrow = dim(link)[1])
        if (length(fromto) > 0) {
            for (i in 1:dim(link)[1]) {
                fromto[i, 1] <- which(as.character(link[i, 2]) == Nomfun[, 2])
                fromto[i, 2] <- which(as.character(link[i, 1]) == Nomfun[, 2])
                if (dim(link)[2] > 2) {
                  fromto[i, 3:length(link[i, ])] <- link[i, 3:length(link[i, ])]
                }
            }
        }
    } else {
        fromto <- cbind(0, 0)
    }
    fromto <- data.frame(fromto)
    names(fromto) <- c("from", "to")
    Visdata$Nomfun <- Nomfun
    Visdata$fromto <- fromto
    class(Visdata) <- "dependenciesGraphs"
    return(Visdata)
}




 
