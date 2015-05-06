library(rlist)

essai<-Pck.load.to.vis(c("rlist","car"))
names(essai$fromto)[3]<-"group"
plot(essai,block=TRUE)


require(ibr)
dep <- envirDependencies("package:ibr")
plot(dep)


nodes <- data.frame(id = 1:5)
edges <- data.frame(from = c(2,5,3), to = c(1,2,4))



visNetwork(nodes, edges, width = "100%")%>%visOptions( dragNodes = FALSE) 



visry(nodes, edges, width = "100%")

visry<-function (nodes = NULL, edges = NULL, dot = NULL, gephi = NULL, 
                 legend = FALSE, legend.width = 1, width = NULL, height = NULL) 
{
  if (is.null(nodes) & is.null(edges) & is.null(dot) & is.null(gephi)) {
    stop("Must 'dot' data, or 'gephi' data, or 'nodes' and 'edges' data.")
  }
  if (!is.null(dot)) {
    x <- list(dot = dot, options = list(width = "100%", height = "100%", 
                                        nodes = list(shape = "dot")), groups = NULL, legend = legend, 
              legendWidth = legend.width, width = width, height = height)
  }
  else if (!is.null(gephi)) {
    x <- list(gephi = rjson::fromJSON(file = gephi), options = list(width = "100%", 
                                                                    height = "100%", nodes = list(shape = "dot")), groups = NULL, 
              legend = legend, legendWidth = legend.width, width = width, 
              height = height)
  }
  else {
    groups = as.character(unique(nodes$group))
    if (length(groups) == 0) {
      groups = NULL
    }
    x <- list(nodes = nodes, edges = edges, options = list(width = "100%", 
                                                           height = "100%", nodes = list(shape = "dot")), groups = groups, 
              legend = legend, legendWidth = legend.width, width = width, 
              height = height)
  }
  htmlwidgets::createWidget(name = "visNetwork", x, width = width, 
                            height = height, package = "visNetwork")
}





nb <- 10
nodes <- data.frame(id = 1:nb, label = paste("Label", 1:nb),
                    group = sample(LETTERS[1:3], nb, replace = TRUE), value = 1:nb,
                    title = paste0("<p>", 1:nb,"<br>Tooltip !</p>"), stringsAsFactors = FALSE)

edges <- data.frame(from = trunc(runif(nb)*(nb-1))+1,
                    to = trunc(runif(nb)*(nb-1))+1,
                    value = rnorm(nb, 10), label = paste("Edge", 1:nb),
                    title = paste0("<p>", 1:nb,"<br>Edge Tooltip !</p>"))

visNetwork(nodes, edges, width = "100%")


