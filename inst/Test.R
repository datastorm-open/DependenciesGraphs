
library(FactoMineR)
require(ibr)
library(FactoMineR)
require(FactoMineR)
data(decathlon)
data(tea)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13)


essaifun <- function(lala) {
    lala = lala + 1
}

essaifun2 <- function(lolo0) {
    essaifun
}

essaifun3 <- function(lolo0) {
    hhihi <- MCA(tea, quanti.sup = 19, quali.sup = 20:36)
    return(hhihi)
}

nn <- essaifun3()
data(ozone, package = "ibr")
res.ibr <- ibr(ozone[, -1], ozone[, 1], df = 1.1)
 
