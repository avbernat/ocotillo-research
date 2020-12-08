#load packages
require(FactoMineR)
require(factoextra)
require(corrplot)
require(readr)

#for plotting
Round <- function(number){
  x <- round(number, 1)
  if(x%%1 == 0){
    return(paste(as.character(x), ".0", sep = ""))
  }
  else{
    return(x)
  }
}

#The actual thing
PCA_graphs <- function(dataset){
    GFpca <- PCA(dataset, scale.unit = TRUE, graph = TRUE, ncp = 10)
    
    eig.val <- get_eigenvalue(GFpca)
    var.val <- GFpca$var
    print(eig.val) #will only show in in console
    print(var.val)
    
    scree <- fviz_eig(GFpca, addlabels = TRUE, ylim = c(0, 50))
    print(scree)
    
    labX <- paste("PC1 (", Round(eig.val[1, 2]), "%)", sep = "")
    labY <- paste("PC1 (", Round(eig.val[2, 2]), "%)", sep = "")
    leplot <- fviz_pca_biplot(GFpca, geom.id = c("point"), geom.var = c("arrow", "text"), label = "var", repel = TRUE, col.ind = "gray", col.var = "black")
    ggpubr::ggpar(leplot,title = "Principal Component Analysis of F. splendens Morphology", xlab = labX, ylab = labY,ggtheme = theme_classic(), font.main = 16, font.x = 14, font.y = 14, font.tickslab = 12)
}

#all of them
PCA_graphs(growthForm)

#all of them transformed
GrowthForm <- growthForm
GrowthForm[,-2] <- log(GrowthForm[,-2])
PCA_graphs(GrowthForm)

#transformed, without 1
PCA_graphs(GrowthForm[-1,])
#transformed, without 1 and 14
PCA_graphs(GrowthForm[c(-1, -14),])