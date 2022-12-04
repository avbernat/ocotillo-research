
Round <- function(number){
  x <- round(number, 1)
  if(x%%1 == 0){
    return(paste(as.character(x), ".0", sep = ""))
  }
  else{
    return(x)
  }
}

PCA_graphs <- function(dataset, PCA_title){
  # cos2 and the alpha.var: alpha.var colours variables by cos2 
  # (importance of most important PC to variable), 
  # see https://personal.utdallas.edu/~herve/abdi-awPCA2010.pdf
  
  GFpca <- PCA(dataset, scale.unit = TRUE, graph = TRUE, ncp = 10)
  
  eig.val <- get_eigenvalue(GFpca)
  var.val <- GFpca$var
  print(eig.val) #will only show in in console
  print(var.val)
  
  scree <- fviz_eig(GFpca, addlabels = TRUE, ylim = c(0, 50))
  print(scree)
  
  labX <- paste("PC1 (", Round(eig.val[1, 2]), "%)", sep = "")
  labY <- paste("PC1 (", Round(eig.val[2, 2]), "%)", sep = "")
  leplot <- fviz_pca_biplot(GFpca, geom.id = c("point"), 
                            geom.var = c("arrow", "text"), 
                            #alpha.var = "cos2",
                            label = "var", repel = T, 
                            col.ind = "gray", col.var = "black")
  
  ggpubr::ggpar(leplot, title = PCA_title, xlab = labX, ylab = labY, 
                ggtheme = theme_classic(), font.main = c(20, "bold"), 
                font.x = 14, font.y = 14, font.tickslab = 12
                #, xlim = c(-5, 6) #uncomment for PCA1 (without IQR vars)
  )
}