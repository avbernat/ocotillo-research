
Round <- function(number){
  x <- round(number, 1)
  if(x%%1 == 0){
    return(paste(as.character(x), ".0", sep = ""))
  }
  else{
    return(x)
  }
}

PCA_graphs <- function(dataset, PCA_title, run_corr=FALSE){
  
  # cos2 and the alpha.var: alpha.var colours variables by cos2 
  # (importance of most important PC to variable), 
  # see https://personal.utdallas.edu/~herve/abdi-awPCA2010.pdf
  
  GFpca <- PCA(dataset, scale.unit = TRUE, graph = FALSE, ncp = 10)
  eig.val <- get_eigenvalue(GFpca)
  var.val <- GFpca$var
  scree <- fviz_eig(GFpca, addlabels = TRUE, ylim = c(0, 50))
  
  labX <- paste("PC1 (", Round(eig.val[1, 2]), "%)", sep = "")
  labY <- paste("PC1 (", Round(eig.val[2, 2]), "%)", sep = "")
  leplot <- fviz_pca_biplot(GFpca, geom.id = c("point"), 
                            geom.var = c("arrow", "text"), 
                            #alpha.var = "cos2",
                            label = "var", repel = T, 
                            col.ind = "gray", col.var = "black")
  
  pca = ggpubr::ggpar(leplot, title = PCA_title, xlab = labX, ylab = labY, 
                ggtheme = theme_classic(), font.main = c(20, "bold"), 
                font.x = 14, font.y = 14, font.tickslab = 12
                #, xlim = c(-5, 6) #uncomment for PCA1 (without IQR vars)
  )
  
  if (run_corr) {
    corr_dataset = cor(dataset)
    pvalues <- cor.mtest(dataset)$p
    cor_graph_sig = corrplot.mixed(corr_dataset, number.cex = .7, p.mat=pvalues, sig.level=0.05)
    
    corrplot(var.val$cos2, number.cex = .7, is.corr=FALSE)
    corrplot(var.val$contrib, number.cex = .7, is.corr=FALSE)
  }
  
  pcaResults <- as.data.frame(GFpca$ind$coord)
  
  output_list = list(eig.val, var.val, scree, GFpca, pca, pcaResults)
  
  return(output_list)
}

run_corrplots = function(dataset, pca_vars) {
  
  corr_dataset = cor(dataset)
  pvalues <- cor.mtest(dataset)$p
  corrplot.mixed(corr_dataset, number.cex = .7, p.mat=pvalues, sig.level=0.05, 
                                        title="significant correlations", mar=c(0,0,1,0))
  
  corrplot(pca_vars$cos2, number.cex = .7, is.corr=FALSE, title="cos2 (variable representation)", mar=c(0,0,1,0))
  corrplot(pca_vars$contrib, number.cex = .7, is.corr=FALSE, title="contribution", mar=c(0,0,1,0))
}