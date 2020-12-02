
# Function cleans up ocotillo and segment data for multivariate and mixed effect modeling.

clean_data <- function(ocotillo_file, segments_file) {
  ocos = read.csv(ocotillo_file, fileEncoding="UTF-8-BOM",  stringsAsFactors=TRUE)
  segs = read.csv(segments_file, fileEncoding="UTF-8-BOM",  stringsAsFactors=TRUE)
  
  # Centering the ocos data
  
  ocos$Elevation_c <- ocos$Elevation - mean(ocos$Elevation, na.rm=TRUE)
  ocos$Intra_Dis_c<- ocos$Intra_Dis - mean(ocos$Intra_Dis, na.rm=TRUE)
  ocos$Height_c <- ocos$Height - mean(ocos$Height, na.rm=TRUE)
  ocos$Circ_c <- ocos$Circumference - mean(ocos$Circumference, na.rm=TRUE)
  ocos$NBranch_c <- ocos$Number_Branches - mean(ocos$Number_Branches, na.rm=TRUE)
  ocos$Arroyo_c <- ocos$Arroyo_Dis - mean(ocos$Arroyo_Dis, na.rm=TRUE)
  ocos$TSegIQR_c <- ocos$Terminal_SegIQR - mean(ocos$Terminal_SegIQR, na.rm=TRUE)
  ocos$Inter_Dis_c <- ocos$Inter_Dis - mean(ocos$Inter_Dis, na.rm=TRUE)
  ocos$Inter_plant_b <- 0
  ocos$Inter_plant_b[ocos$Inter_Plant_Group == "shrub"] <- 1
  
  # Log transforming the data
  
  ocos$logElevation <- log(ocos$Elevation)
  ocos$logIntraD <- log(ocos$Intra_Dis)
  ocos$logHeight <- log(ocos$Height)
  ocos$logNB <- log(ocos$Number_Branches)
  ocos$logInterD <- log(ocos$Inter_Dis)
  ocos$logCirc <- log(ocos$Circumference)
  ocos$logArroyo <- log(ocos$Arroyo_Dis)
  ocos$logTS_IQR <- log(ocos$Terminal_SegIQR)
  ocos$logS1m <- log(ocos$X1m_sum)
  
  # Coding Site for segs and ocos
  
  for(i in 1:20) {ocos$Site[ocos$Plant == i] <- 0} # bajada
  for(i in 21:27) {ocos$Site[ocos$Plant == i] <- 1} # plain
  
  for(i in 1:20) {segs$Site[segs$Tree == i] <- 0}
  for(i in 21:27) {segs$Site[segs$Tree == i] <- 1}
  
  # Create an empty dataframe
  
  d = data.frame(nrow = length(segs[,1]), ncol = 8)
  
  # Populate the empty dataframe with ocos data in the format of the segs data:
  
  for(col in c(1:7)){
    for(row in c(1:length(segs[,1]))){
      for(tree in c(1:27)){
        while(segs[row,1] == tree){
          d[row,col] = ocos[tree,col+2] 
          tree=tree+1}	
      }	
    }		
  }	
  for(col in c(8:25)){
    for(row in c(1:length(segs[,1]))){
      for(tree in c(1:27)){
        while(segs[row,1] == tree){
          d[row,col] = ocos[tree,col+3] 
          tree=tree+1}
      }	
    }	
  }	
  
  segs = cbind(segs,d)
  
  seg_baj = segs[1:1000,]
  
  col_names <- c("Tree", "Elevation", "Length", "seg_num", "branch_num", "Site",
                 "Intra_Dis", "Height",  "Circumference", 
                 "Number_Branches", "Inter_Dis", "Inter_ID", "Inter_Plant_Group",
                 "Arroyo_Dis", "IQR", "X1m_ID1", "X1m_type1", "X1m_Num1",
                 "X1m_ID2", "X1m_type2", "X1m_Num2",
                 "X1m_ID3", "X1m_type3", "X1m_Num3",
                 "X1m_ID4", "X1m_type4", "X1m_Num4",
                 "T1m_NumShrub", "T1m_NumCacti", "R1m_shrub2cactus", "T1m_sum")
  
  colnames(seg_baj) <- col_names
  colnames(segs) <- col_names
  
  dfs <- list(seg_baj, segs)
  
  for (i in 1:length(dfs)) {
    
    # Rescaling Elevation - needed for mixed effect modeling
    
    dfs[[i]]$Elevation_km <-  dfs[[i]]$Elevation / 1000 
    dfs[[i]]$Arroyo_km <-  dfs[[i]]$Arroyo_Dis / 1000 
    
    # Centering the data
    
    dfs[[i]]$Length_c <- dfs[[i]]$Length - mean(dfs[[i]]$Length, na.rm=TRUE)
    dfs[[i]]$Elevation_c <-  dfs[[i]]$Elevation - mean(dfs[[i]]$Elevation)
    dfs[[i]]$Intra_Dis_c <- dfs[[i]]$Intra_Dis - mean(dfs[[i]]$Intra_Dis)
    dfs[[i]]$Height_c <- dfs[[i]]$Height - mean(dfs[[i]]$Height)
    dfs[[i]]$Circ_c <- dfs[[i]]$Circumference - mean(dfs[[i]]$Circumference)
    dfs[[i]]$NBranches_c <- dfs[[i]]$Number_Branches - mean(dfs[[i]]$Number_Branches)
    dfs[[i]]$Inter_Dis_c <- dfs[[i]]$Inter_Dis - mean(dfs[[i]]$Inter_Dis)
    dfs[[i]]$Inter_plant_b <- 0
    dfs[[i]]$Inter_plant_b[dfs[[i]]$Inter_Plant_Group == "shrub"] <- 1
    dfs[[i]]$Arroyo_c <- dfs[[i]]$Arroyo_Dis - mean(dfs[[i]]$Arroyo_Dis, na.rm=TRUE)
    
  }
  
  return(list(dfs, ocos))
  
}