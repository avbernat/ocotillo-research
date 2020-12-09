
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
  ocos$BL_IQR_c <- ocos$BranchLength_IQR - mean(ocos$BranchLength_IQR, na.rm=TRUE)
  ocos$Median_BL_c <- ocos$Median_BranchLength - mean(ocos$Median_BranchLength, na.rm=TRUE)
  ocos$NumNodes_c <- ocos$Num_Nodes - mean(ocos$Num_Nodes, na.rm=TRUE)
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
  ocos$logBL_IQR <- log(ocos$BranchLength_IQR)
  ocos$logMedian_BL <- log(ocos$Median_BranchLength)
  ocos$logNodes <- log(ocos$Num_Nodes)
  ocos$logInterD <- log(ocos$Inter_Dis)
  ocos$logCirc <- log(ocos$Circumference)
  ocos$logArroyo <- log(ocos$Arroyo_Dis)
  ocos$logTS_IQR <- log(ocos$Terminal_SegIQR)
  ocos$logS1m <- log(ocos$X1m_sum)
  
  # Coding site for segs and ocos
  
  for(i in 1:20) {ocos$Site[ocos$Plant == i] <- 0} # bajada
  for(i in 21:27) {ocos$Site[ocos$Plant == i] <- 1} # plain
  
  for(i in 1:20) {segs$Site[segs$Tree == i] <- 0}
  for(i in 21:27) {segs$Site[segs$Tree == i] <- 1}
  
  # Create an empty dataframe for segs data
  
  d = data.frame(nrow = length(segs[,1]), ncol = 8)
  
  # Populate the empty dataframe with ocos data in the format of the segs data:
  
  for(col in c(1:10)){
    for(row in c(1:length(segs[,1]))){
      for(tree in c(1:27)){
        while(segs[row,1] == tree){
          d[row,col] = ocos[tree,col+2] 
          tree=tree+1}	
      }	
    }		
  }	
  for(col in c(11:28)){
    for(row in c(1:length(segs[,1]))){
      for(tree in c(1:27)){
        while(segs[row,1] == tree){
          d[row,col] = ocos[tree,col+3] 
          tree=tree+1}
      }	
    }	
  }	
  
  segs = cbind(segs,d)
  
  col_names <- c("Tree", "Elevation", "Length", "seg_num", "branch_num", "Site",
                 "Intra_Dis", "Height",  "Circumference", 
                 "Number_Branches", "BL_IQR", "Median_BL", "NumNodes",
                 "Inter_Dis", "Inter_ID", "Inter_Plant_Group",
                 "Arroyo_Dis", "IQR", "X1m_ID1", "X1m_type1", "X1m_Num1",
                 "X1m_ID2", "X1m_type2", "X1m_Num2",
                 "X1m_ID3", "X1m_type3", "X1m_Num3",
                 "X1m_ID4", "X1m_type4", "X1m_Num4",
                 "T1m_NumShrub", "T1m_NumCacti", "R1m_shrub2cactus", "T1m_sum")
  
  colnames(segs) <- col_names
  
  # Rescaling Elevation and Arroyo Distance - needed for mixed effect modeling
  
  segs$Elevation_km <- segs$Elevation / 1000
  segs$Arroyo_km <- segs$Arroyo_Dis / 100
  
  # Centering the data
  
  segs$Length_c <- segs$Length - mean(segs$Length, na.rm=TRUE)
  segs$Elevation_c <- segs$Elevation - mean(segs$Elevation, na.rm=TRUE)
  segs$Intra_Dis_c <- segs$Intra_Dis - mean(segs$Intra_Dis, na.rm=TRUE)
  segs$Height_c <- segs$Heigh - mean(segs$Height, na.rm=TRUE)
  segs$Circ_c <- segs$Circumference - mean(segs$Circumference, na.rm=TRUE)
  segs$NBranches_c <- segs$Number_Branches - mean(segs$Number_Branches, na.rm=TRUE)
  segs$BL_IQR_c <- segs$BL_IQR - mean(segs$BL_IQR, na.rm=TRUE)
  segs$Median_BL_c <- segs$Median_BL - mean(segs$Median_BL, na.rm=TRUE)
  segs$NumNodes_c <- segs$NumNodes - mean(segs$NumNodes, na.rm=TRUE)
  segs$Inter_Dis_c <- segs$Inter_Dis - mean(segs$Inter_Dis, na.rm=TRUE)
  segs$Inter_plant_b <- 0
  segs$Inter_plant_b[segs$Inter_Plant_Group == "shrub"] <- 1
  segs$Median_BL_c <- segs$Median_BL - mean(segs$Median_BL, na.rm=TRUE)
  segs$Arroyo_c <- segs$Arroyo_Dis - mean(segs$Arroyo_Dis, na.rm=TRUE)
  
  
  return(list(segs, ocos))
  
}