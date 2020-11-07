
# Function cleans up ocotillo and segment data for multivariate and mixed effect modeling.

clean_data <- function(ocotillo_file, segments_file) {
  ocos = read.csv(ocotillo_file, fileEncoding="UTF-8-BOM",  stringsAsFactors=TRUE)
  segs = read.csv(segments_file, fileEncoding="UTF-8-BOM",  stringsAsFactors=TRUE)
  
  # Centering the ocos data
  
  ocos$Elevation_c <- ocos$Elevation - mean(ocos$Elevation, na.rm=TRUE)
  ocos$NN_c <- ocos$Neartest_Neighbor - mean(ocos$Neartest_Neighbor, na.rm=TRUE)
  ocos$Height_c <- ocos$Height - mean(ocos$Height, na.rm=TRUE)
  ocos$Circ_c <- ocos$Circumference - mean(ocos$Circumference, na.rm=TRUE)
  ocos$NBranch_c <- ocos$Number_Branches - mean(ocos$Number_Branches, na.rm=TRUE)
  ocos$Watershed_c <- ocos$Watershed_Dis - mean(ocos$Watershed_Dis, na.rm=TRUE)
  ocos$TSeg_c <- ocos$Terminal_SegIQR - mean(ocos$Terminal_SegIQR, na.rm=TRUE)
  ocos$COMP_dis_c <- ocos$COMP_dis - mean(ocos$COMP_dis, na.rm=TRUE)
  ocos$COMP_plant_b <- 0
  ocos$COMP_plant_b[ocos$COMP_plant_type == "shrub"] <- 1
  
  # Log transforming the data
  
  ocos$logElevation <- log(ocos$Elevation)
  ocos$logNN <- log(ocos$Neartest_Neighbor)
  ocos$logHeight <- log(ocos$Height)
  ocos$logNB <- log(ocos$Number_Branches)
  ocos$logCOMP <- log(ocos$COMP_dis)
  ocos$logCirc <- log(ocos$Circumference)
  ocos$logWater <- log(ocos$Watershed_Dis)
  ocos$logTS_IQR <- log(ocos$Terminal_SegIQR)
  ocos$NID_sum <- ocos$X1m_Num1 + ocos$X1m_Num2 + ocos$X1m_Num3 + ocos$X1m_Num4
  ocos$logNID <- log(ocos$NID_sum)
  
  # Coding Site for segs and ocos
  
  for(i in 1:20) {ocos$site[ocos$Plant == i] <- 0}
  for(i in 21:27) {ocos$site[ocos$Plant == i] <- 1}
  
  for(i in 1:20) {segs$site[segs$Tree == i] <- 0}
  for(i in 21:27) {segs$site[segs$Tree == i] <- 1}
  
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
  
  #break the data up into the different sampling sections
  seg_baj = segs[1:1000,]
  seg_organ = segs[1:1250,]
  
  col_names <- c("Tree", "Elevation", "Length", "seg_num", "branch_num", "site",
                 "Nearest_Neighbor", "Height",  "Circumference", 
                 "Number_Branches", "COMP_dis", "COMP_id", "COMP_plant_type",
                 "Watershed_dis", "IQR", "X1m_ID1", "X1m_type1", "X1m_Num1",
                 "X1m_ID2", "X1m_type2", "X1m_Num2",
                 "X1m_ID3", "X1m_type3", "X1m_Num3",
                 "X1m_ID4", "X1m_type4", "X1m_Num4",
                 "T1m_NumShrub", "T1m_NumNonshrub", "R1m_shrub2non", "T1m_sum")
  
  colnames(seg_baj) <- col_names
  colnames(seg_organ) <- col_names
  colnames(segs) <- col_names
  
  dfs <- list(seg_baj, seg_organ, segs)
  
  for (i in 1:length(dfs)) {
    
    # Rescaling Elevation - needed for mixed effect modeling
    dfs[[i]]$Elevation_km <-  dfs[[i]]$Elevation / 1000 
    dfs[[i]]$Watershed_km <-  dfs[[i]]$Watershed_dis / 1000 
    
    # Centering the data
    
    dfs[[i]]$Length_c <- dfs[[i]]$Length - mean(dfs[[i]]$Length, na.rm=TRUE)
    dfs[[i]]$Elevation_c <-  dfs[[i]]$Elevation - mean(dfs[[i]]$Elevation)
    dfs[[i]]$NN_c <- dfs[[i]]$Nearest_Neighbor - mean(dfs[[i]]$Nearest_Neighbor)
    dfs[[i]]$Height_c <- dfs[[i]]$Height - mean(dfs[[i]]$Height)
    dfs[[i]]$Circ_c <- dfs[[i]]$Circumference - mean(dfs[[i]]$Circumference)
    dfs[[i]]$NBranches_c <- dfs[[i]]$Number_Branches - mean(dfs[[i]]$Number_Branches)
    dfs[[i]]$COMP_dis_c <- dfs[[i]]$COMP_dis - mean(dfs[[i]]$COMP_dis)
    dfs[[i]]$COMP_plant_b <- 0
    dfs[[i]]$COMP_plant_b[dfs[[i]]$COMP_plant_type == "shrub"] <- 1
    dfs[[i]]$Watershed_c <- dfs[[i]]$Watershed_dis - mean(dfs[[i]]$Watershed_dis, na.rm=TRUE)

  }
  
  return(list(dfs, ocos))
  
}