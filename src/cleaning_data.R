
clean_ocos_data = function(ocos) {

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
  
  # Binary variable
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
  ocos$logS1m <- log(ocos$T1m_sum)
  
  # Coding binary site
  for(i in 1:20) {ocos$Site[ocos$Tree == i] <- 0}  # bajada
  for(i in 21:27) {ocos$Site[ocos$Tree == i] <- 1} # plain
  
  return(ocos)
  
}

clean_segs_data = function(segments, ocos) {
  
  # Fix a typo
  segments[segments$Tree == 27,]$Elevation = 505
  
  # Merge datasets
  segs = segments %>% left_join(ocos, by = c("Tree", "Elevation")) 

  # Coding binary site
  for(i in 1:20) {segs$Site[segs$Tree == i] <- 0}  # bajada
  for(i in 21:27) {segs$Site[segs$Tree == i] <- 1} # plain
  
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
  segs$BranchLength_IQR_c <- segs$BranchLength_IQR - mean(segs$BranchLength_IQR, na.rm=TRUE)
  segs$Num_Nodes_c <- segs$Num_Nodes - mean(segs$Num_Nodes, na.rm=TRUE)
  segs$Inter_Dis_c <- segs$Inter_Dis - mean(segs$Inter_Dis, na.rm=TRUE)
  segs$Median_BL_c <- segs$Median_BranchLength - mean(segs$Median_BranchLength, na.rm=TRUE)
  segs$Arroyo_c <- segs$Arroyo_Dis - mean(segs$Arroyo_Dis, na.rm=TRUE)
  
  # Binary variable
  segs$Inter_plant_b <- 0
  segs$Inter_plant_b[segs$Inter_Plant_Group == "shrub"] <- 1
  
  return(segs)
}