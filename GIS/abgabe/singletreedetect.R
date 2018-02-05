#' @title Single tree detection using LAS files
#' 
#' @describtion this function allows you to detect singele trees using LiDar technology. You need some kind of normalized canopy height model "CHM" as well as .las files first
#' 
#' @author Laura Giese, Johannes Schnell & Alena Schmid
#' 
#' @note both 'dalponte2016' and 'watershed' are used as methods to detect single trees
#' 
#' @param chm is the normalized canopy heigt model 
#' @param las is the underlaying .las file
#' @param output is the filename to save results (ESRI shapefiles)
#' @param treeheight is the maximum height an object will be considered a 'tree' [m] (try different values)
#' @param crowndm is the crowndiameter [m] (try different values), preset is 'NULL'
#' @param overwrite TRUE or FALSE. Do you want to override existing outputs?, preset is 'TRUE'
#' 
#' @export runsingletreedetectlidr

###--------------------------------------------------------------### 
### 3. functions for single tree detection-----------------------###
###steps 0-2 are preperation check out the github folder "abgabe"###
###--------------------------------------------------------------###

#define function1 SensitivityAnalysis which is part of the final function
SensitivityAnalysis = function(chm, las,
                               output,
                               treeheight,
                               crowndm = NULL,
                               overwrite = TRUE) {

  
  if (method == "dalponte2016"){
    # 1. way of segmentation: dalponte
    # DIST: crown diameter, th = hight below which pixel cannot me a crown, def = 2
    SensAnalysisFiles = matrix(NA, 36, 6)
    x = 0
    
    for (i in treeheight){
      for (j in crowndm){
        t1 = Sys.time()
        extra = lidR::lastrees(las, "dalponte2016", chm, th = i, DIST = j, extra = TRUE)
        t2 = Sys.time()
        crown.shp <- raster::rasterToPolygons(extra$Crown, dissolve = TRUE)
        treenumber<-nrow(crown.shp@data)
        
        rgdal::writeOGR(crown.shp, paste0(output, "crown", i,"_", j,"_", method, ".shp"), 
                 paste0(basename(paste0(output, "crown", i,"_", j,"_", method, ".shp"))), 
                 driver = "ESRI Shapefile", overwrite = overwrite)
        Maxima.shp <- raster::rasterToPolygons(extra$Maxima, dissolve = TRUE)
        treenumber<-nrow(Maxima.shp@data)
        
        rgdal::writeOGR(Maxima.shp, paste0(output, "Maxima_and_crown", i,"_", j,"_", method,".shp"), 
                 paste0(basename(paste0(output, "Maxima", i,"_", j,"_", method, ".shp"))), 
                 driver = "ESRI Shapefile", overwrite = overwrite)
        print("dalponte2016")
        x = x+1
        SensAnalysisFiles[x,1] = paste0("Maxima_and_crown", i,"_", j,"_", method, ".shp")
        SensAnalysisFiles[x,2] = method
        SensAnalysisFiles[x,3] = i
        SensAnalysisFiles[x,4] = j
        SensAnalysisFiles[x,5] = t2-t1
        SensAnalysisFiles[x,6] = treenumber
        
      }
    }
    
  }else if(method == "watershed"){
    SensAnalysisFiles = matrix(NA, 6, 6)
    x = 0
    #2. way of segmentation: watershed
    #th: hight below which pixel cannot me a crown, def = 2
    for (i in treeheight){
      t1 = Sys.time()
      extra = lidR::lastrees(las, "watershed", chm, th = i, extra = TRUE)
      t2 = Sys.time()
      extra.shp <- raster::rasterToPolygons(extra, dissolve = TRUE)
      treenumber<-nrow(extra.shp@data)
      
      raster::writeOGR(extra.shp, paste0(output, "extra", i,"_", method, ".shp"), 
               paste0(basename(paste0(output, "extra", i,"_", method, ".shp"))), 
               driver = "ESRI Shapefile", overwrite = overwrite)
      print("watershed")
      x = x+1
      SensAnalysisFiles[x,1] = paste0("extra", i,"_", method, ".shp")
      SensAnalysisFiles[x,2] = method
      SensAnalysisFiles[x,3] = i
      SensAnalysisFiles[x,5] = t2-t1
      SensAnalysisFiles[x,6] = treenumber
    }
    
  }
  return(SensAnalysisFiles)
}

###define function2 "runSingleTreeDetectionLidR" 
#to run function1 SensitivityAnalysis (both methods: dalponte2016&watershed)

runSingleTreeDetectionLidR = function(chm, las, output, 
                                      treehight, 
                                      crowndm = NULL, 
                                      overwrite = TRUE){
  methvar = c(1:2)
  for (l in methvar){
    if (l == 1){
      SensAnalysisFiles_d = SensitivityAnalysis(chm = chm, las = las, output = output, 
                                                treeheight = treeheight, 
                                                crowndm = crowndm, 
                                                method = "dalponte2016", overwrite = TRUE)
    }else if(l == 2){
      SensAnalysisFiles_w = SensitivityAnalysis(chm = chm, las = las, output = output, 
                                                treeheight = treeheight, 
                                                method = "watershed", overwrite = TRUE)
    }
  }
  
  #save files and output table
  sa_files<- rbind(SensAnalysisFiles_d, SensAnalysisFiles_w)
  colnames(sa_files)<- c("filename", "method", "mintreeheight", "crowndiameter", "time", "treenumber")
  write.table(sa_files, file = paste0(temp, "SensAnalysisFiles_out.txt"), sep =",")
}

