#' @title Single tree detection using LAS files (algorithm based on package LidR)
#' 
#' @describtion this function allows you to detect singele trees using LiDar technology. You need some kind of normalized canopy height model "CHM" as well as .las files first \cr 
#' the output is 1 ESRI shapefile per "treeheight" for method 'watershed' and all combinations bewteen "treeheight" and "crowndm" for method 'dalponte2016', as well as one .csv file summarizing your results per run \cr 
#' ! save your .csv file after each run !
#' 
#' @author Laura Giese, Johannes Schnell & Alena Schmid
#' 
#' @note both 'dalponte2016' and 'watershed' are used as methods to detect single trees \cr required packages are: lidR, rgdal & raster \cr
#'you also need 'EBImage' which can be installed using the instruction below or follow instructions \href<-{"https://www.bioconductor.org/"}{here}  \cr
#'\code{
#'"## try http:// if https:// URLs are not supported
#'source("https://bioconductor.org/biocLite.R")
#'biocLite("EBImage")
#'install.packages("EBImage")"
#'} \cr
#'! make sure to save the produced .csv file after each run !
#' @param chm is the normalized canopy height model 
#' @param las is the underlaying .las file
#' @param output is the path where you want to save the results
#' @param treeheight is the minimum height an object will be considered as 'treecrown' [m] (try different values see example)
#' @param crowndm is the crowndiameter [m] (try different values see example); preset is 'NULL'
#' @param overwrite TRUE or FALSE. Do you want to overwrite existing outputs?; preset is 'TRUE'
#' 
#' @export singletreedetectlidr
#' @examples saves and overwrites (if existing) the results in './temp/' with different iterations of treeheight (bewteen 5 and 10m, by 1m stpes) and different crowndiameters (5,7,10,15,20,25) \cr
#' will produce 36 .shp files 'dalponte2016' and 6 .shp files 'watershed' as well as one .csv file 
#' \dontrun{
#'SingleTreeDetectionLidR(chm = your_chm, las = you_las_files, output = temp,
#'                        treeheight = c(5:10), crowndm = c(5,7,10,15,20,25), 
#'                        overwrite = TRUE)
#' }

singletreedetectlidr = function(chm, las,
                               output,
                               treeheight,
                               crowndm = NULL,
                               overwrite = TRUE,
                               method = method) {
  
  lib = c("lidR", "raster", "rgdal", "EBImage")
  
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
        
        rgdal::writeOGR(crown.shp, paste0(output, "crown_th", i,"_cd", j,"_", method, ".shp"), 
                        paste0(basename(paste0(output, "crown_th", i,"_cd", j,"_", method, ".shp"))), 
                        driver = "ESRI Shapefile", overwrite = overwrite)
        Maxima.shp <- raster::rasterToPolygons(extra$Maxima, dissolve = TRUE)
        treenumber<-nrow(Maxima.shp@data)
        
        rgdal::writeOGR(Maxima.shp, paste0(output, "Maxima_th", i,"_cd", j,"_", method,".shp"), 
                        paste0(basename(paste0(output, "Maxima_th", i,"_cd", j,"_", method, ".shp"))), 
                        driver = "ESRI Shapefile", overwrite = overwrite)
        print("dalponte2016")
        x = x+1
        SensAnalysisFiles[x,1] = paste0("Maxima_and_crown_th", i,"_cd", j,"_", method, ".shp")
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
      
      rgdal::writeOGR(extra.shp, paste0(output, "crown_th", i,"_", method, ".shp"), 
                      paste0(basename(paste0(output, "crown_th", i,"_", method, ".shp"))), 
                      driver = "ESRI Shapefile", overwrite = overwrite)
      print("watershed")
      x = x+1
      SensAnalysisFiles[x,1] = paste0("crown_th", i,"_", method, ".shp")
      SensAnalysisFiles[x,2] = method
      SensAnalysisFiles[x,3] = i
      SensAnalysisFiles[x,5] = t2-t1
      SensAnalysisFiles[x,6] = treenumber
    }
    
  }
  return(SensAnalysisFiles)
}
