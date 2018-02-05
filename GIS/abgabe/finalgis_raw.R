#### SKRIPT for Single Tree Detection with LidR - Sensitivity Analysis####
#***0. Preparation ***1. clip las file ***2. create dtm and normalized chm ***3. single tree detection

###--------------------------------------------------------------### 
### 0. preparation-----------------------------------------------###
###--------------------------------------------------------------### 
setwd("C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/POINTCLOUD_processing/Einzelbaum/temp")
input = "C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/POINTCLOUD_processing/Einzelbaum/input/"
output = "C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/POINTCLOUD_processing/Einzelbaum/output/"
temp = "C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/POINTCLOUD_processing/Einzelbaum/temp/"

#load packages
#install.packages("lidR", dependencies = T)
library(lidR)
library(raster)
#install.packages("raster")
library(rgdal)
library(maptools)
library(png)

## try http:// if https:// URLs are not supported
#source("https://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#install.packages("EBImage")
library(EBImage)

###-------------------------------------------------------------------### 
###--1. clip las file-------------------------------------------------###
###-------------------------------------------------------------------### 

#load lasfiles in one point cloud
#make sure that input folder contains only the las files you need for your point cloud
catalog = catalog(paste0(input))
las = readLAS(catalog, select = "xyzc")

#change projection
las@crs@projargs<- c("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+units=m +no_defs")
#las
plot(las, bg = "black")

#clip las files to extent 
xmin = 477393.0
xmax = 477461.00
ymin = 5631938.0
ymax = 5632004.0

mrect = matrix(c(xmin, xmax, ymin, ymax), ncol = 2)
las = las %>% lasclip("rectangle", mrect)
plot(las, bg = "black", trim)
#save cropped lasfile
writeLAS(las, file = paste0(temp, "las_cropped.las"))


###-------------------------------------------------------------------### 
###--2. create dtm and normalized chm---------------------------------###
###-------------------------------------------------------------------### 
#dtm
cellsize = 1
overwrite = TRUE
dtm1 = grid_terrain(las, res = cellsize, method = "knnidw", k = 10)
plot(dtm1)
dtm = as.raster(dtm1)
dtm@crs@projargs<- c("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+units=m +no_defs")
dtm
writeRaster(dtm, file = paste0(temp, "dtm_out.tif"), overwrite = overwrite)

#normalize
dtm = raster(paste0(temp, "dtm_out.tif"))
catalog = catalog(paste0(temp))
las = readLAS(catalog, select = "xyz")
lasnormalize(las, dtm)
plot(las)

# compute a canopy image
memorylim = 5000000
memory.limit(memorylim)
chm = grid_canopy(las, res = cellsize, subcircle = 0.1, na.fill = "knnidw", k = 4)
chm = as.raster(chm)
chm@crs@projargs<- c("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+units=m +no_defs")
plot(chm)

# smoothing post-process (e.g. 2x mean, 3x3 matrix)
smooth = TRUE
if (smooth == TRUE){
  kernel = matrix(1,3,3)
  chm = raster::focal(chm, w = kernel, fun = mean)
  chm = raster::focal(chm, w = kernel, fun = mean) 
}else{
  print("no smoothing")
}

raster::plot(chm, col = height.colors(50)) # check the image

#save output
overwrite = TRUE
writeRaster(chm, file = paste0(temp, "chm_out.tif"), overwrite = overwrite)

i = 8
j = 15
method = "dalponte2016"
extra = lastrees(las, "dalponte2016", chm, th = i, DIST = j, extra = TRUE)
plot(extra)
crown.shp <- rasterToPolygons(extra$Crown, dissolve = TRUE)
treenumber<-nrow(crown.shp@data)
writeOGR(crown.shp, paste0(temp, "crown", i,"_", j,"_", method, ".shp"), 
         paste0(basename(paste0(output, "crown", i,"_", j,"_", method, ".shp"))), 
         driver = "ESRI Shapefile", overwrite = overwrite)

###--------------------------------------------------------------### 
### 3. functions for single tree detection------------------------###
###--------------------------------------------------------------###

#define function1 SensitivityAnalysis 
SensitivityAnalysis = function(chm = chm,
                               las = las,
                               output = output,
                               treeheight = treeheight,
                               crowndm = NULL,
                               method = method,
                               overwrite = TRUE) {
  
  #load libraries
  library(lidR)
  library(raster)
  library(rgdal)
  library(maptools)
  library(png)
  library(EBImage)
  
  if (method == "dalponte2016"){
    # 1. way of segmentation: dalponte
    # DIST: crown diameter, th = hight below which pixel cannot me a crown, def = 2
    SensAnalysisFiles = matrix(NA, 36, 6)
    x = 0
    
    for (i in treeheight){
      for (j in crowndm){
        t1 = Sys.time()
        extra = lastrees(las, "dalponte2016", chm, th = i, DIST = j, extra = TRUE)
        t2 = Sys.time()
        crown.shp <- rasterToPolygons(extra$Crown, dissolve = TRUE)
        treenumber<-nrow(crown.shp@data)
        
        writeOGR(crown.shp, paste0(output, "crown", i,"_", j,"_", method, ".shp"), 
                 paste0(basename(paste0(output, "crown", i,"_", j,"_", method, ".shp"))), 
                 driver = "ESRI Shapefile", overwrite = overwrite)
        Maxima.shp <- rasterToPolygons(extra$Maxima, dissolve = TRUE)
        treenumber<-nrow(Maxima.shp@data)
        
        writeOGR(Maxima.shp, paste0(output, "Maxima_and_crown", i,"_", j,"_", method,".shp"), 
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
      extra = lastrees(las, "watershed", chm, th = i, extra = TRUE)
      t2 = Sys.time()
      extra.shp <- rasterToPolygons(extra, dissolve = TRUE)
      treenumber<-nrow(extra.shp@data)
      
      writeOGR(extra.shp, paste0(output, "extra", i,"_", method, ".shp"), 
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

runSingleTreeDetectionLidR = function(chm = chm, las = las, output = output, 
                                  treeheight = treehight, 
                                  crowndm = crowndm, 
                                  overwrite = overwrite){
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

## RUN: function for Single Tree Detection with LidR
# to create shape files and overview: SensAnalysisFiles_out.txt
finaloutput = runSingleTreeDetectionLidR(chm = chm,
                                  las = las,
                                  output = temp,
                                  treeheight = c(5:10),
                                  crowndm = c(5,7,10,15,20,25),
                                  overwrite = TRUE)