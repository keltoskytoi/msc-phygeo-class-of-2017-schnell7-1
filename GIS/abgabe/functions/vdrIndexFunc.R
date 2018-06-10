#tempDir : contains caldern_GridSurf.dtm
#CHMDir: contains canopy height model of same resolution as output VDR raster (= cellsize)

#Fusion: http://forsys.sefs.uw.edu/fusion/fusionlatest.html

vdrIndexFunc = function(pathFusion,
                        mainDir,
                        inDir,
                        CHMDir,
                        tempDir,
                        outDir,
                        cellsize = 1,
                        parameter = 31) {

  if (!dir.exists(paste0(mainDir, outDir))) 
    dir.create(paste0(mainDir, outDir))
  
  lib=c("rgdal", "raster")

  #calculate 50th percentile of Lidar point cloud per pixel
  #output: .csv files are important!
 
  system(paste0(pathFusion, "gridmetrics.exe ", "/raster:p50 ", mainDir, tempDir,
                "GridSurf.dtm ", "0 ", cellsize, " ", mainDir, outDir, 
                "HOME_cz_", cellsize,".dtm ", mainDir, inDir, "new_extent.las"))

  
  #take 31 column of all_returns_elevation_stats.csv as this is the 50th percentile
  system(paste0(pathFusion, "CSV2Grid.exe ", mainDir, outDir, 
                "HOME_cz_", cellsize,"_all_returns_elevation_stats.csv ", 
                parameter, " ", mainDir, outDir, "HOMEp50_cz_", cellsize,".asc "))
  
  #die eigentliche Rechnung
  ch = raster::raster(paste0(mainDir, CHMDir, "canopy_surface_mod.asc"))
  HOME = raster::raster(paste0(mainDir, outDir, "HOMEp50_cz_", cellsize,".asc"))
  raster::origin(HOME)[] <- raster::origin(ch)
  VDR = (ch - HOME) / ch
  #fill negative values with NA
  VDR@data@values[which(VDR@data@values >= 1)] <- NA
  VDR@data@values[which(VDR@data@values < 0)] <- NA
  
  raster::writeRaster(VDR, filename = paste0(mainDir, outDir, "VDR_cz_", cellsize,".asc"))

}

vdrIndexFunc(pathFusion = "C:/FUSION/",
             mainDir="C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/createpackage", 
             inDir="/newExtent/", CHMDir="/CHM/", tempDir= "/temp/", outDir="/output/vdr/",
             cellsize = 30,
             parameter = 31)
