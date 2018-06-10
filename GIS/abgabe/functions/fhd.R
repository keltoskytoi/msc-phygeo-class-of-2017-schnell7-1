#             Fusion details: http://forsys.sefs.uw.edu/fusion/fusionlatest.html

fhd=function(pathFusion, mainDir, inDir, tempDir, outDir, cellsize, heights){
  lib = c("raster")
  for (i in seq(length(heights)-1)){
  
    ###create point count for each pixel in raster in PLANE dtm
    system(paste0(pathFusion, "returndensity.exe ", mainDir, tempDir, "dens_cloud_LIDAR_", heights[i],"_",heights[i+1],"_",cellsize,".dtm ",
                  cellsize, " ", mainDir, inDir, "cloud_LIDAR_", heights[i],"_",heights[i+1],".las"))
    
    ###convert to ascii 
    system(paste0(pathFusion, "dtm2ascii.exe ",
                  mainDir, tempDir, "dens_cloud_LIDAR_",
                  heights[i],"_", heights[i+1],"_",cellsize,".dtm ",
                  mainDir, tempDir, "dens_cloud_LIDAR_",
                  heights[i],"_", heights[i+1],"_",cellsize,".asc"))
  }

  #list point density files
  list_fhd<-list.files(paste0(mainDir,tempDir), pattern = glob2rx("*.asc$"), full.names=T)
  ##write the whole file path and name into a .txt
  raster4calc<-lapply(list_fhd, function(x){
    raster(x)
  })
  
  #project
  for (l in 1:length(raster4calc)){
    raster::projection(raster4calc[[l]]) <- CRS("+init=epsg:25832")
    
  }
  
  #create output directory
  dir.create(paste0(mainDir, outDir))
  
  #calculate fhd
  raster4calc_log<-lapply(raster4calc, function(x){
    logb(x, base = exp(1))
  })
  
  raster4calc_prod<-mapply(x=raster4calc,y=raster4calc_log, function(x,y){
    x*y
  })
  
  sum = do.call("sum", raster4calc_prod)
  raster_out = ((-1)*sum)
  #save fhd raster
  raster::projection(raster_out) <- CRS("+init=epsg:25832")
  raster::writeRaster(raster_out, filename = paste0(mainDir, outDir, "fhd.tif"), format="GTiff", overwrite=TRUE)  
}

fhd(pathFusion = "C:/FUSION/", 
    mainDir="C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/createpackage", 
    inDir="/heightclasses/", tempDir="/temp/", outDir="/output/fhd/", 
    cellsize = c(30), heights = c(2,5,10,15,20,50))
