clip2newExtent=function(mainDir, inDir, outDir, xmin, xmax, ymin, ymax, proj){
  lib = c("lidR", "raster")
  library("magrittr")
  
  #load lasfiles in one point cloud
  #make sure that input folder contains only the las files you need for your point cloud
  catalog = lidR::catalog(paste0(mainDir, inDir))
  las = lidR::readLAS(catalog, select = "xyzc")
  #change projection
  las@crs@projargs<- c("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+units=m +no_defs")
  #create rectangle with new extent
  mrect = matrix(c(xmin, xmax, ymin, ymax), ncol = 2)
  #clip las files to extent 
  las = las %>% lidR::lasclip("rectangle", mrect)
  #create output directory
  dir.create(paste0(mainDir, outDir = "/newExtent/"))
  #save cropped lasfile
  lidR::writeLAS(las, file = paste0(mainDir, outDir, "new_extent.las"))
}

#run
clip2newExtent(mainDir = "C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/createpackage",
               inDir = "/input/", outDir = "/newExtent/",
               xmin = "477390.0", xmax = "477580.0", ymin = "5631900.0", ymax = "5632100.0",
               proj = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+units=m +no_defs")
