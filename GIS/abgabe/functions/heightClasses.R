##input folder containing one file with requested extent
inDir <- "/newExtent/"
#@param heights heights = c(2,5,10,15,20,50)
#@param meta vector with grid info: "cellsize xyunits zunits coordsys zone horizdatum vertdatum"
#            example: meta = "30 M M 1 32 0 0"
#                          := cellsize=30 xyunits=M zunits,=M coordsys=UTM zone=32 horizdatum=0 vertdatum=0
#            cellsize: Grid cell size for the surface.
# 
#             xyunits: Units for LIDAR data XY:              M for meters,
#                                                            F for feet.
#             zunits: Units for LIDAR data elevations:       M for meters, 
#                                                            F for feet.
#             coordsys: Coordinate system for the surface:   0 for unknown,
#                                                            1 for UTM,
#                                                            2 for state plane.
#             zone: Coordinate system zone for the surface (0 for unknown).
#             horizdatum: Horizontal datum for the surface:  0 for unknown,
#                                                            1 for NAD27,
#                                                            2 for NAD83.
#             vertdatum: Vertical datum for the surface:     0 for unknown,
#                                                            1 for NGVD29,
#                                                            2 for NAVD88,
#                                                            3 for GRS80.
#             Fusion details: http://forsys.sefs.uw.edu/fusion/fusionlatest.html

heightClasses=function(pathFusion, mainDir, inDir, tempDir, outDir, meta, heights){
  
  #list all las files in inDir
  las_files<-list.files(paste0(mainDir,inDir), pattern=".las$", full.names=TRUE,recursive = TRUE)
  ##write the whole file path and name into a .txt
  lapply(las_files, write, paste0(mainDir,inDir,"lidar_files.txt"), append=T)
  
  ### basic informations
  system(paste0(pathFusion, "catalog.exe ", las_files[1]," ", mainDir, inDir, "info.html"))
  extentcsv<-read.csv(paste0(mainDir, inDir, "info.csv"), sep = ",", header = T)
  #get extent
  extent = paste0(" ", paste(extentcsv$MinX, extentcsv$MinY, extentcsv$MaxX, extentcsv$MaxY))
  ### Create a .las with groundpoints only
  #.txt --> class_GroundPts.las
  system(paste0(pathFusion, "clipdata.exe"," /class:2 ", mainDir, inDir, "lidar_files.txt ", 
                mainDir, tempDir, "classified_GroundPts.las", extent))
  
  ### Create the required PLANS DTM format
  #class_GroundPts.las --> GridSurf.dtm
  system(paste0(pathFusion, "gridsurfacecreate.exe ", mainDir, tempDir, "GridSurf.dtm ",
                meta, " ", mainDir, tempDir, "classified_GroundPts.las"))
  
  ### normalize heights of las point cloud
  #GridSurf.dtm --> NORMALIZED_point_cloud.las
  system(paste0(pathFusion, "clipdata.exe", " /height /dtm:", mainDir, tempDir,"GridSurf.dtm ",
                mainDir, inDir, "lidar_files.txt ", mainDir, tempDir, "normalized_point_cloud_LIDAR.las",
                extent))
  #count points for each height class and create .asc files
  #get cellsize
  #cz = gsub(" ", ",", meta)
  #cz1 = strsplit(cz, ",")
  #cellsize = as.numeric(cz1[[1]][1])
  
  #create output directory
  dir.create(paste0(mainDir, outDir))
  
  for (i in seq(length(heights)-1)){
    ### create slice between zmin and zmax
    system(paste0(paste0(pathFusion, "clipdata.exe"," /zmin:", heights[i], "/zmax:",
                         heights[i+1], " ", mainDir, tempDir,
                         "normalized_point_cloud_LIDAR.las ",
                         mainDir, outDir, "cloud_LIDAR_", heights[i],
                         "_", heights[i+1],".las"), extent)) 
  }
}

heightClasses(pathFusion = "C:/FUSION/",
              mainDir="C:/Users/Laura/Documents/Uni/Rmsc/Data/GIS/createpackage", 
              inDir="/newExtent/", tempDir="/temp/", outDir="/heightclasses/", 
              meta = "30 M M 1 32 0 0", heights = c(2,5,10,15,20,50))