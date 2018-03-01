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
#' @export runsingletreedetectlidr
#' @examples saves and overwrites (if existing) the results in './temp/' with different iterations of treeheight (bewteen 5 and 10m, by 1m stpes) and different crowndiameters (5,7,10,15,20,25) \cr
#' will produce 36 .shp files 'dalponte2016' and 6 .shp files 'watershed' as well as one .csv file 
#' \dontrun{
#' 
#'#requires to define function "SingleTreeDetectionLidR()" in advance
#'
#'#then run:
#'runSingleTreeDetectionLidR(chm = your_chm, las = you_las_files, output = temp,
#'                           treeheight = c(5:10), crowndm = c(5,7,10,15,20,25), 
#'                           overwrite = TRUE)
#' }

runSingleTreeDetectionLidR = function(chm, las, output, 
                                      treeheight, 
                                      crowndm = NULL, 
                                      overwrite = TRUE){
  lib = c("lidR", "raster", "rgdal", "EBImage")
  
  methvar = c(1:2)
  for (l in methvar){
    if (l == 1){
      SensAnalysisFiles_d = singletreedetectlidr(chm = chm, las = las, output = output, 
                                                treeheight = treeheight, 
                                                crowndm = crowndm, 
                                                method = "dalponte2016", overwrite = TRUE)
    }else if(l == 2){
      SensAnalysisFiles_w = singletreedetectlidr(chm = chm, las = las, output = output, 
                                                treeheight = treeheight, 
                                                method = "watershed", overwrite = TRUE)
    }
  }
  
  #save files and output table
  sa_files<- rbind(SensAnalysisFiles_d, SensAnalysisFiles_w)
  colnames(sa_files)<- c("filename", "method", "mintreeheight", "crowndiameter", "time", "treenumber")
  write.table(sa_files, file = paste0(temp, Sys.Date(), "SensAnalysisFiles_out.csv"), sep =",")
}

