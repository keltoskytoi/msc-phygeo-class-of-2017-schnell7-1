#' @author  j schnell, l giese, a schmidt
#' @description clearing() can be used to determine clearings in forestareas. The file beeing created is an .asc file with pixelinformations either '1' = is a clearing or '0' = is not a clearing.
#' to get best results you should try a variety of different values for the parameters and always check what fits your needs the best and what looks reasonable for you.
#' A good way to get better results is to create 2 files, one with a relativly small 'mat_size' and a second one with a higher 'mat_size'. Then vectorize your results and selcet via "is the one with high_mat_size *within* the one with low_mat_size" and use the low_mat_size ones that have the hight_mat_size ones within them.
#' @note  you need a normalized canopy heigth model, delivered by eg FUSION toolset http://forsys.sefs.uw.edu/fusion/fusionlatest.html, as well as the "raster" package
#' @param x your normalized canopy heigth model
#' @param mat_size the size of a matrix used as a filter to determine the cleraings preset is 17, Must be uneven!
#' @param min_size a min filter runs before the detection its preset is 3 if set to 1 it's disabled. It's recommended to use 3,5 or 7. Must be uneven!
#' @param mean_kill is an indicator of what is considered a clearing. Read it like "i accecpt trees in a clearing as high as 'mean_kill' units (meters in most cases) ". Preset is 7 can be any number
#' @param var_kill is used together with mean_kill to determine a clearing, for a better understanding use raster::focal with 'fun = var' and look at the histogram using hist("your_focalised_raster") you can get better results playing around with it, always check your results wheater you accept a pixel beeing a clearing
#' @param save if TRUE writes an .asc file to your working dir. using the form "clearing_ALL_YOUR_PARAMS.asc", if FALSE it returns the object to work with it in R eg my_clearing = clearing(..., save = FALSE)
#' @example 
#' \dontrun{
#' # write it to your harddrive, without using the min-filter
#' clearing(chm_las, mat_size = 35, min_size = 1, mean_kill = 8, var_kill = 1000, save=TRUE)
#' 
#' #save it to a variable within R 
#' my_clearing = clearing(chm_las, save = FALSE)
#' 
#' # use a for-loop to produce different sets, and write them to your harddrive
#' for(minv in c(3)){
#'for(matv in c(13,17,23,31,51)){
#'  for(mk in c(7)){
#'    for(vk in c(1000)){
#'      clearing(chm_las, mat_size = matv, min_size = minv, mean_kill = mk, var_kill = vk, save=T)
#'      
#'    }
#'  }
#'}
#'}
#' 
#' }
#'
#'@export clearing
#'
#'
#'
#'
#'
#'



clearing = function(x, mat_size = 17, min_size = 3, mean_kill = 7, var_kill = 1000, save=T) {
  print(paste(c(x, mat_size, min_size, mean_kill, var_kill, save)))
  w_min = matrix(1, min_size, min_size)
  w_ges = matrix(1, mat_size, mat_size)
  pre_min = raster::focal(x, w = w_min, fun = min, na.rm =T)
  dies = mean_kill
  das = var_kill
  lich = raster::focal(x = pre_min, w = w_ges, fun = function(x, mean_kill = dies, var_kill= das){
    if(mean(x, na.rm = T)< mean_kill && var(x, na.rm = T) < var_kill ){
      return(1)
    }
    else{
      return(0)
    }
  })
  
  if (save == T) {
    writeRaster(lich, filename = paste0("clearing_min", min_size, "_var", mat_size, "_", mean_kill, "_", var_kill, ".asc"))
  }
  else{
    return(lich)
  }
}

