#' Plot or add a mask
#'
#' This function plots or adds a mask raster on the default graphics.
#' @param mask a binary or logical matrix, describing the mask (0:black for selected pixels, 1:white for not selected pixels)
#' @param add a logical variable, whether to add the mask to an existing plot
#' @param col a character string, color value of the plotted mask
#' @keywords  plot mask raster
#' @export
#' @rawNamespace import(raster, except = quantile)
#' @import rgdal
#' @import sp
#' @import jpeg
#' @import tiff
#' @examples
#'
#' #read a mask file in TIFF format
#' m <- tiff::readTIFF(system.file(package = 'xROI', 'dukehw-mask.tif'))
#' str(m)
#'
#' #plot the mask in black color
#' addMask(m, add = FALSE)
#'
#' #add the same mask in the red color to the existing plot
#' addMask(m, add = TRUE, col = 'red')
#'
addMask <- function(mask, add = TRUE, col='black'){
  wd <- getwd()
  setwd(tmpDir())
  writeTIFF(mask*1, 'tmp.tif')
  rmask <- raster('tmp.tif')
  rmask[rmask!=0] <- NA

  plot(rmask,legend=F, add=add, col=col)
  file.remove('tmp.tif')
  setwd(wd)
  return(0)
}




