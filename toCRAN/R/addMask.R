#' Plot or add a mask
#'
#' This function plot or add a mask raster on the graphics.
#' @param mask binary or logical matrix, describing the mask (1 for selected pixels, 0 for not selected pixels)
#' @param add whether to add the mask to and existing plot
#' @param col color value of the plotted mask
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
#' #add the same mask in red color to the existing plot
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
}




