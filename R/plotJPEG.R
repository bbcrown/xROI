#' Plot JPEG image
#'
#' This funciton plots a jpeg image as a raster given image path.
#'
#' @param path a character string. path to the JPEG file to be plotted.
#' @param add logical. logical variable whether to add the image to the existing graphics.
#' @param xlim numeric vector of lenght 2, x axis range
#' @param ylim numeric vector of lenght 2, y axis range
#' @return This function returns statistical metrics for each color channel. The function returns NULL, if dimensions do not agree.
#' @keywords  exract chromatic coefficients rcc gcc bcc
#' @import jpeg
#' @importFrom graphics abline axis locator mtext par polygon rasterImage rect
#' @export
#' @examples
#' f <- system.file(package = 'xROI', 'dukehw.jpg')
#' if (interactive() ) plotJPEG(f)
#'
plotJPEG <- function(path, add=FALSE, xlim = NULL, ylim = NULL)
{
  jpgNative <-  readJPEG(path, native=T) # read the file
  res <-  dim(jpgNative)[2:1] # get the resolution
  if(is.null(xlim)) xlim <- c(1,res[1])
  if(is.null(ylim)) ylim <- c(1,res[2])
  if (!add) # initialize an empty plot area if add==FALSE
    plot(NA, xlim = xlim, ylim = ylim, type='n',
         xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
  rasterImage(jpgNative,1,1,res[1],res[2])
  invisible(c(res=res))
}


