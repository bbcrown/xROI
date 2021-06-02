#' Plot CLI array
#'
#' This function plots a CLI array on the graphics.
#'
#' @param clArray a numeric array. A 3D array of CLI (HxWx3)
#' @param bands an integer vector. integer vector of length 3, showing bands to be plotted
#' @return invisibly returns the dimension of the plotted image
#' @keywords  exract chromatic coefficients rcc gcc bcc
#' @export
#' @import jpeg
#' @examples
#' f <- system.file(package = 'xROI', 'dukehw-cli.jpg')
#' jp <- jpeg::readJPEG(f)
#' if(interactive())plotCLArray(jp)
#'
plotCLArray <- function(clArray, bands=1:3){
  tmp <- tempfile()
  if(length(dim(clArray))==2)
    writeJPEG(clArray, target = tmp)
  else
    writeJPEG(clArray[,,bands], target = tmp)

  plotJPEG(tmp)
}

