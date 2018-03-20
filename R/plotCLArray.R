#' Plot CLI array
#'
#' This function plots a CLI array on the graphics.
#'
#' @param clArray A 3D array of CLI (HxWx3)
#' @param bands integer vector of length 3, showing bands to be plotted
#' @keywords  exract chromatic coefficients rcc gcc bcc
#' @export
#' @import jpeg
#' @examples
#' f <- system.file(package = 'xROI', 'dukehw-cli.jpg')
#' jp <- jpeg::readJPEG(f)
#' plotCLArray(jp)
#'
plotCLArray <- function(clArray, bands=1:3){
  tmp <- tempfile()
  if(length(dim(clArray))==2)
    writeJPEG(clArray, target = tmp)
  else
    writeJPEG(clArray[,,bands], target = tmp)

  plotJPEG(tmp)
}

