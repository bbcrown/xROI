#' Extract chromatic coefficients and their statistics
#'
#' This function apply a mask matrix to a jpeg image and extract statstical metrics for each chromatic coefficients on R, G and B.
#' @param path path to the JPEG file
#' @param m mask binary matrix (1 for selected)
#' @return The function returns statistical metrics for each color channel. The function returns NULL, if dimensions do not agree.
#' @keywords  exract chromatic coefficients rcc gcc bcc
#' @export
#' @rawNamespace import(raster, except = quantile)
#' @import rgdal
#' @import sp
#' @import jpeg
#' @import tiff
#' @importFrom stats approx na.omit sd quantile
#' @examples
#'
#' m <- tiff::readTIFF(system.file(package = 'xROI', 'dukehw-mask.tif'))
#' jpgFile <- system.file(package = 'xROI', 'dukehw.jpg')
#' cc <- extractCCC(jpgFile, m)
#
# extract chromatic colors of RGB channels for given jpeg file and mask matrix
extractCCC <- function(path, m){
  path
  jp <- readJPEG(path)
  dm <- dim(jp)
  rgb <- jp
  dim(rgb) <- c(dm[1]*dm[2],3)

  if(!identical(dim(rgb), dim(m))) return(NULL)

  mrgb <- na.omit(rgb*m)
  t <- rowSums(mrgb)
  ccMat <- apply(mrgb, 2, '/', t)

  ccMat <- na.omit(ccMat)
  cc <- colMeans(ccMat)
  cc <- cc/sum(cc)

  tbl <- as.data.frame(t(apply(ccMat, 2, quantile, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))))
  rownames(tbl) <- c('r','g','b')
  colnames(tbl) <- c('min','q2.5','q25','q50','q75','q975','max')

  tbl$cc <- cc
  # tbl$mean <- colMeans(ccMat)
  tbl$std <- apply(ccMat, 2, sd)
  # tbl$skewness <- apply(ccMat, 2, skewness)
  # tbl$kurtosis <- apply(ccMat, 2, kurtosis)
  tbl$brightness <- mean(apply(mrgb, 2, max))
  tbl$darkness <- mean(apply(mrgb, 2, min))
  tbl$contrast <- tbl$brightness - tbl$darkness
  tbl
}


