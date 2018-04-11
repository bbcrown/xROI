#' Center line column of an image
#'
#' This function returns the R,G,B vectors extracted from the center-line of a JPEG file
#' @param file a character string, path to the JPEF file
#' @return a three-column matrix of red, green blue bands of the center line
#' @keywords  Center-line image CLI
#' @export
#' @import jpeg
#' @examples
#'
#' f <- system.file(package = 'xROI', 'dukehw.jpg')
#' cli <- getCL(f)
#'
getCL <- function(file){
  jp <- try(readJPEG(file, native=F), silent = T)

  if(class(jp)=="try-error") return(NA)

  res <-  dim(jp)[1:2] # get the resolution
  center <- floor(res/2)

  centerLine <- jp[1:res[1], center[2],]

  # list(res=res, centerLine=centerLine)
  centerLine
}

