#' Extract chromatic coefficients and their statistics for an array of JPEG files
#'
#' This function apples a list of mask matrices to a vector of jpeg images and extract statstical metrics for each chromatic coordinate on R, G and B.
#' @param paths a vector of character strings, paths to the JPEG files
#' @param rmskList a list, rasters of mask as list
#' @param mIndex a numeric vector, a vector of integer numbers as the index of mask files, same length as paths. This vector shows which mask should be used with which JPEG file.
#' @return This function returns statistical metrics for each color channel. The function returns NULL, if dimensions do not agree.
#' @keywords  exract chromatic coordinates rcc gcc bcc
#' @export
#' @rawNamespace import(raster, except = quantile)
#' @import rgdal
#' @import sp
#' @import jpeg
#' @import tiff
#' @importFrom stats approx na.omit sd quantile
#' @import shiny
#' @rawNamespace import(shinyjs, except = c(runExample, updateColourInput, reset, colourPicker, colourInput, show, click, removeClass))
#'
extractCCCTimeSeries <- function(rmskList, mIndex, paths){
  session <- shiny::getDefaultReactiveDomain()

  continue = TRUE

  mlist <- list()

  for(i in 1:length(rmskList)){
    mi <- as.vector(1-as.matrix(rmskList[[i]]))
    mi[mi==0] <- NA
    mlist[[i]] <- mi
  }

  n <- length(paths)
  CCCT <- matrix(NA, nrow=n, ncol=33)

  # if(exists('session'))
  withProgress(value = 0, message = 'Extracting CCs',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break
                 m <- mlist[[mIndex[i]]]
                 # printLog(paste(i, mIndex[i], sum(m, na.rm = T)))
                 tbl <- extractCCC(paths[i],
                                   cbind(m, m, m))
                 if(!is.null(tbl))
                   CCCT[i,] <- c( tbl$RGB,
                                  tbl$cc,
                                  tbl$std,
                                  tbl$q5, tbl$q10, tbl$q25, tbl$q50, tbl$q75, tbl$q90, tbl$q95,
                                  # tbl$skewness,
                                  # tbl$kurtosis,
                                  tbl$brightness[1], tbl$darkness[1], tbl$contrast[1])
                 incProgress(1/n)
                 # Sys.sleep(1)
                 # if(i%%20==0)httpuv:::service()
               }
  )

  CCCT <- as.data.table(CCCT)

  colnames(CCCT) <- c('red', 'green', 'blue',
                      'rcc','gcc','bcc',
                      'rcc.std','gcc.std','bcc.std',
                      'rcc05', 'gcc05', 'bcc05',
                      'rcc10', 'gcc10', 'bcc10',
                      'rcc25', 'gcc25', 'bcc25',
                      'rcc50', 'gcc50', 'bcc50',
                      'rcc75', 'gcc75', 'bcc75',
                      'rcc90', 'gcc90', 'bcc90',
                      'rcc95', 'gcc95', 'bcc95',
                      # 'r.skewness','g.skewness','b.skewness',
                      # 'r.kurtosis','g.kurtosis','b.kurtosis',
                      'brightness','darkness','contrast'
  )
  CCCT[,grR:=gcc/rcc]
  CCCT[,rbR:=rcc/bcc]
  CCCT[,gbR:=gcc/bcc]
  CCCT[,GRVI:=(gcc - rcc)/(gcc - rcc)]
  CCCT[, exG:=(2*green - red - blue)]
  CCCT
}
