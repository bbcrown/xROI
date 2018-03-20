#' Extract chromatic coefficients and their statistics for an array of JPEG files
#'
#' This function apply a list of mask matrices to a vector of jpeg images and extract statstical metrics for each chromatic coefficients on R, G and B.
#' @param paths paths to the JPEG files
#' @param rmskList rasters of mask as list
#' @param mIndex A vector of integer numbers as the index of mask files, same length as paths. This vector shows which mask should be used with which JPEG file.
#' @return This function returns statistical metrics for each color channel. The function returns NULL, if dimensions do not agree.
#' @keywords  exract chromatic coefficients rcc gcc bcc
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
  CCCT <- matrix(NA, nrow=n, ncol=24)


  # if(exists('session'))
  withProgress(value = 0, message = 'Extracting CCs',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break
                 m <- mlist[[mIndex[i]]]
                 # printLog(paste(i, mIndex[i], sum(m, na.rm = T)))
                 tbl <- extractCCC(paths[i],
                                   cbind(m, m, m))
                 if(!is.null(tbl))
                   CCCT[i,] <- c(tbl$cc, tbl$std,
                                 tbl$q2.5, tbl$q25, tbl$q50, tbl$q75, tbl$q975,
                                 # tbl$skewness, tbl$kurtosis,
                                 tbl$brightness[1], tbl$darkness[1], tbl$contrast[1])
                 incProgress(1/n)
                 # Sys.sleep(1)
                 # if(i%%20==0)httpuv:::service()
               }
  )
  CCCT <- as.data.table(CCCT)
  colnames(CCCT) <- c('red','green','blue',
                      # 'r.mean','g.mean','b.mean',
                      'r.std','g.std','b.std',
                      'r2.5', 'g2.5', 'b2.5',
                      'r25', 'g25', 'b25',
                      'r50', 'g50', 'b50',
                      'r75', 'g75', 'b75',
                      'r975', 'g975', 'b975',
                      # 'r.skewness','g.skewness','b.skewness',
                      # 'r.kurtosis','g.kurtosis','b.kurtosis',
                      'brightness','darkness','contrast'
  )
  CCCT
}
