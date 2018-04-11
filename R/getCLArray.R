#' Center line arrary of an array of image
#'
#' This function returns CLI array for vector of JPEG files
#' @param files a vector of character strings, paths to the JPEF files
#' @return A 3D array. The center line image as an array (NxHx3), where N is number of files, and H is the height of an image in pixels.
#' @keywords  Center-line image CLI
#' @export
#' @import jpeg
#' @import shiny
#' @importFrom utils data download.file read.csv read.table setTxtProgressBar txtProgressBar unzip write.table zip
#'
#' @examples
#'
#' f <- system.file(package = 'xROI', 'dukehw.jpg')
#' cli <- getCL(f)
#'
getCLArray <- function(files){
  session=shiny::getDefaultReactiveDomain()
  continue = TRUE

  first <- which(!is.na(files))[1]
  res <-  dim(readJPEG(files[first], native=F))[1:2] # get the resolution
  n <- length(files)

  clArray <- array(data = NA, dim = c(res[1], n ,3))

  pb <- txtProgressBar(1, n, style = 3)


  withProgress(value = 0, message = 'Extracting CLI',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break

                 if(is.na(files[i]))
                   cl <- matrix(0, res[1], 3)
                 else
                   cl <- getCL(files[i])

                 if(is.na(cl)) cl <- matrix(0, res[1], 3)
                 r <- cl[,1]
                 g <- cl[,2]
                 b <- cl[,3]

                 if(nrow(cl)!=res[1]){
                   message(paste('wrong dimensions at', i, files[i], 'resampling ...'))
                   x1 <- (0:(nrow(cl)-1))/nrow(cl)
                   x2 <- (0:(res[1]-1))/res[1]
                   r <- approx(x1, r, x2)$y
                   g <- approx(x1, g, x2)$y
                   b <- approx(x1, b, x2)$y
                   r[is.na(r)] <- 0
                   g[is.na(g)] <- 0
                   b[is.na(b)] <- 0
                 }

                 clArray[,i,1] <- r
                 clArray[,i,2] <- g
                 clArray[,i,3] <- b

                 setTxtProgressBar(pb, i)

                 incProgress(1/n)
                 # Sys.sleep(1)
                 # if(i%%20==0)httpuv:::service()
               }
  )
  clArray
}

