#' Rasterize ROI Polygons
#'
#' This function convert point-based polygons to raster format
#' @param pnts a two column matrix of points as relative x and y values (0 to 1)
#' @param imgSize size of the final raster
#' @keywords  Rasterize ROI Polygons
#' @export
#' @examples
#'
#' pnts <- matrix(c(0.1, 0.2,
#'                  0.1, 0.4,
#'                  0.5, 0.4,
#'                  0.5, 0.2),
#'                  4, 2, byrow= TRUE)
#' imgSize <- c(300, 400)
#' m <- rasterizeROI(pnts, imgSize)
#' xROI::addMask(m, add = FALSE)
#'
rasterizeROI <- function(pnts, imgSize){

  pnts <- t(apply(pnts, 1, '*', imgSize))
  ext <- extent(1, imgSize[1], 1, imgSize[2])
  poly <- as(ext  ,"SpatialPolygons")
  # poly@polygons[[1]]@Polygons[[1]]@coords <- as.matrix(pnts)

  tbl <- as.data.table(na.omit(cbind(pnts,cumsum(is.na(pnts[,1]))+1 )))
  colnames(tbl) <- c('x', 'y', 'g')
  ng <- table(tbl$g)

  polyList <- list()
  np <- length(ng[which(ng>=3)])

  for(gi in 1:np)
    polyList[[gi]] <- as.matrix(tbl[g==gi, .(x,y)])

  polys <- SpatialPolygons(
    lapply(1:np,
           function(x){
             p <- slot(poly@polygons[[1]], "Polygons")[[1]]
             slot(p, "coords") <- polyList[[x]]
             pp <- Polygons(list(p), ID = x)
             return(pp)
           })
  )

  r <- rasterize(polys, raster(ext, nrow = imgSize[2], ncol = imgSize[1]))
  r[!is.na(r)] <- 1

  m1 <- as.matrix(r)
  m <- m1
  m[m1==0|is.na(m1)] <- 1
  m[m1!=0] <- 0
  m
}







