#' Interactive drawing of a polygon
#'
#' This function provides an interactive tool for drawing of polygons by user clicks on the plot
#' @param col color value of the polygon polygon
#' @param lty lty variable as line type
#' @param ... passing graphical parameters tp
#' @keywords  plot polygon interactive
#' @export
#' @importFrom graphics abline axis locator mtext par polygon rasterImage rect
#' @examples
#'
#' #user can click to add vertices, pressing the Escape key would end it.
#' if(interactive()){
#'    drawPolygon()
#' }
#'
#
drawPolygon <- function (col = "#80303080", lty = 1, ...)
  {
    xy <- locator(2)
    lines(xy$x, xy$y, lty = lty)

    while(is.list(c1 <- locator(1))) {
      xy$x <- c(xy$x, c1$x)
      xy$y <- c(xy$y, c1$y)
      lines(xy$x, xy$y, lty = lty)
    }
    xy <- data.frame(xy)
    xy <- rbind(xy, xy[1, ])
    polygon(xy$x, xy$y, lty = lty, col = col, ...)

    invisible(xy)
  }


