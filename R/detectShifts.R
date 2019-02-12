#' Detect FOV shift
#'
#' This function calculates day-to-day similarity of images based on the CLI file.
#' @param cli_path a character string, path to the CLI file
#' @return a data.frame with two columns containing day-to-day correlations of the brightness and blue bands
#' @keywords  FOV shift
#' @import jpeg
#' @importFrom stats cor smooth
#' @export
#' @examples
#'
#' cli_path <- system.file(package = 'xROI', 'archboldbahia-cli.jpg')
#'
#' cor_mat <- detectShifts(cli_path)
#'
#' plot(cor_mat$brightness.cor)
#' plot(cor_mat$blue.cor)
#'
detectShifts <- function(cli_path){

cli <- readJPEG(cli_path)

bright <- apply(cli, 1:2, max)
blue <- cli[,,3]

n <- ncol(bright)

c1 <- c()
for(i in 2:n)  c1 <- c(c1, cor(smooth(bright[,i]),
                               smooth(bright[,i-1])))

c2 <- c()
for(i in 2:n)  c2 <- c(c2, cor(smooth(blue[,i]),
                               smooth(blue[,i-1])))

data.frame(brightness.cor = c1,
           blue.cor = c2)
}


