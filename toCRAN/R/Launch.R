#' Launch xROI app
#'
#' This function launch the app by opening the default web browser.
#' @param inputDir path to the input directory as a character string.
#' @keywords  Run Launch xROI App
#' @export
#' @import shiny
#' @rawNamespace import(shinyjs, except = c(runExample, updateColourInput, reset, colourPicker, colourInput, show, click, removeClass))
#' @import shinythemes
#' @rawNamespace import(plotly, except = select)
#' @rawNamespace import(raster, except = quantile)
#' @import rgdal
#' @import sp
#' @import jpeg
#' @import tiff
#' @import shinyBS
#' @import shinyAce
#' @import shinyTime
#' @import shinyFiles
#' @import shinydashboard
#' @rawNamespace import(colourpicker, except = runExample)
#' @import rjson
#' @import RCurl
#' @import stringr
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek, shift))
#' @rawNamespace import(lubridate, except = origin)
#' @import moments
#' @importFrom grDevices rgb2hsv
#' @importFrom graphics abline axis locator mtext par polygon rasterImage rect
#' @import methods
#' @importFrom stats approx na.omit sd quantile
#' @importFrom utils data download.file read.csv read.table setTxtProgressBar txtProgressBar unzip write.table zip
#'
#' @examples
#'
#' #Launch xROI app
#' xROI::Launch()
#'
#'
Launch <- function(inputDir= NULL){
  exampleDir <- system.file('example', package = "xROI")
  tmpdir <- tempdir()
  file.copy(exampleDir, tmpdir, recursive = T)
  exampleDir <- file.path(tmpdir, 'example')
  ## Only run examples in interactive R sessions
  if (interactive()) {
    ui <- getUI()
    server <- getServer(exampleDir, inputDir)
    shinyApp(ui, server, options = list(launch.browser = TRUE))
  }else{
    print('This function requires an interactive R session!')
  }
}

