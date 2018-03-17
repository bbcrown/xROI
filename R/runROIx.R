#' Launch ROIx app
#'
#' This function launch the app by opening the default web browser.
#' @param inputDir path to the input directory as a character string.
#' @keywords  Run Launch ROIx App
#' @export
#' @import shiny
#' @import shinythemes
#' @import plotly
#' @import raster
#' @import rgdal
#' @import sp
#' @import jpeg
#' @import tiff
#' @import shinyBS
#' @import shinyAce
#' @import shinyTime
#' @import shinyFiles
#' @import shinydashboard
#' @import colourpicker
#' @import rjson
#' @import stringr
#' @import data.table
#' @import lubridate
#' @import moments
#' @examples
#'
#' #Launch ROIx app
#' runROIx()
#'
runROIx <- function(inputDir='.'){
  data('example')
  tmpfile <- tempfile(fileext = '.zip')
  writeBin(example, tmpfile)
  exdir <- paste0(gettmpdir())
  unzip(zipfile = tmpfile, exdir = exdir)
  FOLDERPATH = exdir

  data("uiserver")
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
