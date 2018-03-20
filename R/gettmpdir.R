#' Path to the TEMP directory
#'
#' This function returns the path to the TEMP directory
#'
#' @keywords  temporary directory
#' @export
#' @examples
#' p <- gettmpdir()
#'
gettmpdir <- function() {
  if (.Platform$OS.type == 'windows')
    Sys.getenv('R_USER')
  else
    '/tmp'
}
