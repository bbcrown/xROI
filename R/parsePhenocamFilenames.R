#' Parse Phenocam filenames
#'
#' This function parse filename to extract sitename, date and timing of the images based on the phenocam naming convention.
#' @param filenames a character vector of filenames
#' @return a datatable containing filenames, with site name, date and timing
#' @keywords  Parse Filename
#' @export
#' @importFrom data.table data.table as.data.table
#' @importFrom lubridate date yday
#'
parsePhenocamFilenames <- function(filenames)
{
  filenames <- gsub('.jpg', '', basename(filenames))
  werr <- grepl('.err', filenames, fixed = TRUE)
  filenames <- filenames[!werr]

  lsplit <- lapply(filenames, function(x){strsplit(x, split = '_')[[1]]})
  len <- lapply(lsplit, length)

  imgDT <- cbind(filenames[len==5], matrix(unlist(lsplit[len==5]), ncol = 5, byrow = TRUE))
  colnames(imgDT) <- c('filenames', 'Site', 'Year', 'Month','Day','HHMMSS')
  imgDT <- data.table(imgDT)

  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  imgDT
}
