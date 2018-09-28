#' Parse Phenocam filenames
#'
#' This function parse filename to extract sitename, date and timing of the images based on the phenocam naming convention.
#' @param filepaths a character vector of filenames
#' @return a datatable containing filenames, with site name, date and timing
#' @keywords  Parse Filename
#' @export
#' @importFrom data.table data.table as.data.table
#' @importFrom lubridate date yday
#'
parsePhenocamFilenames <- function(filepaths)
{
  filenames <- gsub('.jpg', '', basename(filepaths))
  werr <- grepl('.err', filenames, fixed = TRUE)

  lsplit <- lapply(filenames, function(x){strsplit(x, split = '_')[[1]]})
  len <- lapply(lsplit, length)
  wIR <- (grepl('_IR_', filenames))

  wfilter <- (!werr)&(len==5)&(!wIR)

  imgDT <- cbind(filepaths[wfilter],filenames[wfilter], matrix(unlist(lsplit[wfilter]), ncol = 5, byrow = TRUE))
  colnames(imgDT) <- c('filepaths', 'filenames', 'Site', 'Year', 'Month','Day','HHMMSS')
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
