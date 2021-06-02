globalVariables(names = c('Year','Month', 'Day', 'Hour', 'Minute', 'Second',
                          'HHMMSS','DOY', 'Date', 'DateTime', 'YearDOY',
                          'ID', 'Label', 'x', 'y', 'z', '.', 'archive',
                          'mainDataPath', 'middayListPath', 'newpath', 'path', 'filenames',
                          'band', 'blackness',  'conT', 'dukehw', 'empty',
                          'GRVI', 'exG',
                          'gbR', 'grR', 'rbR',
                          'red', 'green', 'blue',
                          'bcc', 'bcc05', 'bcc10', 'bcc25', 'bcc75', 'bcc90', 'bcc95',
                          'gcc', 'gcc05', 'gcc10', 'gcc25', 'gcc75', 'gcc90', 'gcc95',
                          'rcc', 'rcc05', 'rcc10', 'rcc25', 'rcc75', 'rcc90', 'rcc95'))



.onAttach <- function(libname, pkgname) {
  citation =
  'To cite xROI in publications, please cite both the paper and the R package:

  Bijan Seyednasrollah, Thomas Milliman, Andrew D. Richardson. "Data extraction from
  digital repeat photography using xROI: An interactive framework to facilitate the
  process"", ISPRS Journal of Photogrammetry and Remote Sensing 152 (2019): 132-144.

  Bijan Seyednasrollah, Thomas Milliman, Andrew D. Richardson (2018). "xROI: A Toolkit to
  Delineate Region of Interests (ROIs) and Extract Time-series Data from Digital Repeat
  Photography Images DOI:10.5281/zenodo.1202273
  '

  packageStartupMessage(citation)
  }
