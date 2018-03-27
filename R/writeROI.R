#' Write ROI list file
#'
#' This function writes the ROI list file on a disk space.
#' @param ROIList ROI List variable to be written
#' @param roifilepath path to the ROI file
#' @keywords  Write ROI ROIList
#' @export
#' @examples
#'
#' #loading the ROI files from the example directory
#' f <- system.file(package = 'xROI', 'example/ROI/example_DB_0001_roi.csv')
#'
#' #parsing the example ROI file and store in roi
#' roi <- parseROI(f)
#'
#' #write the loaded ROI in the temporary path
#' tempPath <- file.path(tempdir(), 'roi.csv')
#' writeROI(roi, tempPath)
#'
writeROI <- function(ROIList, roifilepath){
  roifilename <- basename(roifilepath)
  roipath <- paste0(dirname(roifilepath), '/')

  updateTime <- Sys.time()
  hdrText <- paste0('#\n# ROI List for ', ROIList$siteName,
                    '\n#',
                    '\n# Site: ', ROIList$siteName,
                    '\n# Veg Type: ', ROIList$vegType,
                    '\n# ROI ID Number: ', sprintf('%04d', ROIList$ID),
                    '\n# Owner: ', ROIList$Owner,
                    '\n# Creation Date: ', ROIList$createDate,
                    '\n# Creation Time: ', ROIList$createTime,
                    '\n# Update Date: ', strftime(updateTime, format = '%Y-%m-%d'),
                    '\n# Update Time: ', strftime(updateTime, format = '%H:%M:%S'),
                    '\n# Description: ', ROIList$Description,
                    '\n#\n')


  bdyText <- 'start_date,start_time,end_date,end_time,maskfile,sample_image\n'

  for(i in 1:length(ROIList$masks)){
    m <- ROIList$masks[[i]]$rasteredMask

    rName <- names(ROIList$masks)[i]

    writeTIFF(m*1 , where = paste0(roipath, rName,'.tif'))

    maskpoints <- ROIList$masks[[i]]$maskpoints
    maskpoints <- rbind(dim(m), maskpoints)
    if(nrow(maskpoints)>3)
      write.table(maskpoints, file = paste0(roipath, rName,'_vector.csv'), col.names = F, row.names = F, sep = ',')

    bdyLine <- paste(
      ROIList$masks[[i]]$startdate,
      ROIList$masks[[i]]$starttime,
      ROIList$masks[[i]]$enddate,
      ROIList$masks[[i]]$endtime,
      paste0(rName,'.tif'),
      ROIList$masks[[i]]$sampleImage, sep = ',')


    bdyText <- paste0(bdyText, bdyLine, '\n')
  }
  allText <- paste0(hdrText, bdyText)
  writeLines(allText, paste0(roipath, roifilename))
}


