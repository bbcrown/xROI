#' Parse ROI list file
#'
#' This function reads the ROI list file and returns it as a list variable
#' @param roifilepath path to the ROI file
#' @keywords  ROI ROIList
#' @export
#' @examples
#'
#' f <- system.file(package = 'xROI', 'example/ROI/example_DB_0001_roi.csv')
#' roi <- parseROI(f)
#'
parseROI <- function(roifilepath){
  roifilename <- basename(roifilepath)
  roipath <- paste0(dirname(roifilepath), '/')
  fname <- paste0(roipath, roifilename)
  #if(!file.exists(fname)) return(NULL)

  roilines <- readLines(fname)

  wEmptyLine <- roilines%in%c('', ' ',  '  ')
  wCommented <- as.vector(sapply(roilines, grepl,  pattern = '^#'))
  wNotSkip <- !(wEmptyLine|wCommented)


  parseroiline <- function(roilines, property){
    wProp <- grepl(roilines, pattern = property)
    gsub(roilines[wProp], pattern = paste0('# ', property, ': '), replacement = '')
  }

  ROIList <- list(siteName = parseroiline(roilines[wCommented], 'Site'),
                  vegType = parseroiline(roilines[wCommented], 'Veg Type'),
                  ID = as.numeric(parseroiline(roilines[wCommented], 'ROI ID Number')),
                  Owner = parseroiline(roilines[wCommented], 'Owner'),
                  createDate = parseroiline(roilines[wCommented], 'Creation Date'),
                  createTime = parseroiline(roilines[wCommented], 'Creation Time'),
                  updateDate = parseroiline(roilines[wCommented], 'Update Date'),
                  updateTime = parseroiline(roilines[wCommented], 'Update Time'),
                  Description = parseroiline(roilines[wCommented], 'Description'),
                  masks = NULL)


  parsedMasks <- read.table(textConnection(roilines[which(wNotSkip)]), sep = ',', header = T)

  masksList <- list()
  for(i in 1:nrow(parsedMasks)){
    maskpath <- paste0(roipath, parsedMasks$maskfile[i])
    maskpointspath <- gsub(maskpath, pattern = '.tif', replacement = '_vector.csv')
    if(file.exists(maskpointspath)) {
      dummy=0
      maskpoints <- as.matrix(read.csv(maskpointspath, header = F, skip = 1))
    }else{
      maskpoints <- NULL
    }


    tmpMask <- list(maskpoints = maskpoints,
                    startdate = as.character(parsedMasks$start_date[i]),
                    enddate = as.character(parsedMasks$end_date[i]),
                    starttime = as.character(parsedMasks$start_time[i]),
                    endtime = as.character(parsedMasks$end_time[i]),
                    # sampleyear = NULL,
                    # sampleday = NULL,
                    sampleImage = as.character(parsedMasks$sample_image[i]),
                    rasteredMask = as.matrix(raster(maskpath)))

    tmpMask$rasteredMask[(!is.na(tmpMask$rasteredMask))&tmpMask$rasteredMask!=0] <- 1


    sampleYMD <- strsplit(tmpMask$sampleImage, split = '_')[[1]][2:4]

    masksList[[length(masksList)+1]] <- tmpMask

  }
  names(masksList) <- gsub(parsedMasks$maskfile, pattern = '.tif', replacement = '')
  ROIList$masks <- masksList
  ROIList
}
