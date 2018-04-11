#' Server function
#'
#' This funciton constructs the server functions of the shiny app.
#'
#' @param exdir path to the example directory
#' @param inputDir a character string. the path to the input directory
#' @return the shiny server object
#' @keywords  Server Shiny App
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
#' exampleDir <- system.file('example', package = "xROI")
#' server <- getServer(exampleDir)
#'
getServer <- function(exdir, inputDir = NULL){
  return(function(input, output, session) {

    options(warn = -1)
    rv <- reactiveValues(centers = matrix(numeric(), 0, 2),
                         MASKs = list(),
                         slideShow = 0,
                         filetbl = NULL,
                         # filelist = '.',
                         folderpath = exdir, #paste0(gettmpdir(), '/example'), #'./example',
                         # withDate = FALSE,
                         # roots = list('Working directory'='.', Home='~', root='/'),
                         imgs = NULL,
                         cli = NULL,
                         cliclickID = NULL)

    roots = list('Example'= exdir, Home='~', root='/')
    observe({
      if(!is.null(inputDir)) rv$folderpath <- inputDir
    })
    # ----------------------------------------------------------------------
    # Input directory
    # ----------------------------------------------------------------------
    shinyFileChoose(input, 'filelist', roots = roots, filetypes=c('', 'txt', 'csv'))
    shinyDirChoose(input, 'folderpath', roots = roots)

    observe({
      if(is.null(input$folderpath)) return()
      rv$folderpath <- parseDirPath(roots, selection = input$folderpath)
    })

    output$folderpath <- renderUI({
      basename(rv$folderpath)
    })

    observeEvent(rv$folderpath,{
      dummy <- 0
      dir.create(roipath())
      rv$imgs <- dir(rv$folderpath, pattern = '*.jpg', full.names = T)

      f <- paste(rv$folderpath, 'filelist.csv', sep = '/')

      if(!file.exists(f)){
        showModal(strong(modalDialog(paste(f, ' was not found!'),
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer =  modalButton("OK"),
                                     easyClose = F, size = 's')))
        return()
      }

      rv$filelist <- f

      tbl <- try(read.csv(rv$filelist, colClasses = c('character', rep('integer', 6)), header = F), silent = T)

      if(class(tbl)=='try-error'){
        showModal(strong(modalDialog(as.character(tbl),
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer = NULL,
                                     easyClose = T,
                                     size = 'm')))
        return()
      }

      if(nrow(tbl)!=length(rv$imgs)){
        showModal(strong(modalDialog('# of rows in filelist.csv not match with number of files!',
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer =  modalButton("OK"),
                                     easyClose = F, size = 's')))
        return()
      }

      if(ncol(tbl)!=7){
        showModal(strong(modalDialog('filelist.csv should have 7 columns: filename, year, month, day, hour, minute, second',
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer =  modalButton("OK"),
                                     easyClose = F, size = 'm')))
        return()
      }
      colnames(tbl) <- c('filename', 'Year','Month','Day','Hour','Minute','Second')
      rv$filetbl <- as.data.table(tbl)
      rv$filetbl[,path:=paste0(rv$folderpath, '/',filename)]
      rv$filetbl[,DateTime:=as.POSIXct(paste(Year, Month, Day, Hour, Minute, Second), format='%Y %m %d %H %M %S')]
      rv$filetbl <- rv$filetbl[order(DateTime),]
      rv$filetbl[,ID:=1:.N]

      updateDateInput(session, 'maskStartDate', value = strftime(rv$filetbl$DateTime[1], format="%Y-%m-%d"))
      updateTextInput(session, inputId = 'maskStartTime', value = strftime(rv$filetbl$DateTime[1], format="%H:%M:%S"))

      updateDateInput(session, 'maskEndDate', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%Y-%m-%d"))
      updateTextInput(session, inputId = 'maskEndTime', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%H:%M:%S"))

      updateCheckboxInput(session, inputId = 'openEnd', value = T)

      rv$cli <- NULL
      rv$slideShow <- 0
      rv$centers <- matrix(numeric(), 0, 2)

      rv$MASKs <- list()
      updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
      rv$contID <- 1
      updateNumericInput(session, 'roiID', value = 1)
      updateSelectInput(session, 'vegType', selected = 'RF')
      updateTextInput(session, 'siteName', value = basename(rv$folderpath))
      updateTextInput(session, 'siteDescription', value = '')
    })



    # ----------------------------------------------------------------------
    # Image List
    # ----------------------------------------------------------------------
    observeEvent(input$vegType,{
      rv$slideShow <- 0
      if(length(rv$MASKs)==0) return()

      maskNames <- names(rv$MASKs)
      f <- function(x, y){
        z <- unlist(strsplit(x, '_'))
        paste(c(z[1], y, z[3:4]), collapse = '_')
      }

      newmaskNames <- as.vector(sapply(maskNames, f, y = input$vegType))
      if(!is.null(rv$MASKs))names(rv$MASKs) <- newmaskNames
      updateSelectInput(session, inputId = 'maskName', choices = c(names(rv$MASKs), 'New mask'))
    })

    # ----------------------------------------------------------------------
    # Image List
    # ----------------------------------------------------------------------

    imgList <- reactive({
      if(is.null(rv$filetbl)) return(character(0))
      if(nrow(rv$filetbl)==0) return(character(0))
      return(rv$filetbl$path)
    })

    imgT <- reactive({
      if(is.null(rv$filetbl)) return(character(0))
      if(nrow(rv$filetbl)==0) return(character(0))
      # t <- if(rv$withDate)rv$filetbl$DateTime else rv$filetbl$ID
      t <- rv$filetbl$DateTime
      return(t)
    })


    nimgList <- reactive(
      return(length(imgList()))
    )

    observe({
      if(nimgList()==0) updateSliderInput(session, inputId = 'contID', min = 1, max = 1)
      updateSliderInput(session, inputId = 'contID', min = 1, max = nimgList())
    })

    sampleImage <- reactive({
      imgList()[as.numeric(input$contID)]
    }  )

    sampleImageSize <- reactive({
      smpl <- sampleImage()
      dim(readJPEG(smpl))[2:1]
    })


    output$sampleImagePath <- renderText(

      basename(sampleImage())
    )
    # ----------------------------------------------------------------------
    # Plot image
    # ----------------------------------------------------------------------
    output$imagePlot2 <- renderImage(
      {
        list(src = sampleImage(),
             height = floor(session$clientData$output_imagePlot_width/1.35),
             res=96,
             alt = sampleImage())
      }
    )

    output$imagePlot <- renderPlot(
      res=96,
      height = function(){floor(session$clientData$output_imagePlot_width/1.35)},
      {
        par(mar=c(0,0,0,0))
        if(nimgList()==0){
          plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
          text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No image was found!', font=2, adj=.5)
        }else if(!file.exists(sampleImage())) {
          plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
          text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No image was found!', font=2, adj=.5)
        }else{
          dummy <- 0

          jp <- plotJPEG(sampleImage())

          if(is.null(rv$centers))
            absPoints <- matrix(numeric(), 0, 2)
          else if(nrow(rv$centers)==0)
            absPoints <- matrix(numeric(), 0, 2)
          else if(nrow(rv$centers)==1)
            absPoints <- rv$centers*sampleImageSize()
          else
            absPoints <- t(apply(rv$centers, 1, '*', sampleImageSize()))
          dummy <- 0
          polygon(absPoints, pch = 9, lwd=3, border = input$roiColors)
          mm <- curMask()
          if(!is.null(mm)&input$showMask)addMask(mm, col = input$roiColors)
        }
      })

    # ----------------------------------------------------------------------
    # Slideshow
    # ----------------------------------------------------------------------

    observeEvent(input$pause, {
      rv$slideShow <- 0
    })

    observeEvent(input$play, {
      if(rv$slideShow==0) rv$slideShow <- 1
      if(rv$slideShow==-1) rv$slideShow <- 0
    })

    observeEvent(input$backplay, {
      if(rv$slideShow==0) rv$slideShow <- -1
      if(rv$slideShow==1) rv$slideShow <- 0
    })

    observeEvent(input$back, {
      rv$slideShow <- 0
      nextID <- as.numeric(input$contID) - 1
      if(nextID > nimgList()) nextID <- 1
      if(nextID == 0) nextID <- nimgList()

      updateSliderInput(session, "contID", value = nextID)
    })


    observeEvent(input$forw, {
      rv$slideShow <- 0
      nextID <- as.numeric(input$contID) + 1
      if(nextID > nimgList()) nextID <- 1
      if(nextID == 0) nextID <- nimgList()

      updateSliderInput(session, "contID", value = nextID)
    })

    observe({
      if(rv$slideShow==0) return()
      nextID <- as.numeric(input$contID) + rv$slideShow
      if(nextID > nimgList()) nextID <- 1
      if(nextID < 1) nextID <- nimgList()
      updateSliderInput(session, inputId = 'contID',value = nextID)
    })



    # ----------------------------------------------------------------------
    # New point
    # ----------------------------------------------------------------------

    observeEvent(input$newPoint, {
      rv$slideShow <- 0
      newPoint <- matrix(c(input$newPoint$x/input$newPoint$domain$right,
                           input$newPoint$y/input$newPoint$domain$top),1, 2)
      rv$centers <- rbind(rv$centers, newPoint)
    })


    observeEvent(input$gapPoint, {
      n <- nrow(rv$centers)
      if(n<3) return()
      rv$slideShow <- 0
      pnts <- rv$centers
      tbl <- as.data.table(na.omit(cbind(pnts,cumsum(is.na(pnts[,1]))+1 )))
      colnames(tbl) <- c('x', 'y', 'g')
      tbln <- table(tbl$g)
      if(tbln[length(tbln)]<3) return()
      newPoint <- matrix(c(NA, NA),1, 2)
      if(!is.na(rv$centers[n,1]))
        rv$centers <- rbind(rv$centers, newPoint)
    })



    # ----------------------------------------------------------------------
    # Edit Canvas
    # ----------------------------------------------------------------------

    observeEvent(input$clearCanvas, {
      rv$slideShow <- 0
      rv$centers <- matrix(numeric(), 0, 2)
    })


    observeEvent(input$undoCanvas, {
      rv$slideShow <- 0
      if (nrow(rv$centers) > 2)
        rv$centers <- rv$centers[-nrow(rv$centers),]
      else if (nrow(rv$centers) == 2)
        rv$centers <- matrix(rv$centers[1,], 1, 2)
      else if (nrow(rv$centers) == 1)
        rv$centers <- matrix(numeric(), 0, 2)
    })


    # ----------------------------------------------------------------------
    # Accept canvas
    # ----------------------------------------------------------------------
    observeEvent(input$acceptCanvas,{
      rv$slideShow <- 0
      if(is.null(rv$centers)) {
        showModal(strong(modalDialog('First draw a polgon by clicking on the image!',
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer = NULL, easyClose = T, size = 'm')))
        return()
      }
      if (nrow(rv$centers)<3) {
        showModal(strong(modalDialog('At least 3 points are required to create a polygon!',
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer = NULL, easyClose = T, size = 'm')))
        return()
      }

      if(input$maskName=='New mask'){
        showModal(strong(modalDialog("Raster is being produced ...",
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     easyClose = F,
                                     size = 's',
                                     footer = NULL
        )))
        newMask <- list(maskpoints = rv$centers,
                        startdate = input$maskStartDate,
                        enddate = input$maskEndDate,
                        starttime = input$maskStartTime,
                        endtime = input$maskEndTime,
                        sampleImage = basename(sampleImage()),
                        rasteredMask = rasterizeROI(rv$centers, sampleImageSize()))

        tmp <- rv$MASKs
        tmp[[length(tmp)+1]] <-  newMask

        maskID <- length(rv$MASKs) + 1
        # tmpName <- sprintf('Mask.%02d',maskID)
        tmpName <- paste(input$siteName, input$vegType,
                         sprintf('%04d', input$roiID),
                         sprintf('%02d',maskID), sep = '_')

        names(tmp)[length(tmp)] <- tmpName
        rv$MASKs <- tmp
        updateSelectInput(session, inputId = 'maskName', choices = c(names(tmp), 'New mask'), selected = tmpName)

        removeModal()
      }else{
        if(is.null(curMask()))return()

        showModal(strong(modalDialog("Raster is being updated ...",
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     easyClose = F,
                                     size = 's',
                                     footer = NULL
        )))

        newMASK <- rasterizeROI(rv$centers, sampleImageSize())
        tmpMask <- list(maskpoints = rv$centers,
                        startdate = input$maskStartDate,
                        enddate = input$maskEndDate,
                        starttime = input$maskStartTime,
                        endtime = input$maskEndTime,
                        sampleImage = basename(sampleImage()),
                        rasteredMask = newMASK)


        rv$MASKs[[input$maskName]] <- tmpMask

        removeModal()

      }
    })




    # ----------------------------------------------------------------------
    # Mask
    # ----------------------------------------------------------------------
    observeEvent(input$matchStart, {
      id <- as.numeric(input$contID)
      updateDateInput(session, 'maskStartDate', value = strftime(rv$filetbl$DateTime[id], format="%Y-%m-%d"))
      updateTextInput(session, inputId = 'maskStartTime', value = strftime(rv$filetbl$DateTime[id], format="%H:%M:%S"))
    })


    observeEvent(input$matchEnd, {
      updateCheckboxInput(session, 'openEnd', value = F)
      id <- as.numeric(input$contID)
      updateDateInput(session, 'maskEndDate', value = strftime(rv$filetbl$DateTime[id], format="%Y-%m-%d"))
      updateTextInput(session, inputId = 'maskEndTime', value = strftime(rv$filetbl$DateTime[id], format="%H:%M:%S"))
    })


    curMask <- reactive({
      if(input$maskName=='New mask') {
        return(NULL)
      }
      rv$MASKs[[input$maskName]]$rasteredMask
    })

    observeEvent(input$maskName, {
      if(nimgList()==0) return()

      rv$slideShow <- 0
      if(input$maskName=='New mask') {
        updateDateInput(session, 'maskStartDate', value = strftime(rv$filetbl$DateTime[1], format="%Y-%m-%d"))
        updateTextInput(session, inputId = 'maskStartTime', value = strftime(rv$filetbl$DateTime[1], format="%H:%M:%S"))

        updateDateInput(session, 'maskEndDate', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%Y-%m-%d"))
        updateTextInput(session, inputId = 'maskEndTime', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%H:%M:%S"))

        updateCheckboxInput(session, inputId = 'openEnd', value = T)
        return()
      }
      updateCheckboxInput(session, 'openEnd', value = F)
      tmpmask <- rv$MASKs[[input$maskName]]

      rv$centers <- tmpmask$maskpoints
      updateDateInput(session, 'maskStartDate', value = tmpmask$startdate)
      updateDateInput(session, 'maskEndDate', value = tmpmask$enddate)

      updateTextInput(session, inputId = 'maskStartTime', value = tmpmask$starttime)
      updateTextInput(session, inputId = 'maskEndTime', value = tmpmask$endtime)

      w <- which(basename(rv$filetbl$path)==tmpmask$sampleImage)
      if(length(w)!=0) updateSliderInput(session, 'contID', value = w)
    })


    # ----------------------------------------------------------------------
    # Plot mask
    # ----------------------------------------------------------------------
    output$maskPlot <-
      renderPlot(
        res=96,
        height = function(){floor(session$clientData$output_maskPlot_width/1.35)},
        {
          par(mar=c(0,0,0,0))
          # plot(1,
          #      type='n',
          #      xaxs='i',yaxs='i',
          #      xaxt='n',yaxt='n',
          #      xlab='',ylab='',
          #      bty='o')

          if(is.null(curMask())) {
            plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
            text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No mask was generated!', font=2, adj=.5)

            return()
          }
          mask <- curMask()
          res <- dim(mask)[2:1]

          wd <- getwd()
          setwd(tmpDir())

          plot(NA,xlim=c(1,res[1]),ylim=c(1,res[2]), type='n',
               xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
          dummy=0
          writeTIFF(mask*1, '.tmpraster.tif')
          rmask <- raster('.tmpraster.tif')
          rmask[rmask!=0] <- NA

          plot(rmask,legend=F, add=T, col='black')
          file.remove('.tmpraster.tif')
          setwd(wd)
        })




    # ----------------------------------------------------------------------
    # ROI label
    # ----------------------------------------------------------------------
    observeEvent(input$newROI,{
      rv$slideShow <- 0
      rv$centers <- matrix(numeric(), 0, 2)

      rv$MASKs <- list()
      updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
      rv$contID <- 1
      updateNumericInput(session, 'roiID', value = 1)
      updateSelectInput(session, 'vegType', selected = 'RF')
      updateTextInput(session, 'siteDescription', value = '')

      updateDateInput(session, 'maskStartDate', value = strftime(rv$filetbl$DateTime[1], format="%Y-%m-%d"))
      updateTextInput(session, inputId = 'maskStartTime', value = strftime(rv$filetbl$DateTime[1], format="%H:%M:%S"))

      updateDateInput(session, 'maskEndDate', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%Y-%m-%d"))
      updateTextInput(session, inputId = 'maskEndTime', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%H:%M:%S"))

      updateCheckboxInput(session, inputId = 'openEnd', value = T)
    })

    roiLabel <- reactive(
      paste(input$siteName,
            input$vegType,
            sprintf('%04d',input$roiID), sep = '_')  )

    output$roiFileName <- renderText(
      paste0(roiLabel(),'_roi.csv')
    )

    roipath <- reactive({
      paste0(rv$folderpath,'/ROI/')

    }  )

    dirroipath <- reactive({
      dir(roipath(), pattern = 'roi.csv$')
    })

    autoInvalidate <- reactiveTimer(10000)

    observe({
      autoInvalidate()
      tmp.rv.ROIs <- c(dirroipath(), "New ROI")
      if(!identical(rv$ROIs, tmp.rv.ROIs))    {
        rv$ROIs <- tmp.rv.ROIs
        updateSelectInput(session, 'roiName', choices = rv$ROIs)
        updateSelectInput(session, 'roiName', selected = 'New ROI')
      }
    }
    )


    # ----------------------------------------------------------------------
    # Parsed ROI List
    # ----------------------------------------------------------------------
    observeEvent(input$roiName,{
      if(input$roiName=='') return()
      rv$slideShow <- 0
      if(input$roiName=='New ROI') {
        shinyjs::enable('vegType')
        rv$MASKs <- list()
        rv$centers <- matrix(numeric(), 0, 2)
        updateSelectInput(session, inputId = 'maskName', choices = 'New mask')
        updateSelectInput(session, inputId = 'vegType', selected = list('RF'))
        updateSelectInput(session, inputId = 'siteDescription', selected = '')
        updateTextInput(session, inputId = 'roiOwner', value = '')

        updateDateInput(session, 'maskStartDate', value = strftime(rv$filetbl$DateTime[1], format="%Y-%m-%d"))
        updateTextInput(session, inputId = 'maskStartTime', value = strftime(rv$filetbl$DateTime[1], format="%H:%M:%S"))

        updateDateInput(session, 'maskEndDate', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%Y-%m-%d"))
        updateTextInput(session, inputId = 'maskEndTime', value = strftime(rv$filetbl$DateTime[max(rv$filetbl$ID)], format="%H:%M:%S"))

        updateCheckboxInput(session, inputId = 'openEnd', value = T)

        return()
      }
      showModal(strong(
        modalDialog(HTML('Loading ROI files ...'),
                    easyClose = F,
                    size = 's',
                    style='background-color:#3b3a35; color:#fce319; ',
                    footer = NULL
        )))
      shinyjs::disable('vegType')
      ROIList <- parseROI(paste0(roipath(), '/',input$roiName))
      if(is.null(ROIList)) return()
      rv$parsedROIList <- ROIList
      updateTextInput(session, inputId = 'siteName', value =  rv$parsedROIList$siteName)
      updateSelectInput(session, inputId = 'vegType', selected =  rv$parsedROIList$vegType)
      updateTextInput(session, inputId = 'siteDescription', value = rv$parsedROIList$Description)
      updateTextInput(session, inputId = 'roiOwner', value = rv$parsedROIList$Owner)
      updateNumericInput(session, inputId = 'roiID', value = rv$parsedROIList$ID)
      rv$MASKs <- rv$parsedROIList$masks
      updateSelectInput(session, inputId = 'maskName', choices = c(names(rv$MASKs), 'New mask'))

      removeModal()
    })


    # ----------------------------------------------------------------------
    # Save ROI List
    # ----------------------------------------------------------------------
    observeEvent(input$saveROI,{
      rv$slideShow <- 0
      if(length(rv$MASKs)==0) return()
      systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
      ROIList <- list(siteName = input$siteName,
                      vegType = input$vegType,
                      ID = input$roiID,
                      Owner= input$roiOwner,
                      Description = input$siteDescription,
                      createDate = strftime(systime, format = '%Y-%m-%d'),
                      createTime = strftime(systime, format = '%H:%M:%S'),
                      updateDate = strftime(systime, format = '%Y-%m-%d'),
                      updateTime = strftime(systime, format = '%H:%M:%S'),
                      masks = rv$MASKs)

      if(input$roiName!='New ROI'){
        ROIList$createDate <- rv$parsedROIList$createDate
        ROIList$createTime <- rv$parsedROIList$createTime
      }
      dummy <- 0

      roifilename <- paste0(roiLabel(),'_roi.csv')

      dir.create(roipath())
      writeROI(ROIList, paste0(roipath(), '/',roifilename))

      showModal(strong(modalDialog("ROI was saved in the ROI folder!",
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   easyClose = T,
                                   size = 's',
                                   footer = NULL
      )))

      tmp.rv.ROIs <- c(dirroipath(), "New ROI")
      if(!identical(rv$ROIs, tmp.rv.ROIs)) rv$ROIs <- tmp.rv.ROIs
      updateSelectInput(session, inputId = 'roiName', choices = rv$ROIs)
      updateSelectInput(session, inputId = 'roiName', selected = roifilename)
    })




    # ----------------------------------------------------------------------
    # Download ROI List
    # ----------------------------------------------------------------------
    output$downloadROI <- downloadHandler(
      filename = function(){
        make.names(paste0(input$roiOwner, '_',roiLabel(),'_roi.zip'))
      },
      content = function(fname){
        wd <- getwd()

        setwd(tempdir())
        print(tempdir())

        systime <- format(Sys.time(), '%Y-%m-%d %H:%M:%S')
        ROIList <- list(siteName = input$siteName,
                        vegType = input$vegType,
                        Owner= input$roiOwner,
                        ID = input$roiID,
                        Description = input$siteDescription,
                        createDate = strftime(systime, format = '%Y-%m-%d'),
                        createTime = strftime(systime, format = '%H:%M:%S'),
                        masks = rv$MASKs)



        roifilename <- paste0(roiLabel(),'_roi.csv')
        writeROI(ROIList, roifilename)
        fs <- c(roifilename,
                paste0(names(ROIList$masks), '.tif'),
                paste0(names(ROIList$masks), '_vector.csv'))
        zip(zipfile=fname, files=fs)
        setwd(wd)
      },
      contentType = "application/zip"
    )


    # ----------------------------------------------------------------------
    # Plot CC timeseries
    # ----------------------------------------------------------------------
    ccIndex <- reactive(seq(1, nimgList(), by = as.numeric(input$ccInterval)))


    ccImgList <- reactive(imgList()[ccIndex()])


    ccTime <- eventReactive(input$startExtractCC,{
      if(is.null(curMask())) {
        return(NA)
      }
      imgT()[ccIndex()]
    })


    ccVals <- eventReactive(input$startExtractCC,{
      if(is.null(curMask())|length(ccImgList())==0) {
        return(data.frame(rcc=NA, gcc=NA, bcc=NA))
      }
      dummy <- 0
      showModal(strong(modalDialog("Time series data are being extracted ...",
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   easyClose = F,
                                   size = 's',
                                   footer = actionButton(inputId = "stopExtractCC",
                                                         label =  "Stop",
                                                         width = '100%',
                                                         # class="btn-danger",
                                                         icon = icon('stop'),
                                                         style='background-color:#3b3a35; color:#fce319; ',
                                                         onclick="Shiny.onInputChange('stopThis',true)")
      )))
      dummy <- 0
      idMat <- sapply(rv$MASKs, function(x){c(as.POSIXct(paste(x$startdate, x$starttime)),
                                              as.POSIXct(paste(x$enddate, x$endtime)))})

      mIndex <- rep(NA, length(imgT()))
      for(i in 1:length(mIndex)){
        d <- rv$filetbl$DateTime[[i]]
        w <- which(d>=idMat[1,]&d<=idMat[2,])
        if(length(w)!=0) mIndex[i] <- w[1]
      }
      dummy <- 0

      dummy <- 0

      CCCT <- extractCCCTimeSeries(rmskList = lapply(rv$MASKs, function(x)(x$rasteredMask)),
                                   mIndex = mIndex,
                                   ccImgList())
      removeModal()
      CCCT
    })


    output$timeSeriesPlotly <- renderPlotly(
      {

        fontList <- list(
          family = "Courier New, monospace",
          size = 16,
          color = "#7f7f7f"
        )
        xAxis <- list(
          title = "Time",
          titlefont = fontList
        )
        yAxis <- list(
          title = "CC",
          titlefont = fontList
        )

        pointsMode <- tolower(input$ccMode)

        if(input$startExtractCC==0|is.null(isolate(curMask()))){


          if(input$startExtractCC>0)showModal(strong(modalDialog('You first have to create/select a mask!',
                                                                 style='background-color:#3b3a35; color:#fce319; ',
                                                                 footer = NULL, easyClose = T, size = 's')))

          tvals <- 0:1

          cvals <- matrix(NA, nrow=length(tvals), ncol = 3)
          colnames(cvals) <- c('rcc','gcc','bcc')
          cvals <- as.data.frame(cvals)

          yAxis$range <- c(0,1)
          xAxis$range <- c(0,1)
          cc <- melt(data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc),
                     variable.name='band', value.name='cc', id.vars=NULL)
          d <- data.table(time=tvals, cc)

          ccSel <- as.vector(sapply(input$ccBand, switch, R='red', G='green',  B='blue'))
          d <- d[band%in%ccSel]


          p <- plot_ly(data = d, x=~time, y= ~cc,
                       color = ~band,
                       colors = c('#FF4615','#007D00','#2364B7'),
                       type = 'scatter', mode = pointsMode) %>%
            layout(xaxis = xAxis, yaxis = yAxis) %>%
            config(collaborate = FALSE)
          return(p)
        }

        dummy <- 0
        cvals <- ccVals()
        tvals <- ccTime()

        wZeros <- (rowSums(cvals)==0)
        cvals[wZeros,] <- c(NA, NA, NA)

        shinyjs::enable("downloadTSData")

        # cc1 <- melt(cvals[,.(red, green, blue)],
        #             variable.name='band', value.name='cc', id.vars=NULL)
        #
        cc2 <- rbind(cvals[,.(cc=red, q25=red-r25, q75=r75-red, q5=red-r5, q95=r95-red, q10=red-r10, q90=r90-red, band='red')],
                     cvals[,.(cc=green, q25=green-g25, q75=g75-green, q5=green-g5, q95=g95-green, q10=green-g10, q90=g90-green, band='green')],
                     cvals[,.(cc=blue, q25=blue-b25, q75=b75-blue, q5=blue-b5, q95=b95-blue, q10=blue-b10, q90=b90-blue, band='blue')])

        cc2[,band:=factor(band, levels=c('red','green','blue'))]

        cc <- cc2

        d <- data.table(time=tvals, cc)
        ccSel <- as.vector(sapply(input$ccBand, switch, R='red', B='blue', G="green"))

        dd <- d[band%in%ccSel]
        # d[,band:=as.factor(band)]

        p0 <- plot_ly(data = dd, x=~time, y= ~cc,
                      color = ~band,
                      colors = c('#FF4615','#007D00','#2364B7'),
                      type = 'scatter', mode = pointsMode)

        p50 <- plot_ly(data = dd, x=~time, y= ~cc,
                       error_y = list(
                         type = "data",
                         symmetric = FALSE,
                         array = ~q75,
                         arrayminus = ~q25,
                         color=~band),
                       color = ~band,
                       colors = c('#FF4615','#007D00','#2364B7'),
                       type = 'scatter', mode = pointsMode)

        p80 <- plot_ly(data = dd, x=~time, y= ~cc,
                       error_y = list(
                         type = "data",
                         symmetric = FALSE,
                         array = ~q90,
                         arrayminus = ~q10,
                         color=~band),
                       color = ~band,
                       colors = c('#FF4615','#007D00','#2364B7'),
                       type = 'scatter', mode = pointsMode)

        p90 <- plot_ly(data = dd, x=~time, y= ~cc,
                       error_y = list(
                         type = "data",
                         symmetric = FALSE,
                         array = ~q95,
                         arrayminus = ~q5,
                         color=~band),
                       color = ~band,
                       colors = c('#FF4615','#007D00','#2364B7'),
                       type = 'scatter', mode = pointsMode)

        p <- switch(input$ccVar, 'None'=p0, '50%'=p50, '80%'=p80, '90%'=p90)

        hide_legend(p  %>%
                      layout(xaxis = xAxis, yaxis = yAxis
                             %>%
                               config(collaborate = FALSE)))
      })



    # ----------------------------------------------------------------------
    # Download timeseries
    # ----------------------------------------------------------------------
    output$downloadTSData <- downloadHandler(
      filename = function() {
        paste('timeseries-', input$maskName, '-', format(Sys.time(), format = '%Y-%m-%d-%H%M%S'), ".csv", sep="")
      },

      content = function(file) {
        cvals <- ccVals()
        # tvals <- paths()[,.(Year, DOY)]
        tvals <- ccTime()
        fvals <- ccImgList()
        # cc <- data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc)
        cc <- as.data.frame(round(cvals, digits = 5))
        d <- data.table(file = fvals, time = tvals, cc)
        write.table(d, file, sep = ',', row.names = F)
      }
    )

    observeEvent(rv$MASKs,{
      if(length(rv$MASKs)==0) {
        shinyjs::disable("downloadROI")
      }else{
        shinyjs::enable("downloadROI")
      }
    })




    # ----------------------------------------------------------------------
    # CLI
    # ----------------------------------------------------------------------
    observeEvent(input$getCLI, {
      files <- imgList()
      showModal(strong(modalDialog("CLI is being generated ...",
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   easyClose = F,
                                   size = 's',
                                   footer = actionButton(inputId = "stopExtractCC",
                                                         label =  "Stop",
                                                         width = '100%',
                                                         # class="btn-danger",
                                                         icon = icon('stop'),
                                                         style='background-color:#3b3a35; color:#fce319; ',
                                                         onclick="Shiny.onInputChange('stopThis',true)")
      )))
      rv$cli <- getCLArray(files)
      removeModal()
    })

    cliPath <- reactive(
      paste0(rv$folderpath,'/ROI/CLI.jpg')
    )

    observeEvent(input$writeCLI, {
      if(is.null(rv$cli)) {
        showModal(strong(modalDialog('First generate the CLI!',
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer = NULL, easyClose = T, size = 'm')))
        return()
      }
      writeJPEG(rv$cli, target =  cliPath())
      showModal(strong(modalDialog('CLI was saved in the ROI folder!',
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   footer = NULL, easyClose = T, size = 's')))

    })

    observeEvent(input$readCLI, {
      dummy <- 0
      if(!file.exists(cliPath())) {
        showModal(strong(modalDialog('ROI/CLI.jpg was not found!',
                                     style='background-color:#3b3a35; color:#fce319; ',
                                     footer = NULL, easyClose = T, size = 'm')))
        return()
      }
      rv$cli <- readJPEG(cliPath(), native=F)

      showModal(strong(modalDialog('CLI was loaded from the ROI folder!',
                                   style='background-color:#3b3a35; color:#fce319; ',
                                   footer = NULL, easyClose = T, size = 's')))
    })

    bri <- reactive({
      if(is.null(rv$cli)) return()
      apply(rv$cli, 1:2, max)
    })

    dar <- reactive({
      if(is.null(rv$cli)) return()
      apply(rv$cli, 1:2, min)
    })

    cliProcessed <- reactive({
      if(is.null(rv$cli)) return()
      # clhsv <- clRGB2HSV(rv$cli)
      switch(input$cliType,
             'RGB'=rv$cli,
             'R'=rv$cli[,,1],
             'G'=rv$cli[,,2],
             'B'=rv$cli[,,3],
             'Bright' = bri(),
             'Dark' =dar(),
             'Contrast' = bri()-dar())
    })
    output$cliPlot <- renderPlot(
      res=36,
      # width = function(){floor(session$clientData$output_cliPlot_width*1.5)},
      height = function(){floor(session$clientData$output_cliPlot_height*1.7)},
      {
        if(is.null(rv$cli)) return()
        par(mar=c(3,0,0,0))
        par(cex.axis = 2)
        oo <- rep(0, length(imgT()))
        plot(imgT(), oo,xaxs='i',yaxs='i', yaxt='n', type='n', ylab = '', ylim = c(0, 1), xaxt='n')

        par(xaxt='s')
        DT <- data.table(Year=rep(1981:2020, each=12), Month=rep(1:12, times=40), Day=1)
        DT[,Date:=as.POSIXct(as.Date(sprintf('%4d-%02d-%02d', Year, Month, Day)))]
        DT[,Label:=month.abb[Month]]
        DT[Month==1,Label:=as.character(Year)]
        axis(1, at = DT$Date, labels = as.character(DT$Label), font=2, cex=1.2)
        par(new=T)
        plotCLArray(cliProcessed())
        if(!is.null(rv$cliclickID)) abline(v=rv$cliclickID-0.5, col='red', lwd = 5)
      }
    )

    observeEvent(input$cliPoint, {
      if(is.null(rv$cli)) return()
      dumm <- 0

      rv$cliclickID <- ceiling(input$cliPoint$x)
    })


    output$cliClickPlot <- renderPlot(
      res=36,
      height = function(){floor(session$clientData$output_cliClickPlot_width/1.35)},

      {
        par(mar=c(0,0,0,0), oma=c(0,0,2,0))
        dumm <- 0
        if(is.null(rv$cli)|is.null(rv$cliclickID)) {
          plot(NA, xlim=c(0,1), ylim=c(0,1), xaxs='i',yaxs='i', xaxt='n', yaxt='n', bty='o', xlab='',ylab='')
          text(mean(par()$usr[1:2]), mean(par()$usr[3:4]), 'No CLI element was selected!', font=2, adj=.5, cex = 2)
        }else{
          jp <- plotJPEG(imgList()[rv$cliclickID])
          mtext(imgT()[rv$cliclickID], side = 3, line = 0.3, adj = 0.5, cex = 2, col = 'black', outer = T)
        }
      }
    )

    output$hoverText <- renderText({
      if(is.null(rv$cli)|is.null(input$cliHover)) return()
      ID <- ceiling(input$cliHover$x)
      paste0('Image # ', ID ,' : ',imgT()[ID])
    })


    shinyjs::disable("downloadTSData")
    shinyjs::disable("downloadROI")


    observe({
      if(nimgList()==0) return()

      req(input$maskEndTime)
      req(input$maskEndDate)
      if(input$openEnd) {
        shinyjs::disable('maskEndDate')
        shinyjs::disable('maskEndTime')
        updateDateInput(session, 'maskEndDate', value = '9999-12-31')
        updateTextInput(session, 'maskEndTime', value = '00:00:00')
      }else{
        shinyjs::enable('maskEndDate')
        shinyjs::enable('maskEndTime')
      }
    })



  }
  )
}
