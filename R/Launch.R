#' Launch xROI app
#'
#' This function launch the app by opening the default web browser.
#' @param inputDir path to the input directory as a character string.
#' @keywords  Run Launch xROI App
#' @export
#' @import shiny
#' @rawNamespace import(shinyjs, except = c(runExample, updateColourInput, reset, colourPicker, colourInput, show))
#' @import shinythemes
#' @rawNamespace import(plotly, except = select)
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
#' @rawNamespace import(colourpicker, except = runExample)
#' @import rjson
#' @import RCurl
#' @import stringr
#' @rawNamespace import(data.table, except = c(month, hour, quarter, week, year, wday, second, minute, mday, yday, isoweek, shift))
#' @rawNamespace import(lubridate, except = origin)
#' @import moments
#' @importFrom grDevices rgb2hsv
#' @examples
#'
#' #Launch xROI app
#' xROI::Launch()
#'
Launch <- function(inputDir= NULL){
  data('example')
  tmpfile <- tempfile(fileext = '.zip')
  writeBin(example, tmpfile)
  exdir <- paste0(gettmpdir())
  unzip(zipfile = tmpfile, exdir = exdir)

  ## Only run examples in interactive R sessions
  if (interactive()) {
    ui <- getUI()
    server <- getServer(exdir, inputDir)
    shinyApp(ui, server, options = list(launch.browser = TRUE))
  }
}





getServer <- function(exdir, inputDir = NULL){
  return(function(input, output, session) {

    options(warn = -1)
    rv <- reactiveValues(centers = matrix(numeric(), 0, 2),
                         MASKs = list(),
                         slideShow = 0,
                         filetbl = NULL,
                         # filelist = '.',
                         folderpath = paste0(gettmpdir(), '/example'), #'./example',
                         # withDate = FALSE,
                         # roots = list('Working directory'='.', Home='~', root='/'),
                         imgs = NULL,
                         cli = NULL,
                         cliclickID = NULL,
                         downloadDir = gettmpdir())

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
      rv$folderpath
    })

    observeEvent(rv$folderpath,{
      dummy <- 0
      dir.create(roipath())
      rv$imgs <- dir(rv$folderpath, pattern = '*.jpg', full.names = T)

      f <- paste(rv$folderpath, 'filelist.csv', sep = '/')

      if(!file.exists(f)){
        showModal(strong(modalDialog('filelist.csv was not found in the folder!',
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

          jp <- plotJPEG(sampleImage(),  downloadDir = rv$downloadDir)

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
          if(!is.null(mm)&input$showMask)addMaskPlot(mm, col = input$roiColors)
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
                        rasteredMask = createRasteredROI(rv$centers, sampleImageSize()))

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

        newMASK <- createRasteredROI(rv$centers, sampleImageSize())
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
      ROIList <- parseROI(roifilename= input$roiName,
                          roipath = roipath(),
                          downloadDir = rv$downloadDir)
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
      writeROIListFile(ROIList, path = roipath(),  roifilename)

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
        writeROIListFile(ROIList, path = '',  roifilename)
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

      CCCT <- extractCCCTimeSeriesMultiMasks(rmskList = lapply(rv$MASKs, function(x)(x$rasteredMask)),
                                             mIndex = mIndex,
                                             ccImgList(),
                                             downloadDir = rv$downloadDir)
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
                       type = 'scatter', mode = 'lines+markers') %>%
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
        cc2 <- rbind(cvals[,.(cc=red, q25=red-r25, q75=r75-red, q2.5=red-r2.5, q975=r975-red, band='red')],
                     cvals[,.(cc=green, q25=green-g25, q75=g75-green, q2.5=green-g2.5, q975=g975-green, band='green')],
                     cvals[,.(cc=blue, q25=blue-b25, q75=b75-blue, q2.5=blue-b2.5, q975=b975-blue, band='blue')])

        cc2[,band:=factor(band, levels=c('red','green','blue'))]

        cc <- cc2

        d <- data.table(time=tvals, cc)
        ccSel <- as.vector(sapply(input$ccBand, switch, R='red', B='blue', G="green"))

        dd <- d[band%in%ccSel]
        # d[,band:=as.factor(band)]

        p0 <- plot_ly(data = dd, x=~time, y= ~cc,
                      color = ~band,
                      colors = c('#FF4615','#007D00','#2364B7'),
                      type = 'scatter', mode = 'lines+markers')

        p50 <- plot_ly(data = dd, x=~time, y= ~cc,
                       error_y = list(
                         type = "data",
                         symmetric = FALSE,
                         array = ~q75,
                         arrayminus = ~q25,
                         color=~band),
                       color = ~band,
                       colors = c('#FF4615','#007D00','#2364B7'),
                       type = 'scatter', mode = 'lines+markers')

        p95 <- plot_ly(data = dd, x=~time, y= ~cc,
                       error_y = list(
                         type = "data",
                         symmetric = FALSE,
                         array = ~q975,
                         arrayminus = ~q2.5,
                         color=~band),
                       color = ~band,
                       colors = c('#FF4615','#007D00','#2364B7'),
                       type = 'scatter', mode = 'lines+markers')

        p <- switch(input$ccVar, 'None'=p0, '50%'=p50, '95%'=p95)

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

        # cc <- data.frame(red= cvals$rcc, green = cvals$gcc, blue= cvals$bcc)
        cc <- as.data.frame(cvals)
        d <- data.table(tvals, cc)
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
      rv$cli <- getCenterLineArray(files)
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
      clhsv <- clRGB2HSV(rv$cli)
      switch(input$cliType,
             'RGB'=rv$cli,
             'R'=rv$cli[,,1],
             'G'=rv$cli[,,2],
             'B'=rv$cli[,,3],
             'H'=clhsv[,,1],
             'S'=clhsv[,,2],
             'V'=clhsv[,,3],
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
        if(!is.null(rv$cliclickID)) abline(v=rv$cliclickID-0.5, col='red')
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
          jp <- plotJPEG(imgList()[rv$cliclickID],  downloadDir = rv$downloadDir)
          mtext(imgT()[rv$cliclickID], side = 3, line = 0.3, adj = 0.5, cex = 2, col = 'black', outer = T)
        }
      }
    )

    output$hoverText <- renderText({
      if(is.null(rv$cli)|is.null(input$cliHover)) return()
      ID <- ceiling(input$cliHover$x)
      paste0('Image # ', ID ,' = ',imgT()[ID])
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
getUI <- function(){
  return(fluidPage(
    theme= shinytheme('darkly'),
    tags$head(
      tags$style(HTML("body {
                      background-color: #aaa;
}
#sidebar {
background-color: #808080;
}"
      ))
    ),

    shinyjs::useShinyjs(),
    # tags$head(tags$style(HTML( "#Select1 ~ .selectize-control.single .selectize-input {border: 1px solid #fff;}"))),
    # tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max {visibility: hidden !important;}'))),

    headerPanel("xROI: A General ROI Processor"),
    tabsetPanel(
      tabPanel('ROI Tool',
               sidebarPanel(id='sidebar',
                            width = 4,
                            # div(style='border-color: #303030; background-color: #303030',
                            #     # directoryInput('inputDir', label = 'Select a directory', value = if(LOCAL_RUN)'~/Projects/hyperPheno/'else'~')
                            #     directoryInput('inputDir', label = 'Select a directory', value = 'example/')
                            # ),
                            # fileInput("filelist", "Date-time file:",
                            #           multiple = F,
                            #           accept = c("text/csv",
                            #                      "text/comma-separated-values,text/plain",
                            #                      ".csv")),

                            fluidRow(
                              column(4, shinyDirButton('folderpath', label='Images directory',
                                                       title='Select the directory of images')),
                              column(8, br(), htmlOutput('folderpath'))
                            ),

                            # shinyFilesButton('filelist', label='Date-time file',
                            #                  title='Select Date-time file',
                            #                  multiple=FALSE),
                            # htmlOutput('filelist'),

                            textInput('siteName','Site', placeholder = 'Enter the site name'),
                            textInput('siteDescription','Description', placeholder = 'Enter a description for the ROI'),
                            selectInput("vegType", "Vegetation Type", choices = list('Agriculture (AG)' = 'AG',
                                                                                     'Deciduous Broadleaf (DB)' = 'DB',
                                                                                     'Evergreen Broadleaf (EB)' = 'EB',
                                                                                     'Evergreen Needleleaf (EN)' = 'EN',
                                                                                     'Deciduous Needleleaf (DN)' = 'DN',
                                                                                     'Grassland (GR)' = 'GR',
                                                                                     'Mixed Forest (MX)' = 'MX',
                                                                                     'Non-vegetated (NV)' = 'NV',
                                                                                     'Reference Panel (RF)' = 'RF',
                                                                                     'Shrub (SH)' = 'SH',
                                                                                     'Tundra (TN)' = 'TN',
                                                                                     'Understory (UN)' = 'UN',
                                                                                     'Wetland (WL)' = 'WL',
                                                                                     'Other/Canopy (XX)' = 'XX'),
                                        selected = 'RF'
                            ),

                            fluidRow(
                              column(2, p('ROI', style="font-weight: bold; font-size:18px ")),
                              column(6, numericInput('roiID', label = NULL, min = 1, max = 99, value = 1)),
                              column(3, actionButton('newROI', label = 'New', icon = icon('new')))
                            ),
                            textInput('roiOwner','Owner', placeholder = 'Enter your name'),
                            # br(),
                            selectInput("roiName", "ROI", 'New ROI'),
                            strong(textOutput('roiFileName')),
                            # br(),

                            selectInput("maskName", label = 'Mask', choices = 'New mask'),
                            fluidRow(column(4, strong('Sample Image:')),
                                     column(8, textOutput('sampleImagePath'))),
                            # br(),
                            fluidRow(
                              column(6, actionButton( 'matchStart', 'Match start', width = '100%', style='background-color:#666; color:#fff;font-weight: bold;')),
                              column(6, actionButton( 'matchEnd', 'Match end', width = '100%', style='background-color:#666; color:#fff;font-weight: bold;'))
                            ),
                            br(),

                            # fluidRow(
                            #   column(5, numericInput('maskStartID', label = NULL, min = 1, max = 1, value = 1, step = 1)),
                            #   column(5, numericInput('maskEndID', label = NULL, min = 1, max = 1, value = 1, step = 1)),
                            #   column(1, checkboxInput('openEnd', label = '', value = F))
                            # ),

                            # conditionalPanel('input.siteName!=""', {
                            fluidRow(
                              column(1, strong('from', style='font-size:70%;font-weight: bold;')),
                              column(5, dateInput('maskStartDate', label = NULL, value =  '2001-01-01', startview = 'day')),
                              column(4, textInput('maskStartTime', label = NULL, value = '00:08:00')),
                              column(1, '')
                            )
                            # })
                            ,
                            # conditionalPanel('input.siteName!=""', {
                            fluidRow(
                              column(1, strong('to', style='font-size:70%')),
                              column(5, dateInput('maskEndDate', label = NULL, value =  '2099-01-01', startview = 'day')),
                              column(4, textInput('maskEndTime', label = NULL, value = '00:20:00')),
                              column(1, checkboxInput('openEnd', label = '', value = F))
                            )
                            # })
                            ,

                            fluidRow(
                              column(6, actionButton("saveROI", "Save ROI", icon = icon('list-alt'), width = "100%")),
                              column(6, downloadButton("downloadROI", "Download ROI files"))
                            )

               ),




               mainPanel(
                 sliderInput(inputId = "contID",label =  NULL,min = 1, max = 1,ticks = F, animate=F, value = 1, step = 1, width = '100%'),

                 fluidRow(
                   column(1, strong()),
                   column(2, actionButton("back", "", icon = icon('minus'), width = '100%', style="border-color: #222222; background-color: #222222")),
                   column(2, actionButton("backplay", "", icon = icon('backward'), width = '100%', style="border-color: #222222; background-color: #222222")),
                   column(2, actionButton("pause", "", icon = icon('stop'), width = '100%',  style="border-color: #222222; background-color: #222222")),
                   column(2, actionButton("play", "", icon = icon('forward'), width = '100%', style="border-color: #222222; background-color: #222222")),
                   column(2, actionButton("forw", "", icon = icon('plus'), width = '100%',  style="border-color: #222222; background-color: #222222"))
                 ),

                 fluidRow(
                   column(1, strong()),
                   column(10, plotOutput("imagePlot", click = "newPoint", dblclick = 'gapPoint', height = 'auto')),
                   # column(5, plotOutput("maskPlot", height = 'auto')),
                   column(1, strong())
                 ),

                 br(),

                 fluidRow(
                   column(1, strong()),
                   column(5,  fluidRow(
                     column(4, actionButton("clearCanvas", "Erase", icon = icon('eraser'), class="btn-primary", width = "100%", style='font-weight: bold;')),
                     column(4,  actionButton("undoCanvas", "Undo", icon = icon('undo'), class="btn-primary", width = "100%", style='font-weight: bold;')),
                     column(4, actionButton("acceptCanvas", "Accept", icon = icon('thumbs-up'), class="btn-primary", width = "100%", style='font-weight: bold;'))
                   )),
                   column(5,  fluidRow(
                     column(6, checkboxInput('showMask', label = 'Show Mask', value = T)),
                     column(6, colourpicker::colourInput(inputId = 'roiColors', allowTransparent=T, label = NULL, value = '#ab522280',  showColour = 'background'))
                   )),
                   column(1, strong())
                 )
               )
      ),

      tabPanel('Time series extraction',
               fluidRow(
                 column(2,
                        selectInput('ccInterval', label = 'Interval', choices = c(1:7, 10, 15, 20, 30), selected = 1, width = '50px'),
                        actionButton("startExtractCC", "Extract", icon = icon('line-chart'), onclick="Shiny.onInputChange('stopThis',false)", width = "110px", style="background-color:#666; color:#fff;font-weight: bold;"),
                        hr(),
                        checkboxGroupInput('ccBand', label = NULL, choices = c(Red='R', Green='G', Blue='B'),
                                           selected = c( 'G'), width = '100%', inline = F),
                        hr(),
                        radioButtons('ccVar', label = 'Intervals', choices = c('None', '50%', '95%')),
                        hr(),
                        downloadButton("downloadTSData", "Download\t")
                 ),
                 column(10, plotlyOutput(outputId = "timeSeriesPlotly", height = "800", width = "100%"))

               )
      ),

      tabPanel('CLI Processor',
               mainPanel(width=12,
                         fluidRow(
                           column(3,
                                  br(),
                                  br(),
                                  actionButton("getCLI", "Generate CLI", icon = icon('line-chart'),
                                               onclick="Shiny.onInputChange('stopThis',false)",
                                               width = "100%",
                                               style="background-color:#666; color:#fff;font-weight: bold;"),
                                  br(),
                                  br(),
                                  actionButton("writeCLI", "Write CLI", icon = icon('line-chart'),
                                               onclick="Shiny.onInputChange('stopThis',false)",
                                               width = "100%",
                                               style="background-color:#666; color:#fff;font-weight: bold;"),
                                  br(),
                                  br(),
                                  actionButton("readCLI", "Read CLI", icon = icon('line-chart'),
                                               onclick="Shiny.onInputChange('stopThis',false)",
                                               width = "100%",
                                               style="background-color:#666; color:#fff;font-weight: bold;"),
                                  br(),
                                  br(),
                                  radioButtons('cliType', label = NULL, choices = c('RGB',
                                                                                    'R','G','B',
                                                                                    # 'H','S','V',
                                                                                    'Bright', 'Dark', 'Contrast'), inline = T),
                                  br(),

                                  plotOutput("cliClickPlot",  width =  '100%'),
                                  br(),
                                  br(),
                                  strong(textOutput('hoverText'))
                           ),
                           column(9,
                                  br(),
                                  plotOutput(outputId = "cliPlot", click = "cliPoint", hover = hoverOpts(id="cliHover", delay = 10), width = "100%")
                           )
                         )
               )

      ),

      tabPanel('About',{
        includeHTML( textConnection('<div id="readme" class="readme blob instapaper_body">
                                    <article class="markdown-body entry-content" itemprop="text">
                                    <h2>xROI: A General ROI Processor</h2>
                                    <p>In order to extract time series data from a series of images, one needs to : <br/>
                                    &nbsp; &nbsp; &nbsp; 1) delineate a region of interest (ROI); <br/>
                                    &nbsp; &nbsp; &nbsp; 2) create a mask file identifying pixles of interest; and<br/>
                                    &nbsp; &nbsp; &nbsp; 3) calculate averaged values of particular bands (e.g. Green Chromatic Coordinate or GCC) over a time period.<br/> <br/>
                                    However, these steps are painstaking and need special accuracy. The xROI tool provides an interactive web interface to facilitate these process of large imagery datasets. This document is a simple guide to explain different elements of the xROI ROI and their functionality.</p>
                                    <br/>

                                    <h3>Easy to launch the app:</h3>
                                    <p>The following command will check for all the required R packages, install them if needed and run the xROI app directly from CRAN. However, latest version of R should be already installed. The app requires pre-installed GDAL and basic image rendering libraries (png, jpeg, tif, etc.) on the operating system.</p>
                                    <pre lang="{r,">
                                    <code><br/>install.packages("xROI", repos = "https://cran.rstudio.com/")<br/><br/>xROI::Launch()</code><br/>
                                    </pre>
                                    <br/>
                                    <p>The R package is developed and maintained by <a href="https://github.com/bnasr" target="_blank">Bijan Seyednarollah</a>.</p>
                                    <p>Most recent release is available from: <a href="https://github.com/bnasr/xROI" target="_blank">https://github.com/bnasr/xROI</a></p>
                                    <p>We have also developed another Shiny app for ROI processing of digital repeat photography: drawROI. The drawROI app has been specifically customized and extended for the PhenoCam network, where user can extract the data and process using a single interface.  </p>
                                    </article>
                                    </div>'))
      }
      )

    )
  ))
}



# LOCAL_RUN <- T
PRINT_LOGS <- T

# Check If String is a URL
is.url <-function(x) {
  grepl("www.|http:|https:", x)
}

# plot jpeg image using as raster given image path.
plotJPEG <- function(path, add=FALSE, xlim = NULL, ylim = NULL, downloadDir, showLoad = F, Update = F)
{
  jpgNonNative <- NULL
  jpgNative <-  readJPEG(path, native=T) # read the file
  res <-  dim(jpgNative)[2:1] # get the resolution
  if(is.null(xlim)) xlim <- c(1,res[1])
  if(is.null(ylim)) ylim <- c(1,res[2])
  if (!add) # initialize an empty plot area if add==FALSE
    plot(NA, xlim = xlim, ylim = ylim, type='n',
         xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='o')
  rasterImage(jpgNative,1,1,res[1],res[2])
  invisible(list(res=res,
                 jpgNonNative=jpgNonNative,
                 jpgNative=jpgNative ))
}


# interactive drawing of polygons by user clicks on the plot
draw.polygon <-
  function (col = "#80303080", lty = 1, ...)
  {
    xy <- locator(2)
    lines(xy$x, xy$y, lty = lty)

    while(is.list(c1 <- locator(1))) {
      xy$x <- c(xy$x, c1$x)
      xy$y <- c(xy$y, c1$y)
      lines(xy$x, xy$y, lty = lty)
    }
    xy <- data.frame(xy)
    xy <- rbind(xy, xy[1, ])
    polygon(xy$x, xy$y, lty = lty, col = col, ...)

    invisible(xy)
  }


# extract chromatic colors of RGB channels for given jpeg file and mask matrix
extractCCC <- function(path, m, downloadDir){
  path
  # path <- tryDownload(path, downloadDir = downloadDir, showLoad = F, Update = F)
  jp <- readJPEG(path)
  dm <- dim(jp)
  rgb <- jp
  dim(rgb) <- c(dm[1]*dm[2],3)

  if(!identical(dim(rgb), dim(m))) return(NULL)

  mrgb <- na.omit(rgb*m)
  t <- rowSums(mrgb)
  ccMat <- apply(mrgb, 2, '/', t)

  ccMat <- na.omit(ccMat)
  cc <- colMeans(ccMat)
  cc <- cc/sum(cc)

  tbl <- as.data.frame(t(apply(ccMat, 2, quantile, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))))
  rownames(tbl) <- c('r','g','b')
  colnames(tbl) <- c('min','q2.5','q25','q50','q75','q975','max')

  tbl$cc <- cc
  # tbl$mean <- colMeans(ccMat)
  tbl$std <- apply(ccMat, 2, sd)
  # tbl$skewness <- apply(ccMat, 2, skewness)
  # tbl$kurtosis <- apply(ccMat, 2, kurtosis)
  tbl$brightness <- mean(apply(mrgb, 2, max))
  tbl$darkness <- mean(apply(mrgb, 2, min))
  tbl$contrast <- tbl$brightness - tbl$darkness
  tbl
}


# shapefile to mask raster given coordinates of the polygon
createRasteredROI <- function(pnts, imgSize){

  pnts <- t(apply(pnts, 1, '*', imgSize))
  ext <- extent(1, imgSize[1], 1, imgSize[2])
  poly <- as(ext  ,"SpatialPolygons")
  # poly@polygons[[1]]@Polygons[[1]]@coords <- as.matrix(pnts)

  tbl <- as.data.table(na.omit(cbind(pnts,cumsum(is.na(pnts[,1]))+1 )))
  colnames(tbl) <- c('x', 'y', 'g')
  ng <- table(tbl$g)

  polyList <- list()
  np <- length(ng[which(ng>=3)])

  for(gi in 1:np)
    polyList[[gi]] <- as.matrix(tbl[g==gi, .(x,y)])

  polys <- SpatialPolygons(
    lapply(1:np,
           function(x){
             p <- slot(poly@polygons[[1]], "Polygons")[[1]]
             slot(p, "coords") <- polyList[[x]]
             pp <- Polygons(list(p), ID = x)
             return(pp)
           })
  )

  r <- rasterize(polys, raster(ext, nrow = imgSize[2], ncol = imgSize[1]))
  r[!is.na(r)] <- 1

  m1 <- as.matrix(r)
  m <- m1
  m[m1==0|is.na(m1)] <- 1
  m[m1!=0] <- 0
  m
}


#extract time series of chromatic colors for a vector of jpeg files.

extractCCCTimeSeriesMultiMasks <- function(rmskList, mIndex, paths, PLUS=F, session=shiny::getDefaultReactiveDomain(), downloadDir){

  continue = TRUE

  mlist <- list()

  for(i in 1:length(rmskList)){
    mi <- as.vector(1-as.matrix(rmskList[[i]]))
    mi[mi==0] <- NA
    mlist[[i]] <- mi
  }

  n <- length(paths)
  CCCT <- matrix(NA, nrow=n, ncol=24)


  # if(exists('session'))
  withProgress(value = 0, message = 'Extracting CCs',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break
                 m <- mlist[[mIndex[i]]]
                 # printLog(paste(i, mIndex[i], sum(m, na.rm = T)))
                 tbl <- extractCCC(paths[i],
                                   cbind(m, m, m),
                                   downloadDir = downloadDir)
                 if(!is.null(tbl))
                   CCCT[i,] <- c(tbl$cc, tbl$std,
                                 tbl$q2.5, tbl$q25, tbl$q50, tbl$q75, tbl$q975,
                                 # tbl$skewness, tbl$kurtosis,
                                 tbl$brightness[1], tbl$darkness[1], tbl$contrast[1])
                 incProgress(1/n)
                 # Sys.sleep(1)
                 # if(i%%20==0)httpuv:::service()
               }
  )
  CCCT <- as.data.table(CCCT)
  colnames(CCCT) <- c('red','green','blue',
                      # 'r.mean','g.mean','b.mean',
                      'r.std','g.std','b.std',
                      'r2.5', 'g2.5', 'b2.5',
                      'r25', 'g25', 'b25',
                      'r50', 'g50', 'b50',
                      'r75', 'g75', 'b75',
                      'r975', 'g975', 'b975',
                      # 'r.skewness','g.skewness','b.skewness',
                      # 'r.kurtosis','g.kurtosis','b.kurtosis',
                      'brightness','darkness','contrast'
  )
  CCCT
}

# writing ROIList file on the given list, path and filename
writeROIListFile <- function(ROIList, path, roifilename){

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

    writeTIFF(m*1 , where = paste0(path, rName,'.tif'))

    maskpoints <- ROIList$masks[[i]]$maskpoints
    maskpoints <- rbind(dim(m), maskpoints)
    if(nrow(maskpoints)>3)
      write.table(maskpoints, file = paste0(path, rName,'_vector.csv'), col.names = F, row.names = F, sep = ',')

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
  writeLines(allText, paste0(path, roifilename))
}



# parsing klima filenames to data tabels of site, date and time given vector of filenames
filePathParse <- function(filenames)
{
  imgDT <- data.table(filenames = filenames)
  # imgDT$tmp <- unlist(lapply(filenames, function(x){strsplit(x,split = '/', fixed = T)[[1]][3]}))
  imgDT[,c('Site', 'Year', 'Month','Day','HHMMSS'):=as.data.table(matrix(unlist(strsplit(gsub(filenames,pattern = '.jpg', replacement = ''), split = '_')), ncol=5, byrow = T))]
  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]

  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  imgDT
}


# constraining string input to fixed time format
fixFormatTime <- function(asText){
  asSplit <- unlist(strsplit(asText, ':'))
  for(i in 1:length(asSplit))
    if(any(!unlist(strsplit(asSplit[i], ''))%in%as.character(0:9))) asSplit[i] <- 0

    asNum <- as.numeric(asSplit)
    if(length(asNum)>3) asNum <- asNum[1:3]
    if(length(asNum)==0) asNum <- c(0, 0, 0)
    if(length(asNum)==1) asNum <- c(asNum[1], 0,0)
    if(length(asNum)==2) asNum <- c(asNum[1:2], 0)

    asNum[1] <- min(23, max(asNum[1], 0))
    asNum[2] <- min(59, max(asNum[2], 0))
    asNum[3] <- min(59, max(asNum[3], 0))

    asTextNew <- sapply(asNum, sprintf, fmt='%02d')
    asTextNew <- paste(asTextNew, collapse = ':')
    asTextNew
}




#parsing ROIList file into a list in R
parseROI <- function(roifilename, roipath, downloadDir){
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
    if(file.exists(maskpointspath)|url.exists(maskpointspath)) {
      dummy=0
      maskpoints <- as.matrix(read.csv(maskpointspath, header = F, skip = 1))
    }else{
      maskpoints <- NULL
    }

    maskpath <- tryDownload(maskpath, downloadDir = downloadDir, showLoad = T, Update = F)

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




# an effort to convert rasterized mask to vectorized shape
maskRaster2Vector <- function(r){
  r[r==0] <- 1
  r[r==255] <- NA

  p <- rasterToPolygons(r, dissolve = T)
  c <- p@polygons[[1]]@Polygons[[1]]@coords
  c
}


#parse image data table to site, date and time, probably redundant function
parseIMG.DT <- function(imgDT){

  imgDT[,c('Site', 'Year', 'Month','Day','HHMMSS'):=as.data.table(matrix(unlist(strsplit(gsub(filenames, pattern = '.jpg', replacement = ''), split = '_')), ncol=5, byrow = T))]
  imgDT[,Year:=as.numeric(Year)]
  imgDT[,Month:=as.numeric(Month)]
  imgDT[,Day:=as.numeric(Day)]
  imgDT[,HHMMSS:=as.numeric(HHMMSS)]
  imgDT[,Hour:=floor(HHMMSS/10000)]
  imgDT[,Minute:=floor((HHMMSS%%10000)/100)]
  imgDT[,Second:=HHMMSS%%100]
  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]

  imgDT[,DOY:=yday(ISOdate(Year, Month, Day))]
  imgDT[,Date:=date(ISOdate(Year, Month, Day))]
  imgDT[,DateTime:=ISOdatetime(Year, Month, Day, Hour, Minute, Second)]
  imgDT[,conT:=Year+DOY/(365+(2001%%4==0))]
  imgDT[,YearDOY:=Year+DOY/1000]
  imgDT
}


# gettng image data table given site and midday list path
getIMG.DT <- function(sites){
  imgDT <- data.table()

  for(site in sites){
    if(is.url(middayListPath)){
      mdiJSON = fromJSON(file = paste0(middayListPath, site,'/'))
      tbl <- data.table( DateJSON = as.Date(sapply(mdiJSON$images, function(x){x$date })),
                         path = as.character(sapply(mdiJSON$images, function(x){x$midimg})))
    }else{
      tbl <- read.table(paste0(middayListPath, site), header = F, colClasses = 'character', col.names = 'path')
    }

    imgDT.tmp <- as.data.table(tbl)
    imgDT.tmp$path <- paste0(mainDataPath, imgDT.tmp$path)
    imgDT <- rbind(imgDT, imgDT.tmp)
  }

  pathsub <- imgDT[, .(gsub(path, pattern = mainDataPath, replacement = ''))]
  splt <- pathsub[, tstrsplit(V1, split = '/')]
  colnames(splt) <- c('empty','data','archive','site','year','month','filenames')
  splt[, newpath:=paste(empty, data, archive, site, 'originals', year, month, filenames, sep='/')]

  imgDT$filenames <- splt$filenames
  imgDT$newpath <- splt$newpath

  imgDT$newpath <- NULL

  imgDT <- imgDT[str_count(filenames, pattern = '_')==4, ]
  imgDT <- parseIMG.DT(imgDT)
  imgDT
}


# print a message into konsole given the message string for logging purposes
printLog <- function(msg=NULL, init=F, finit=F){
  if(!PRINT_LOGS) return()

  if(init){
    message(paste('\n--------------------------------------------------------------------\n',
                  as.character(Sys.time()),'New session just started!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }

  if(finit){
    message(paste('\n--------------------------------------------------------------------\n',
                  as.character(Sys.time()),'Initial setup was completed!',
                  '\n--------------------------------------------------------------------\n'))
    return()
  }

  message(paste(as.character(Sys.time()), msg, '\t'))
}


dirHTML <- function(url, sitename, pattern = 'roi.csv$'){
  showModal(strong(
    modalDialog(HTML('Loading roi.csv files ...'),
                easyClose = F,
                size = 's',
                style='background-color:#3b3a35; color:#fce319; ',
                footer = NULL
    )))

  tmpath <- tempfile()
  download.file(url, destfile = tmpath, quiet = !PRINT_LOGS, method = 'curl')

  txt <- readLines(tmpath)
  n <- grep(paste0('<a href=\"', sitename), x = txt)
  DT <- data.table(file = as.character(sapply(txt[n], function(x){s=strsplit(x, split='<|>'); s[[1]][3]})))
  roi <- grep(DT$file, pattern = pattern)
  removeModal()
  DT$file[roi]
}


tryDownload <- function(path, Update = T, showLoad = T, downloadDir){
  printLog(paste('tryDownload was called with ', path, downloadDir ))

  if(!is.url(path)) {
    if(file.exists(path))
      return(path)
    else
      return(NULL)
  } else if(!url.exists(path))return(NULL)

  fname <- basename(path)

  if(showLoad)showModal(strong(
    modalDialog(HTML(paste0('Loading ', fname , '...')),
                easyClose = F,
                size = 's',
                style='background-color:#3b3a35; color:#fce319; ',
                footer = NULL
    )), session=shiny::getDefaultReactiveDomain())



  destfile <- paste0(downloadDir, '/', fname)

  if(!file.exists(destfile))
    download.file(path, destfile = destfile, method = 'curl')
  else
    if(as.Date(file.info(destfile)$mtime)<Sys.Date()&Update)
      download.file(path, destfile = destfile, method = 'curl')

  if(showLoad)removeModal()

  destfile
}


getNAfromLast <- function(x){
  xrev <- rev(x)
  xnew <- xrev
  repeat{
    w <- which(is.na(xrev))
    xnew[w] <- xrev[w+1]
    if(identical(xnew, xrev)) break
    xrev <- xnew
  }
  rev(xrev)
}

putImageFooter <- function(id, mrgDT, footer='', grid = T, cex = NULL){
  if(is.null(cex)) {
    dummy <- 0
    session <- shiny::getDefaultReactiveDomain()
    cex <- session$clientData$output_imagePlot_width/180
  }
  Date <- mrgDT[ID==id, Date]
  if(length(Date)==0) return()
  Haze <- mrgDT[ID==id, Haze]
  Black <- signif(mrgDT[ID==id, blackness], 2)

  if(grid){
    usr <- par()$usr
    abline(v=seq(usr[1], usr[2], length.out = 10), lty=2, col='yellow', lwd = 2)
    abline(h=seq(usr[3], usr[4], length.out = 10), lty=2, col='yellow', lwd = 2)
  }

  rect(par()$usr[1], par()$usr[3], par()$usr[2], par()$usr[4]*.05, col = 'white')
  mtext(side = 1, Date, line = -1, adj = .05, col = 'black', font = 2, cex = cex)
  mtext(side = 1, footer, line = -1, adj = .5, col = 'black', font = 2, cex = cex)
  mtext(side = 1, Black, line = -1, adj = .85, col = 'black', font = 2, cex = cex)
  mtext(side = 1, Haze, line = -1, adj = .95, col = 'black', font = 2, cex = cex)

}


gettmpdir <- function() {
  # tm <- Sys.getenv(c('TMPDIR', 'TMP', 'TEMP'))
  # d <- which(file.info(tm)$isdir & file.access(tm, 2) == 0)
  # if (length(d) > 0)
  #   tm[[d[1]]]
  # else
  if (.Platform$OS.type == 'windows')
    Sys.getenv('R_USER')
  else
    '/tmp'
}


addMaskPlot <- function(mask, add = T, col='black'){
  wd <- getwd()
  setwd(tmpDir())
  writeTIFF(mask*1, 'tmp.tif')
  rmask <- raster('tmp.tif')
  rmask[rmask!=0] <- NA

  plot(rmask,legend=F, add=T, col=col)
  # file.remove('tmp.tif')
  setwd(wd)
}





getCenterLine <- function(file){
  jp <- try(readJPEG(file, native=F), silent = T)

  if(class(jp)=="try-error") return(NA)

  res <-  dim(jp)[1:2] # get the resolution
  center <- floor(res/2)

  centerLine <- jp[1:res[1], center[2],]

  # list(res=res, centerLine=centerLine)
  centerLine
}


getCenterLineArray <- function(files, session=shiny::getDefaultReactiveDomain()){
  continue = TRUE

  first <- which(!is.na(files))[1]
  res <-  dim(readJPEG(files[first], native=F))[1:2] # get the resolution
  n <- length(files)

  clArray <- array(data = NA, dim = c(res[1], n ,3))

  pb <- txtProgressBar(1, n, style = 3)


  withProgress(value = 0, message = 'Extracting CLI',
               for(i in 1:n){
                 if(isTRUE(session$input$stopThis))break

                 if(is.na(files[i]))
                   cl <- matrix(0, res[1], 3)
                 else
                   cl <- getCenterLine(files[i])

                 if(is.na(cl)) cl <- matrix(0, res[1], 3)
                 r <- cl[,1]
                 g <- cl[,2]
                 b <- cl[,3]

                 if(nrow(cl)!=res[1]){
                   message(paste('wrong dimensions at', i, files[i], 'resampling ...'))
                   x1 <- (0:(nrow(cl)-1))/nrow(cl)
                   x2 <- (0:(res[1]-1))/res[1]
                   r <- approx(x1, r, x2)$y
                   g <- approx(x1, g, x2)$y
                   b <- approx(x1, b, x2)$y
                   r[is.na(r)] <- 0
                   g[is.na(g)] <- 0
                   b[is.na(b)] <- 0
                 }

                 clArray[,i,1] <- r
                 clArray[,i,2] <- g
                 clArray[,i,3] <- b

                 setTxtProgressBar(pb, i)

                 incProgress(1/n)
                 # Sys.sleep(1)
                 # if(i%%20==0)httpuv:::service()
               }
  )





  clArray
}

plotCLArray <- function(clArray, bands=1:3){
  tmp <- tempfile()
  if(length(dim(clArray))==2)
    writeJPEG(clArray, target = tmp)
  else
    writeJPEG(clArray[,,bands], target = tmp)

  plotJPEG(tmp)
}

clRGB2HSV <- function(clArray){
  dat <- floor(255*clArray[,,1:3])
  dm <- dim(dat)
  RGB <- aperm(dat,c(3,2,1))
  dim(RGB) <- c(3, prod(dm[1:2]))

  HSV <- rgb2hsv(RGB)
  dim(HSV) <- c(3, dm[2], dm[1])
  HSV <- aperm(HSV, c(3,2,1))

  HSV
}
