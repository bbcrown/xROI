#' UI function
#'
#' This funciton constructs the user interface of the shiny app.
#' @return the user intrface module of the shiny app.
#' @keywords  UI Shiny App
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
#'
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

                            radioButtons(inputId = 'fileload', label = 'File load', choices = c('Phenocam format'='phenocam', 'From filelist.csv'='filelist')),
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
                              column(6, downloadButton("downloadROI", "Download ROI"))
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
                        selectInput('ccInterval', label = 'Temporal Interval', choices = c(1:7, 10, 15, 20, 30), selected = 1, width = '100%'),
                        actionButton("startExtractCC", "Extract", icon = icon('line-chart'), onclick="Shiny.onInputChange('stopThis',false)", width = "110px", style="background-color:#666; color:#fff;font-weight: bold;"),
                        hr(),
                        radioButtons('ccMode', label = NULL, choices = c('Markers', 'Lines+Markers')),
                        hr(),
                        checkboxGroupInput('ccBand', label = NULL, choices = c(Red='R', Green='G', Blue='B'),
                                           selected = c( 'G'), width = '100%', inline = F),
                        hr(),
                        radioButtons('ccVar', label = 'Confidence Interval', choices = c('None', '50%', '80%', '90%')),
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
                                  strong(textOutput('hoverText'), style="color:#FFFF00; border-color: #303030; background-color: #303030; font-weight: bold; font-size:18px")
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
                                    &nbsp; &nbsp; &nbsp; 2) create a mask file identifying pixels of interest; and<br/>
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
