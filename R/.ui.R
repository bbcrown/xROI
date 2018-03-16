#######################################################################
# User interface for the imageRun shiny app. 
# 
# The imageRun app is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in November, 2017.
#
# Most recent release: https://github.com/bnasr/imageRun
#######################################################################


fluidPage(
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
  
  headerPanel("imageRun: A General ROI Processor"),
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
                   column(6, colourpicker::colourInput(inputId = 'roiColors', allowTransparent=T, label = NULL, value = '#93a52480',  showColour = 'background'))
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
      includeHTML('about.html')
    }
    )
    
  )
)
