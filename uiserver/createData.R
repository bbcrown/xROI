rm(list = ls())

library(shiny)
library(shinythemes)
library(shinyFiles)
library(plotly)

setwd('uiserver/')

ui <- source('ui.R')$value
server <- source('server.R')$value
source('funcs.R')

save.image(file = '../data/uiserver.RData')

f <- dir(path = 'example/', full.names = T, recursive = T)
zip(zipfile = 'example.zip', files = f)
ex <- file('example.zip', 'rb')
example <- readBin(ex, 'raw', n = 10^12)
close(ex)

save('example', file = '../data/example.RData')

setwd('..')

rm(list = ls())

