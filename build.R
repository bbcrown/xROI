if(!require("utils")) {install.packages("utils"); library(utils)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}


rm(list = ls())
PACKAGE.NAME = 'xROI'
# create('~/Projects/xROI/')
# source('data-raw/procdata.R')
file.remove('NAMESPACE')
document(pkg = '.')
install(pkg = '.')

xROI::Launch()

system('mkdir toCRAN')
system(paste0('rm ', PACKAGE.NAME, '*.tar.gz'))
system('cp -r R data man DESCRIPTION NAMESPACE LICENSE inst toCRAN')
f <- build('toCRAN')
system('rm -r toCRAN')
system(command = paste0('R CMD check --as-cran ', basename(f)))

# devtools::check()
# devtools::revdep_check()
# devtools::submit_cran()
