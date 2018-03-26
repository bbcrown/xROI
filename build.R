if(!require("utils")) {install.packages("utils"); library(utils)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}


rm(list = ls())
# create('~/Projects/xROI/')
# source('data-raw/procdata.R')
PACKAGE.NAME = 'xROI'
file.remove('NAMESPACE')
document(pkg = '.')
install(pkg = '.')

# xROI::Launch()

system('rm -r toCRAN')
system('mkdir toCRAN', ignore.stderr = T)
system(paste0('rm ', PACKAGE.NAME, '*.tar.gz'), ignore.stderr = T)
system('cp -r R man DESCRIPTION NAMESPACE LICENSE inst toCRAN')
f <- build('toCRAN')
devtools::check('toCRAN')
devtools::revdep_check('toCRAN')
system(command = paste0('R CMD check --as-cran ', basename(f)))

# devtools::submit_cran()
