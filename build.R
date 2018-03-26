if(!require("utils")) {install.packages("utils"); library(utils)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}


rm(list = ls())
PACKAGE.NAME = 'xROI'
file.remove('NAMESPACE')
unlink('toCRAN', recursive = TRUE)
system('mkdir toCRAN', ignore.stderr = TRUE)
system(paste0('rm ', PACKAGE.NAME, '*.tar.gz'), ignore.stderr = TRUE)

# create('~/Projects/xROI/')
devtools::document(pkg = '.')
devtools::install(pkg = '.')
system('cp -r R man DESCRIPTION NAMESPACE LICENSE inst toCRAN')
f <- devtools::build('toCRAN')
devtools::check('toCRAN')
system(command = paste0('R CMD check --as-cran ', basename(f)))

# devtools::submit_cran()
