if(!require("utils")) {install.packages("devtools"); library(devtools)}
if(!require("devtools")) {install.packages("devtools"); library(devtools)}
if(!require("roxygen2")) {devtools::install_github("klutometis/roxygen"); library(roxygen2)}

PACKAGE.NAME = 'ROIx'
# create('~/Projects/ROIx/')
document(pkg = '.')
install(pkg = '.')

system('mkdir toCRAN')
system(paste0('rm ', PACKAGE.NAME, '*.tar.gz'))
system('cp -r R man DESCRIPTION NAMESPACE LICENSE inst toCRAN')
f <- build('toCRAN')
system('rm -r toCRAN')
system(command = paste0('R CMD check --as-cran ', basename(f)))

# devtools::revdep_check()
# devtools::submit_cran()
