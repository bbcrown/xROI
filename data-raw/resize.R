library(imager)

f <- dir('data-raw/original/', '*.jpg', full.names = T)

thmb <- function(f){
  im <- load.image(f)
  thmb <- resize(im,round(width(im)/3),round(height(im)/3))
  save.image(thmb, paste0('data-raw/example/', basename(f)), quality = 0.7)
}

x= mapply(thmb, f)
