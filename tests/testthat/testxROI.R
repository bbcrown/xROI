context("testxROI")

mask <- tiff::readTIFF(system.file(package = 'xROI', 'dukehw-mask.tif'))
addMask_return <- addMask(mask, add = FALSE)

jpgFile <- system.file(package = 'xROI', 'dukehw.jpg')

f <- system.file(package = 'xROI', 'dukehw.jpg')
cli <- getCL(f)

server <- getServer()
ui <- getUI()

cli_file <- system.file(package = 'xROI', 'dukehw-cli.jpg')
jp <- jpeg::readJPEG(cli_file)

res <- plotJPEG(f)

roi_file <- system.file(package = 'xROI', 'example/ROI/example_DB_0001_roi.csv')
roi <- parseROI(roi_file)
tempPath <- file.path(tempdir(), 'roi.csv')
return_writeROI <- writeROI(roi, tempPath)


pnts <- matrix(c(0.1, 0.2,
                 0.1, 0.4,
                 0.5, 0.4,
                 0.5, 0.2),
                 4, 2, byrow= TRUE)
imgSize <- c(300, 400)
m <- rasterizeROI(pnts, imgSize)


test_that("test writeROI", {
  expect_equal(return_writeROI, 0)
})



test_that("test rasterizeROI", {
  expect_equal(floor(mean(m)*10), 9)
})


test_that("test addMask", {
  expect_equal(addMask_return, 0)
})

test_that("test cli", {
  expect_equal(floor(mean(jp)*100), 50)
})

test_that("test plotJPEG", {
  expect_equal(as.numeric(res[1]), 432)
})


test_that("test getCL", {
  expect_equal(floor(mean(cli)*100), 49)
})

test_that("test roi", {
  expect_equal(roi$Owner, 'bijan')
})

test_that("test server ui", {
  expect_equal(length(server), 1)
  expect_equal(length(ui), 3)
})
