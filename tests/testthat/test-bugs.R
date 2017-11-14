context("Bugs")

test_that("DS-975",
          {
                  suppressWarnings(gss <- foreign::read.spss("https://docs.displayr.com/images/7/7b/GSS2014.sav",
                                   to.data.frame = TRUE, max.value.labels = 20))
                  expect_equal(dim(gss), c(3842, 380))
                  gss <- flipExampleData::TidySPSS(gss)
                  expect_equal(dim(gss), c(3842, 380))
                  attach(gss)
                  df <- data.frame(confinan, conbus, coneduc, conlabor,
                                              conmedic, conpress)
                  expect_equal(dim(df), c(3842, 6))
                  z = suppressWarnings(apply(flipTransformations::AsNumeric(df, binary = FALSE), 2, mean, na.rm = TRUE))
                  expect_equal(unname(z), c(2.189400, 2.018562, 1.943903, 2.183138, 1.727344, 2.368917), tolerance = 5e-7)
                  z = suppressWarnings(apply(flipTransformations::AsNumeric(df, binary = FALSE), 2, sd, na.rm = TRUE))
                  expect_equal(unname(z), c(0.6434409, 0.5976801, 0.6378311, 0.6168390, 0.6404637, 0.6238356), tolerance = 5e-8)
                  kmeans <- suppressWarnings(KMeans(df, show.labels = TRUE))

                  expect_error(print(kmeans), NA)
                  detach(gss)
          })




#
# test_that("DS-998",
#           {
#               library(foreign)
#               suppressWarnings({
#               test.data <- CreateSimulatedClusters(3, 4, 10, 500)
#
#                 z gss <- read.spss("http://docs.displayr.com/images/7/7b/GSS2014.sav", to.data.frame = TRUE)
#                 gss <- flipExampleData::TidySPSS(gss)
#                 attach(gss)
#                 zd = data.frame(confinan, conbus, coneduc, conlabor, conmedic, conpress)
#                 kmeans <- KMeans(zd,
#                                  show.labels = TRUE,
#                                  algorithm = "Bagging",
#                                  missing = "Exclude cases with missing data")
#                 n = 50
# e1071::bclust(zd, iter.base = 100, weights = NULL, centers = 2, base.centers = min(20, n / 5), final.kmeans = FALSE, verbose = FALSE)
#
#                 expect_error(print(kmeans), NA)
#                 detach(gss)
#               })
#           })






