context("Bugs")

test_that("DS-975",
          {
              library(foreign)
              suppressWarnings({
                gss <- read.spss("http://docs.displayr.com/images/7/7b/GSS2014.sav", to.data.frame = TRUE, max.value.labels = 20)
                gss <- flipExampleData::TidySPSS(gss)
                attach(gss)
                kmeans <- KMeans(data.frame(confinan, conbus, coneduc, conlabor, conmedic, conpress), show.labels = TRUE)
                expect_error(print(kmeans), NA)
                detach(gss)
              })
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






