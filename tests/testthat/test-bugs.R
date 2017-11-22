context("Bugs")

test_that("DS-975",
{
    ## suppressWarnings(gss.raw <- foreign::read.spss("https://docs.displayr.com/images/7/7b/GSS2014.sav",
    ##                  to.data.frame = TRUE, max.value.labels = 1000))
    ## expect_equal(dim(gss.raw), c(3842, 380))
    ## expect_equal(as.character(gss.raw$confinan[1:3]), c("ONLY SOME", "A GREAT DEAL", NA))
    ## expect_equal(levels(gss.raw$confinan)[1:3], c("A GREAT DEAL", "ONLY SOME", "HARDLY ANY"))
    data(gss, package = "flipCluster")

    gss.tidy <- flipExampleData::TidySPSS(gss)
    zz = suppressWarnings(flipTransformations::AsNumeric(gss.tidy, binary = FALSE))
    # Checking the raw data coming out of AsNumeric
    expect_equal(zz[1:3,1], c(2, 1, NA))
    expect_equal(as.numeric(zz[1,]), c(2, 1, 2, 3, 2, 2))
    # Checking the first two moments of the variables
    z = suppressWarnings(apply(zz, 2, mean, na.rm = TRUE))
    expect_equal(unname(z), c(2.189400, 2.018562, 1.943903, 2.183138, 1.727344, 2.368917),
                 tolerance = 5e-7)
    z = suppressWarnings(apply(zz, 2, sd, na.rm = TRUE))
    expect_equal(unname(z), c(0.6434409, 0.5976801, 0.6378311, 0.6168390, 0.6404637,
                              0.6238356), tolerance = 5e-8)
    kmeans <- suppressWarnings(KMeans(gss.tidy, show.labels = TRUE))

    expect_error(print(kmeans), NA)
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






