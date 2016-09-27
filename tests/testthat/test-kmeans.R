context("k-means")


for (alg in c("Hartigan-Wong", "Forgy", "Lloyd", "MacQueen", "Bagging"))
test_that("K-means algorithm",
          {
              set.seed(1)
              test.data <- CreateSimulatedClusters(3, 4, 10, 100)
              z <- KMeans(as.data.frame(test.data$x), 4, algorithm = alg)
              expect_equal(AdjustedRand(test.data$cluster, z$original$cluster), 1)

          })
