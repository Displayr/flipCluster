context("Allocate to clusters (predict)")

test_that("Allocate to clusters",
          {
              df <- cbind(c(1:4,1), NA)
              cl <- matrix(c(1:4, 4:1), 4, 2)
              expect_equal(predict.KMeans(cl, df), c(1,2,3,4,1))
              expect_equal(predict.KMeans(cl, df[, 2:1] ), c(4,3,2,1,4))

          })





for (alg in c("Hartigan-Wong", "Forgy", "Lloyd", "MacQueen", "Bagging", "Batch"))
    for (dens in 1:3)
        for (n in 200)
            test_that(paste("predict algorithm", alg, " density =", dens),
            {
                set.seed(121)
                #alg = "Hartigan-Wong"
                #dens = 3
                test.data <- CreateSimulatedClusters(dens, 4, 10, n)
                sb <- runif(n) < .9
                z <- suppressWarnings(KMeans(as.data.frame(test.data$x), 4, subset = sb, n.starts = 10, algorithm = alg, seed = 1112))
                #AdjustedRand(test.data$cluster, z$cluster)

                expect_equal(AdjustedRand(test.data$cluster, predict(z)), 1)
          })


test_that("Missing data",
        {
                data("consultant", package = "flipExampleData")
                nms <- names(consultant)
                dat <- consultant[, match("Q050__1", nms):match("Q050__25", nms) ]
                attach(dat)
                zd = data.frame(Q050__15, Q050__1, Q050__5, Q050__13, Q050__14, Q050__3, Q050__17, Q050__18, Q050__19, Q050__20, Q050__24)
                z <- suppressWarnings(KMeans(zd, 2))
                expect_equal(sum(is.na(predict(z))), 4)
                detach(dat)
        })


