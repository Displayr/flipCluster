context("Allocate to clusters")

test_that("Allocate to clusters",
          {
              df <- cbind(c(1:4,1), NA)
              cl <- matrix(c(1:4, 4:1), 4, 2)
              expect_equal(predict.KMeans(cl, df), c(1,2,3,4,1))
              expect_equal(predict.KMeans(cl, df[, 2:1] ), c(4,3,2,1,4))

          })
