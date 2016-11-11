context("k-means")


for (alg in c("Hartigan-Wong", "Forgy", "Lloyd", "MacQueen", "Bagging", "Batch"))
    for (dens in 1:3)
        for (n in 200)
test_that(paste("K-means algorithm", alg, " density =", dens),
          {
              set.seed(121)
              #alg = "Hartigan-Wong"
              #dens = 3
              test.data <- CreateSimulatedClusters(dens, 4, 10, n)
              z <- suppressWarnings(KMeans(as.data.frame(test.data$x), 4, n.starts = 10, algorithm = alg, seed = 1112))
              #AdjustedRand(test.data$cluster, z$cluster)

              expect_equal(AdjustedRand(test.data$cluster, z$cluster), 1)
          })


for (alg in c("Batch", "Bagging"))
    for (dens in 1:3)
        for (n in 200)
test_that(paste("Weighted K-means algorithm", alg, " density =", dens),
          {
              set.seed(23)
              test.data <- CreateSimulatedClusters(dens, 4, 10, n)
              weights <- runif(n) #Should do nothing; just checking that no computational errors arise
              z <- KMeans(as.data.frame(test.data$x), 4, algorithm = alg, weights = weights, seed = NULL)
              a <- AdjustedRand(test.data$cluster, z$cluster)
              expect_equal(a, 1)
          })

test_that("Error with batch",
          {
                dat <- data.frame(a = 1:10, b = 1:10)
                expect_error(KMeans(as.data.frame(dat), 2, algorithm = alg, weights = NULL, seed = NULL))
          }
          )


alg = "Batch"#Bagging"#, #MacQueen"  Batch"#
for (alg in c("Hartigan-Wong", "Forgy", "Lloyd", "MacQueen","Batch"))
    test_that(paste("Weighting", alg),
              {
                  set.seed(2)
                  clusters <- rep(1:3, c(20, 20, 20))
                  data <- data.frame(A = c(1, 2.1, 3)[clusters], B = 1)
                  expected.clusters.weighted <- c(1, 1, 2)[clusters]
                  expected.clusters.unweighted <- c(1, 2, 2)[clusters]
                  wgt <- c(1, 1, 10)[clusters]
                  solution <- suppressWarnings(KMeans(data, 2, algorithm = alg))$cluster
                  expect_equal(1, AdjustedRand(solution, expected.clusters.unweighted))
                  solution <- suppressWarnings(KMeans(data, 2, algorithm = alg, weights = wgt))$cluster
                  if (alg == "Batch")
                      expect_equal(1, AdjustedRand(solution, expected.clusters.weighted))
                  else
                      expect_false(1 == AdjustedRand(solution, expected.clusters.weighted))
              })

for (alg in c("Hartigan-Wong", "Forgy", "Lloyd", "MacQueen", "Batch"))
    test_that(paste("Missing data", alg),
              {
                  set.seed(1)
                  clusters <- c(1, 1, 2, 2, 3, 3, 3, 3)
                  data <- data.frame(A = c(1, 1.1, 4, 4.1, 1, NA, 1, 1),
                                     B = c(2, 2.2, 3, 3.1, 2, 2, 2, NA),
                                     C = c(3, 3, 2, 2, NA, 2, 2, 2),
                                     D = c(4, 4, 1, 1, 1, 1, NA, 1))
                  if (alg == "Batch")
                      expect_equal(1, AdjustedRand(suppressWarnings(KMeans(data, 3, algorithm = alg))$cluster, clusters))
                  else
                      expect_error(suppressWarnings(KMeans(data, 3, algorithm = alg))$cluster)
              })


data("consultant", package = "flipExampleData")
nms <- names(consultant)
dat <- consultant[, match("Q050__1", nms):match("Q050__25", nms) ]
for (alg in c("Hartigan-Wong", "Forgy", "Lloyd", "MacQueen", "Batch", "Bagging"))
    test_that(paste("Missing data", alg),
              {
                  set.seed(1)
                  if (alg == "Batch")
                      expect_error(suppressWarnings(KMeans(dat, 2, algorithm = alg)), NA)
                  else
                      expect_error(suppressWarnings(KMeans(dat, 2, algorithm = alg)))
              })


test_that("Options", {

# Defaults
suppressWarnings(KMeans(data = dat))

# Labels
suppressWarnings(KMeans(data = dat, show.labels = TRUE))

# number of clusters
suppressWarnings(KMeans(data = dat, show.labels = TRUE, centers = 3))

# Subset
sb <- dat$Q050__1 != "Very Important 5"
suppressWarnings(KMeans(data = dat, subset = sb, show.labels = TRUE, centers = 3))
suppressWarnings(KMeans(data = dat, subset = TRUE))#, show.labels = TRUE, centers = 3))

# weights
set.seed(1)
wgt <- runif(length(sb))
suppressWarnings(KMeans(data = dat, weights = wgt, show.labels = TRUE, centers = 3))

# Subset and weights
suppressWarnings(KMeans(data = dat, subset = sb, weights = wgt, show.labels = TRUE, centers = 3))

# Creating a data set with mean imputation
meanDat <- as.data.frame(lapply(dat, unclass))
mn <- matrix(apply(meanDat, 2, mean, na.rm = TRUE), byrow = TRUE, ncol = 25, nrow = nrow(meanDat))
meanDat[is.na(meanDat)] <- mn[is.na(meanDat)]

missing = "Error if missing data"
expect_error(suppressWarnings(KMeans(data = dat, missing = missing, show.labels = TRUE, centers = 3)))
KMeans(data = meanDat, missing = missing, show.labels = TRUE, centers = 3)

missing = "Exclude cases with missing data"
expect_error(suppressWarnings(KMeans(data = dat, missing = missing, show.labels = TRUE, centers = 3)))
KMeans(data = meanDat, missing = missing, show.labels = TRUE, centers = 3)

missing = "Use partial data"
suppressWarnings(KMeans(data = dat, missing = missing, show.labels = TRUE, centers = 3))
KMeans(data = meanDat, missing = missing, show.labels = TRUE, centers = 3)

missing = "Imputation (replace missing values with estimates)"
suppressWarnings(KMeans(data = dat, missing = missing, show.labels = TRUE, centers = 3))
expect_error(KMeans(data = meanDat, missing = missing, show.labels = TRUE, centers = 3))

# iter.max does something
t <- system.time(suppressWarnings(KMeans(data = dat, subset = sb,iter.max = 10,  weights = wgt, show.labels = TRUE, centers = 3)))
t1 <- system.time(suppressWarnings(KMeans(data = dat, subset = sb,iter.max = 1,  weights = wgt, show.labels = TRUE, centers = 3)))
expect_true(t[[1]] > t1[[1]])

# n.starts  does something
t <- system.time(suppressWarnings(KMeans(data = dat, subset = sb,n.starts = 10,  weights = wgt, show.labels = TRUE, centers = 3)))
t1 <- system.time(suppressWarnings(KMeans(data = dat, subset = sb,n.starts = 1,  weights = wgt, show.labels = TRUE, centers = 3)))
expect_true(t[[1]] > t1[[1]])

# Output
z <- suppressWarnings(KMeans(data = dat, subset = sb,output = "Means",  show.labels = TRUE, centers = 5))
expect_true(!is.matrix(z))
z1 <- suppressWarnings(KMeans(data = dat, subset = sb, output = "Means table", show.labels = TRUE, centers = 5))
expect_true(is.matrix(z1))

# Seed - equal
set.seed(14)
zz <- as.data.frame(matrix(runif(1000), ncol = 10))

z <- suppressWarnings(KMeans(data = zz, iter.max = 2, n.starts = 1))
z1 <- suppressWarnings(KMeans(data = zz, iter.max = 2, n.starts = 1))
expect_equal(z$cluster, z1$cluster)

set.seed(15)
zz <- as.data.frame(matrix(runif(1000), ncol = 10))
z2 <- suppressWarnings(KMeans(data = zz, iter.max = 2, n.starts = 1))
z21 <- suppressWarnings(KMeans(data = zz, iter.max = 2, n.starts = 1))
expect_equal(z2$cluster, z21$cluster)

expect_true(!all(z$cluster == z2$cluster))

})
