context("k-means")

test_that("Variable names",
{
    m <- mtcars
    colnames(m)[1] <- "m/g"
    expect_error(KMeans(m), NA)
})


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
                expect_error(KMeans(as.data.frame(dat), 2, algorithm = "batch", weights = NULL, seed = NULL))
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

# Subset and weights with one weight equal to 0 (DS-2570)
wgt2 <- wgt
wgt2[2] <- 0
suppressWarnings(KMeans(data = dat, subset = sb, weights = wgt2, show.labels = TRUE, centers = 3))

# Subset with one row completely missing (DS-2570)
dat2 <- dat
dat2[2, ] <- NA
suppressWarnings(KMeans(data = dat2, subset = sb, show.labels = TRUE, centers = 3))

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
# expect_warning fails in Travis, hence tryCatch on the line below
tryCatch(KMeans(data = meanDat, missing = missing, show.labels = TRUE, centers = 3),
         warning = function(w){
             if (!grepl("Imputation has been selected, but the data has no missing values.", w$message))
                 stop("OneWayAnova(...) did not throw a warning.")
         })

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

test_that("warnings and errors",{
    set.seed(2)
    clusters <- rep(1:3, c(20, 20, 20))
    data <- data.frame(A = c(1, 2.1, 3)[clusters], B = 1)
    # Warning and errors for bagging with small sample size
    expect_error(suppressWarnings(KMeans(data, 2, algorithm = "Bagging")))
    # Subset wrong size
    expect_error(suppressWarnings(KMeans(data, 2, subset = rep(TRUE, 2))))
    # Data set too small
    expect_error(suppressWarnings(KMeans(data[1, , drop = FALSE], 2)))
})

test_that("Data provided as a list",{
    set.seed(2)
    clusters <- rep(1:3, c(20, 20, 20))
    data <- list(a = data.frame(A = c(1, 2.1, 3)[clusters]), b = data.frame(B = rep(1, 60)))
    attr(data[[1]], "questiontype") = "Number"
    attr(data[[2]], "questiontype") = "Number"
    expect_error(suppressWarnings(KMeans(data, 2)), NA)
})

test_that("Excel exporting", {
    expected <- structure(c(3.15859030837005, 3.72649572649573, 3.65737051792829,
                3.84955752212389, 3.0204081632653, 3.49781659388647, 4.22685185185185,
                3.37556561085973, 2.9070796460177, 4.19047619047619, 4.16744186046512,
                4.11864406779661, 3.79279279279279, 3.53586497890296, 4.32876712328767,
                4.22413793103448, 3.97844827586207, 2.71304347826087, 2.34123222748816,
                3.87719298245614, 4.43192488262911, 3.16748768472907, 2.62162162162162,
                2.09691629955947, 3.16033755274262, 4.21912350597609, 4.34272300469484,
                3.5414847161572, 4.48339483394834, 3.956, 4.54237288135593, 4.60642570281125,
                4.28163265306122, 3.88475836431227, 4.66920152091255, 4.68444444444444,
                4.55938697318008, 4.21428571428571, 4.46530612244898, 4.61240310077519,
                4.41532258064516, 4.65948275862069, 3.86311787072243, 3.57254901960784,
                4.44615384615385, 4.81007751937985, 4.0722433460076, 3.72332015810277,
                2.96484375, 3.94560669456067, 0.310838841885796, 0.140755318241098,
                0.00323574178965025, 0.141424294446759, 0.235600051871481, 0.282659487001124,
                0.0764502778940017, 0.203479663924795, 0.222392573859815, 0.109232783455807,
                0.12779437265589, 0.0850459755732405, 0.0643454816056137, 0.262816362256843,
                0.0443027443443114, 0.0146557799624897, 0.155140698021279, 0.321732247606426,
                0.329737097841614, 0.130154042395525, 0.083993051991418, 0.223445801697439,
                0.228398125265116, 0.14047588381469, 0.173237171309304, 0, 2.31296463463574e-16,
                0.222389659151817, 0, 0, 0, 1.37523603616074e-09, 0, 0, 4.90348502542777e-14,
                1.0870933782788e-14, 3.50506660732701e-11, 2.23377039088035e-08,
                0, 3.68257368303186e-06, 0.00825709174210691, 0, 0, 0, 2.31296463463574e-16,
                1.50472458567326e-10, 0, 0, 0, 0), .Dim = c(25L, 4L), .Dimnames = list(
                    c("Q050__1", "Q050__2", "Q050__3", "Q050__4", "Q050__5",
                      "Q050__6", "Q050__7", "Q050__8", "Q050__9", "Q050__10", "Q050__11",
                      "Q050__12", "Q050__13", "Q050__14", "Q050__15", "Q050__16",
                      "Q050__17", "Q050__18", "Q050__19", "Q050__20", "Q050__21",
                      "Q050__22", "Q050__23", "Q050__24", "Q050__25"),
                    c("Cluster 1 47%", "Cluster 2 53%", "R_Squared", "p")))

    suppressWarnings(result <- KMeans(dat))
    expect_equal(attr(result, "ChartData"), expected)
})
