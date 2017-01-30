#
# The analysis below suggests that bagging is worse than k-means.
#
# data("colas", package = "flipExampleData")
#
# assoc <- colas[, match("Q5_5_1", names(colas)):match("Q5_31_7", names(colas))]
# assoc <- assoc[, 1:6]
#
#
# n.clusters = 5
# x <- assoc
# km <- KMeans(x, n.clusters, missing = "Exclude cases with missing data")
# bagged <- KMeans(x, n.clusters, algorithm = "Bagging", missing = "Exclude cases with missing data")
# AdjustedRand(bagged$cluster, km$cluster)
#
#
# set.seed(1223)
# x <- assoc[sample.int(nrow(assoc), nrow(assoc), replace = TRUE), ]
# km1 <- KMeans(x, n.clusters, missing = "Exclude cases with missing data")
# bagged1 <- KMeans(x, n.clusters, algorithm = "Bagging", missing = "Exclude cases with missing data")
#
#
# AdjustedRand(km$cluster, predict(km1, assoc))
# AdjustedRand(bagged$cluster, predict(bagged1, assoc))
