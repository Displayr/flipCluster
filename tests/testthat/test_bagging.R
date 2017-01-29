data("colas", package = "flipExampleData")

assoc <- colas[, match("Q5_5_1", names(colas)):match("Q5_31_7", names(colas))]
assoc <- assoc[, 1:6]

x <- assoc
km <- KMeans(x, 2, missing = "Exclude cases with missing data")
bagged <- KMeans(x, 2, algorithm = "Bagging", missing = "Exclude cases with missing data")
AdjustedRand(bagged$cluster, km$cluster)


set.seed(1223)
x <- assoc[sample.int(nrow(assoc), nrow(assoc), replace = TRUE), ]
km1 <- KMeans(x, 2, missing = "Exclude cases with missing data")
bagged1 <- KMeans(x, 2, algorithm = "Bagging", missing = "Exclude cases with missing data")


AdjustedRand(bagged1$cluster, km1$cluster)


AdjustedRand(predict(bagged1, assoc), km1$cluster)
