# # Compute the replicability statistic based on the split-half method
# SplitHalfReplication <- function(x, n.clusters,iter.max = 100, nstart = 1000)
# {
#     n <- nrow(x)
#     j <- ncol(x)
#
#     # Split data into two samples
#     sample.index <- sample(1:n,round(.5*n),FALSE)
#     x.1 <- x[sample.index,]
#     x.2 <- x[-sample.index,]
#
#
#     n.1 <- length(sample.index)
#
#     # Use k-means algorithm to classify the data according to the desired number of clusters
#     class.1 <- kmeans2(x.1, n.clusters , iter.max, nstart)
#     class.2 <- kmeans2(x.2, n.clusters, iter.max, nstart)
#
#     # Re-classify both sets of data according to the cluster centers found in the original classification for the other cluster
#     # ie re-classify data set 1 based on the cluster centres found for data set 2
#     cluster.1 <- predict.Kmeans(class.2$centers, x.1)
#     cluster.2 <- predict.Kmeans(class.1$centers, x.2)
#
#     # The output statistic is the average of the two adjusted rand statistics which compare the two classifications of each data set
#     output <- (adjusted.rand(class.1$cluster,cluster.1) + adjusted.rand(class.2$cluster,cluster.2))/2
# }
#
#
#
# # Compute the replicability statistic based on the ACR method (adjusted classification replicability)
# BootstrapReplication <- function(data, n.clusters, iter.max = 100, nstart = 100, test.set.fraction = 1)
# {
#     n <- nrow(data)
#     if (test.set.fraction == 1)
#     {
#         training.data <- data
#         test.data <- data[sample(1:n,n,replace = TRUE),]
#     }
#     else
#     {
#         test.sample <- sample(1:n,round(n*test.set.fraction), replace = FALSE)
#         training.data <- data[-test.sample,]
#         test.data <- data[test.sample,]
#     }
#
#     #   clustering
#     training.kmeans <- KMeans(data = training.data, n.clusters, iter.max = iter.max, nstart = ntart)
#     test.kmeans <- KMeans(data = test.data, n.clusters, iter.max = iter.max, nstart = ntart)
#
#     # reclassify test data based on the "training" cluster centers
#     test.kmeans.reclassified <- predict.Kmeans(training.kmeans$centers, test.data)
#
#     # Output adjusted Rand statistic
#     output <- AdjustedRand(test.kmeans$cluster, test.kmeans.reclassified)
# }
#
#
# # This function repeats the resampling protocol Q times and returns the 5th percentile of the adjusted rand statistics
# ACR <- function(Q,data, n.clusters, iter.max = 100, nstart = 100, test.set.fraction = 1)
# {
#     if (Q==1)
#         output <- resampling.replication(data, n.clusters, iter.max = 100, nstart = 100, test.set.fraction = 1)
#     else
#     {
#         # Set up an empty array to store each of the Q rand stats
#         results <- vector("numeric",Q)
#         for (k in 1:Q)
#             results[k] <- BoostrapReplication(data, n.clusters, iter.max = 100, nstart = 100, test.set.fraction = 1)
#
#         # Calculate the 5th percentile
#         output <- quantile(results,0.05)
#     }
# }
#
#
# acr.compute <- function(replicability, number.clusters,  beta,lambda)
# {
#     if (lambda == 0)
#         penalty <- beta*log(number.clusters)
#     else
#         penalty <- beta*((number.clusters-1)/lambda)
#     replicability + penalty}
#
#
# acr.predict <- function(replicability, number.clusters,  beta,lambda)
# {
#     which.max(acr.compute(replicability, number.clusters,  beta,lambda))+1}
#
# acr.predict.batch <- function(replicability, number.clusters,  beta,lambda)
# {sapply(replicability,function(x) acr.predict(x,number.clusters,beta,lambda))}
#
# hitrate.acr <- function(par, true.clusters, replicability, number.clusters)
# {    sum(true.clusters == acr.predict.batch(replicability,number.clusters,par[1],par[2]))}
#
# optimize.acr <- function(true.cluster, replicability, number.clusters, start.par = c(0.09,0), maxit=1000)
# {
#     results <- optim(start.par,hitrate.acr,true.clusters = true.cluster, number.clusters = number.clusters, replicability = replicability, control = list(fnscale = -1, maxit=maxit), method = "SANN")
#     beta = results$par[1]
#     lambda = results$par[2]
#     predicted <- acr.predict.batch(replicability,number.clusters,beta,lambda)
#     a.rand <- adjusted.rand(predicted,true.cluster)
#     confusion <- xtabs(~predicted+true.cluster)
#     hit.rate <- results$value/length(true.cluster)
#     penalty <- acr.compute(rep(1,length(number.clusters)), number.clusters,  beta,lambda)
#     list(predicted = predicted, penalty = penalty, confusion = confusion, a.rand = a.rand, hits = results$value, hit.rate = hit.rate, beta = beta, lambda = lambda)}
#
