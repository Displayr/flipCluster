#
# selectkMeans <- function(x, min.n.clusters = 2,max.n.clusters = 8, init.centers = NULL, n.starts = 100, iter.max = 100, ignore.error = FALSE,n.tests = 1000, assumption = "sample",save = TRUE)
# {x <- as.matrix(x)
# result <- statistics <- matrix(NA,nrow = max.n.clusters - min.n.clusters + 1, ncol = 15,
#       dimnames = list(clusters = min.n.clusters:max.n.clusters,c("omega.square","Calinski.Harabasz","Split.1","Split.20","Split.100","Split.1000","Rep.1","Rep.20","Rep.100","Rep.1000","Rep.5.Med","Rep.20.Med","Rep.100.Med","Rep.1000.Med","Split.Traditional")))
# for (i in min.n.clusters:max.n.clusters)
# {
#     kMeans.solution <- kmeans2(x, i, iter.max, n.starts)#kMeans(x, n.clusters = i, n.starts = n.starts , iter.max = iter.max)
#     kMeans.summary <- summary(kMeans.solution,x) #summary.kMeans(kMeans.solution)
#     r <- i - min.n.clusters + 1
#     result[r,1] <- kMeans.summary$omega.squared
#     result[r,2] <- kMeans.summary$Calinski.Harabasz
#     replic <- replicability(kMeans.solution, x, iter.max = 100, n.tests = n.tests, assumption = "sample", method="split-half")
#     result[r,3] <- sample(replic$all,1)
#     result[r,4] <- quantile(sample(replic$all,20),0.05)
#     result[r,5] <- quantile(sample(replic$all,100),0.05)
#     result[r,6] <- replic$statistic
#     split.replic <- replic
#     replic <- replicability(kMeans.solution, x,  iter.max = 100, n.tests = n.tests, assumption = "sample",method="bootstrap")
# #replicability <- function(object, x, iter.max = 100, n.tests = 1000, nstart = 100, nstart.classify = 1, assumption = "sample", method = "bootstrap")
#     result[r,7] <- sample(replic$all,1)
#     result[r,8] <- quantile(sample(replic$all,20),0.05)
#     result[r,9] <- quantile(sample(replic$all,100),0.05)
#     result[r,10] <- replic$statistic
#     result[r,11] <- quantile(sample(replic$all,5),0.5)
#     result[r,12] <- quantile(sample(replic$all,20),0.5)
#     result[r,13] <- quantile(sample(replic$all,100),0.5)
#     result[r,14] <- quantile(replic$all,0.5)
#     result[r,15] <- split.half.replication(kMeans.solution,x, iter.max,n.starts)}
# list(split.replic=split.replic,replic =replic, table = result)}
#
# #zX <- matrix(runif(1000),ncol=4)
# #selectkMeans(zX)
#
