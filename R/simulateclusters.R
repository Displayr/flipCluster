#' \code{CreateSimulatedClusters}
#'
#' Generates artificial clusters from a truncated normal distribution.
#'
#' @param DENSIT A value of 1, 2 or, 3. A 1 indicates that the clusters will be appropximately
#' equal in size. A 2 generates a small cluster and the rest of the same size. A 3 generates a big cluster.
#' @param n.clusters The number of clusters to generate.
#' @param j The number of variables.
#' @param n The number of observations.
#' @references Glenn W. Milligan (1985). An algorithm for generating artificial data sets which contain distinct nonoverlapping clusters is presented. The algorithm is useful for generating test data sets for Monte Carlo validation research conducted on clustering methods or statistics. The algorithm generates data sets which contain either 1, 2, 3, 4, or 5 clusters. By default, the data are embedded in either a 4, 6, or 8 dimensional space. Three different patterns for assigning the points to the clusters are provided. One pattern assigns the points equally to the clusters while the remaining two schemes produce clusters of unequal sizes. Finally, a number of methods for introducing error in the data have been incorporated in the algorithm. Psychometrica. February.
#' @importFrom stats runif
#' @export
CreateSimulatedClusters <- function(DENSIT, n.clusters, j, n)
{

    # From Milligan (1985)
   # DENSITY determines the relative sizes of the clusters based on the input DENSIT and the number of clusters
    DENSITY <- c(25, 25, 16, 17, 17, 12, 12, 13, 13, 10, 10, 10, 10, 10, 5, 45, 5, 22, 23, 5, 15, 15, 15, 5, 11, 11, 11, 12, 30, 20, 30, 10, 10, 30, 6, 7, 7, 30, 5, 5, 5, 5)
    NBOUND <- j*n.clusters*2 #The total number of boundaries - each cluster has 2*j
    BOUNDS <- array(NA,c(2,n.clusters,j))
    MEAN <- array(NA,c(n.clusters,j))
    STDDEV <- array(NA,c(n.clusters,j))
    UNI <- array(NA,n.clusters)
    size <- array(NA,n.clusters)
    LENGTH <- array(runif(j*n.clusters,10,40),c(n.clusters,j))
    CLUST <- array(order(runif(n.clusters)),c(n.clusters,j))#randomly ordering clusters (relevant for DENSIT)
    #COMPUTE CLUSTER BOUNDARIES for DIMENSION 1 - OVERLAP IS NOT
    # PERMITTED. ALSO COMPUTE CENTROID AND STD. DEV.
    LOWER <- 0
    for (I in 1:n.clusters)
    {   UPPER <- LOWER+LENGTH[I,1]
    I1 <- CLUST[I,1]
    BOUNDS[1,I1,1] <- LOWER
    BOUNDS[2,I1,1] <- UPPER
    MEAN[I1,1] <- (UPPER-LOWER)/2 + LOWER
    STDDEV[I1,1] <- LENGTH[I,1]/3
    if (I < n.clusters)
    {    ADD <- (STDDEV[I1,1]+LENGTH[I+1,1]/3)*runif(1,0.25,0.75)
    LOWER <- UPPER+ADD    #ICNT <- ICNT+1
    }
    }#12 CONTINUE
    SAVEBD <- UPPER*0.67 #13
    SAVE <- UPPER
    # COMPUTE CLUSTER BOUNDARIES for THE REMAINING DIMENSIONS - OVERLAP IS
    # PERMITTED. COMPUTE CENTROID AND STD. DEV.
    for (J in 2:j)
    {   for (I in 1:n.clusters)
    {    LOWER <- runif(1,0,SAVEBD)# ICNT <- ICNT+1
    I1 <- CLUST[I,J]
    UPPER <- LOWER + LENGTH[I,J]
    BOUNDS[1,I1,J] <- LOWER
    BOUNDS[2,I1,J] <- UPPER
    MEAN[I1,J] <- (UPPER-LOWER)/2 + LOWER
    STDDEV[I1,J] <- LENGTH[I,J]/3
    }   }#14 CONTINUE #11 CONTINUE
    # SPECifY DENSITY WITHIN CLUSTER AND OUTLIERS
    POINT <- 1
    if (DENSIT == 2) POINT <- 15 else if (DENSIT==3) POINT <- 29
    if (n.clusters == 3)
        POINT <- POINT + 2
    else if (n.clusters == 4) POINT <- POINT + 5
    else if (n.clusters == 5) POINT <- POINT + 9
    KMULT <- n/50
    nSum <- 0
    for (I in 1:n.clusters)
    {   # NOTE CHANGE TO ACCOMODATE THE CHANGE IN THE NUMBER OF POINTS
        # WRITTEN for 50 POINTS - * KMUT for MULTIPLES OF 50 POINTS
        size[I] <- round(DENSITY[POINT] * KMULT)#NOUT[I] <- round(OUTLIR[POINT] * KMULT)
        POINT <- POINT+1
        nSum <- nSum + size[I]
    }#15 CONTINUE
    size[n.clusters] <- size[n.clusters]-(nSum-n)
    x <- array(NA,c(n,j))
    cluster <- rep(1:n.clusters,size)
    for (J in 1:n.clusters)
        for (I in 1:j)
        {   ZZ <- trnorm(size[J],MEAN[J,I],STDDEV[J,I],BOUNDS[1,J,I],BOUNDS[2,J,I])
        x[cluster==J,I] <- ZZ}
    list(x= x, cluster = cluster, size = size)
}


#' @importFrom stats pnorm rnorm
trnorm <- function(n, mean=0, sd=1, lower = -Inf, upper = Inf)
{   # Generate n draws from a truncated normal distribution
    multiplier <- 1.01/(pnorm(upper,mean,sd) - pnorm(lower,mean,sd))
    n.rnorms <- 0
    counter <- 0
    while (n.rnorms < n)
    {   counter <- counter + 1
    n.needed <- n - n.rnorms
    new.result <- rnorm(round(multiplier * (n.needed)) , mean, sd) # Takes several standard normal draws
    new.result <- new.result[new.result >=lower & new.result  <=upper] # Keeps only those draws which fit within the truncation
    n.additional <- length(new.result)
    if (n.additional != 0)
        if (n.needed > n.additional)
            if (counter == 1)
                result <- c(new.result,rep(NA,n-n.additional)) #First time set result
    else
        result[(n.rnorms + 1):(n.rnorms + n.additional)] <- new.result # Subsequent times append new.result to result
    else
        if (counter == 1)
            result <- new.result[1:n]
    else
        result[(n.rnorms + 1):(n.rnorms + n.needed)] <- new.result[1:n.needed]
    n.rnorms <- n.rnorms + n.additional
    }
    result
}




# Add noise to the simuated data according to the reliability formula in Tim's thesis
#' @importFrom stats rnorm sd
SimulateReliability <- function(x,a = 1)
{
    error <- matrix(rnorm(prod(dim(x))),ncol = ncol(x))
    x + sweep(error,2,apply(x,2,sd) * sqrt((1-a)/a),"*")
}

