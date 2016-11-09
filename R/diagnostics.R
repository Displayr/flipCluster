#' \code{CalinskiHarabasz}
#'
#' Statistic for evaluating the fit of a cluster analysis (higher is better).
#' KMeans Cluster Analysis.
#' @param x A matrix containing the data.
#' @param centers The cluster centers.
#' @param clusters Cluster membershis by case
#' @importFrom stats var
#' @export

CalinskiHarabasz <- function(x, centers, clusters)
{
  n <- nrow(x)
  T <- sum(apply(x,2,var)*(n-1))
  W <- sum((x - centers[clusters,])^2)
  B <- T - W
  i <- nrow(centers)
  CH <- (B/(i-1))/(W/(n - i))
  list(T=T,B=B,W=W,Calinski.Harabasz=CH)
}

#' AdjustedRand
#'
#' Checks the similarity of two classifications: 1 indicates perfect, 0 indicates as if random.
#' @param trial.classification One classification.
#' @param test.classification Another classification.
#' @importFrom e1071  classAgreement
#' @export
AdjustedRand <- function(trial.classification,test.classification)
{
    classAgreement(table(trial.classification,test.classification))$crand
}

#' ExternalIndices
#'
#' Measures of classification quality.
#' @param k Number of clusters.
#' @param Classification1 One classification.
#' @param Classification2 Another classification.
#' @importFrom e1071  classAgreement
#' @export
ExternalIndices <- function(k, Classification1, Classification2)
{
    n <- length(Classification1)
    Comparison <- matrix(0, k, k)
    for (i in 1:n) {
        Comparison[Classification1[i], Classification2[i]] <- Comparison[Classification1[i], Classification2[i]] + 1
    }
    Z <- sum(Comparison^2)
    # FM
    nDotj <- colSums(Comparison)
    niDot <- rowSums(Comparison)
    Denominator <- (sum(choose(nDotj, 2))*sum(choose(niDot, 2)))^(1/2)
    Numerator <- (1/2)*(Z - n)
    FM <- Numerator/Denominator
    #Jaccard
    jaccard = (Z - n) / (sum(nDotj^2) + sum(niDot^2) - Z - n)
    list(Fowlkes.Mallows = FM, Jaccard = jaccard, adjusted.rand = AdjustedRand(Classification1, Classification2))
}
