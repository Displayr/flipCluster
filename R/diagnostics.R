#' \code{CalinskiHarabasz}
#'
#' Statistic for evaluating the fit of a cluster analysis (higher is better).
#' KMeans Cluster Analysis.
#' @param x A matrix containing the data.
#' @param centers The cluster centers.
#' @param clusters Cluster membershis by case
#' @importFrom stats var
#' @importFrom verbs Sum
#' @export

CalinskiHarabasz <- function(x, centers, clusters)
{
  n <- nrow(x)
  T <- Sum(apply(x,2,var)*(n-1), remove.missing = FALSE)
  W <- Sum((x - centers[clusters,])^2, remove.missing = FALSE)
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
#' @importFrom verbs Sum SumEachRow SumEachColumn
#' @export
ExternalIndices <- function(k, Classification1, Classification2)
{
    n <- length(Classification1)
    Comparison <- matrix(0, k, k)
    for (i in 1:n) {
        Comparison[Classification1[i], Classification2[i]] <- Comparison[Classification1[i], Classification2[i]] + 1
    }
    Z <- Sum(Comparison^2, remove.missing = FALSE)
    # FM
    nDotj <- SumEachColumn(Comparison, remove.missing = FALSE)
    niDot <- SumEachRow(Comparison, remove.missing = FALSE)
    Denominator <- (Sum(choose(nDotj, 2), remove.missing = FALSE)*Sum(choose(niDot, 2), remove.missing = FALSE))^(1/2)
    Numerator <- (1/2)*(Z - n)
    FM <- Numerator/Denominator
    #Jaccard
    jaccard = (Z - n) / (Sum(nDotj^2, remove.missing = FALSE) + Sum(niDot^2, remove.missing = FALSE) - Z - n)
    list(Fowlkes.Mallows = FM, Jaccard = jaccard, adjusted.rand = AdjustedRand(Classification1, Classification2))
}
