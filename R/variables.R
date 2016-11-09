#' \code{predict.KMeans}
#'
#' Predicts cluster membership for the entire sample passed into the original analysis (including missing and filtered values).
#' @param object A \code{KMeans} object, or, cluster centers.
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#' If omitted, the data used to fit the model is used.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.lda.
#' The default is to predict \code{NA}.
#' @importFrom stats na.pass
#' @importFrom flipStatistics SumOfSquares
#' @export
predict.KMeans <- function(object, newdata = object$model, ...)
{
    centers <- if (class(object) == "KMeans") object$centers else object
    n.clusters <- nrow(centers)
    n.variables <- ncol(newdata)
    n <- nrow(newdata)
    if (ncol(centers) != n.variables)
        stop("Different number of variables in data to that in centers.")
    distances <- matrix(NA, n, n.clusters)
    for (c in 1:n.clusters)
    {
        center <- as.numeric(centers[c, ])
        diff <- sweep(newdata, 2, center, "-")
        diff[is.na(diff)] <- 0
        distances[, c] <- apply(diff^2, 1, sum)
    }
    apply(distances, 1, which.min)
}


