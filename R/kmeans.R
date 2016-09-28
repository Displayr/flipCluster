#' \code{KMeans}
#'
#' KMeans Cluster Analysis.
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators will be ignored.
#' transformations nor
#' @param data A \code{\link{data.frame}}.
#' @param centers Either the number of clsters (e.g., 2), or a set of initial cluster
#' centers. Where the number of clusters is specified, or the algorithm is 'Bagging',
#' a random selection of rows of data is chosen  as the initial start points.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#'   \code{"Assign partial data to clusters"}. This is the default.
#'   \code{"Imputation"}. This is the default.
#' @param nstart The number of times the algorithm should be run, each time with a different
#' number of start points..
#' @param iter.max The number of iterations of the algorithm to run.
#' @param algorithm One of \code{"Hartigan-Wong"}, \code{"Forgy"}, \code{"Lloyd"}, \code{"MacQueen"},or
#' \code{"Bagging"}.
#' @param output One of \code{"Means"}, \code{"Confusion matrix"}, or \code{"Detail"}.
#' @param w.size If the data is weighted, this is the size of the bootstrap sample that is created by
#' selection with replacement in proportion to the weights.
#' @param seed The random number seed used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Additional arguments to be applied to \code{bclust}.
#' @details \code{"Bagging"} runs the cluster analysis with boostrap samples, and then applies hierarchical clsuter
#' analysis find clusters of cluster centers (Leisch 1999).

#' @references
#' Forgy, E. W. (1965) Cluster analysis of multivariate data: efficiency vs interpretability of classifications. Biometrics 21, 768–769.
#' Hartigan, J. A. and Wong, M. A. (1979). A K-means clustering algorithm. Applied Statistics 28, 100–108.
#' Leisch, Friedrich (1999)  Bagged clustering. Working Paper 51, SFB “Adaptive Information Systems and Modeling in Economics and Management Science”, August 1999. http://epub.wu.ac.at/1272/ 1/document.pdf
#' Lloyd, S. P. (1957, 1982) Least squares quantization in PCM. Technical Note, Bell Laboratories. Published in 1982 in IEEE Transactions on Information Theory 28, 128–137.
#' MacQueen, J. (1967) Some methods for classification and analysis of multivariate observations. In Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability, eds L. M. Le Cam & J. Neyman, 1, pp. 281–297. Berkeley, CA: University of California Press.
#
#' @importFrom flipData CleanSubset CleanWeights EstimationData
#' @importFrom flipFormat Labels
#' @importFrom flipTransformations CreatingFactorDependentVariableIfNecessary AsNumeric Factor
#' @importFrom flipU OutcomeName
#' @importFrom stats aggregate
#' @importFrom flipRegression ConfusionMatrix
#' @importFrom e1071 bclust
#' @importFrom flipStatistics Mean MeanByGroup SumOfSquaresByGroup
#' @export
KMeans <- function(data = NULL,
                   centers = 2,
                   subset = NULL,
                   weights = NULL,
                   missing = "Exclude cases with missing data",
                   iter.max = 1000,
                   nstart= 10,
                   algorithm = "Hartigan-Wong",
                   output = "Means",
                   w.size = 10000,
                   seed = 12321,
                   binary = FALSE,
                   show.labels = FALSE)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    has.subset <- !is.null(subset)
    partial <- missing == "Assign partial data to clusters"
    if (has.subset)
    {
        subset <- eval(substitute(subset), data, parent.frame())
        subset.description <- if (is.null(substitute(subset))) NULL else deparse(substitute(subset))
        if (!is.null(subset.description))
            attr(subset, "description") <- subset.description
        if (length(subset) > 1 & length(subset) != nrow(data))
            stop("'subset' and 'data' are required to have the same number of observations. They do not.")
        if (partial)
        {
            subset <- CleanSubset(subset, n.total)
            n.subset <- attr(subset, "n.subset")
            original.subset <- subset
        }
    }
    weighted <- !is.null(weights)
    if(weighted)
    {
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- deparse(substitute(weights))
        weights <- eval(substitute(weights), data, parent.frame())
        if (length(weights) != nrow(data))
            stop("'weights' and 'data' are required to have the same number of observations. They do not.")
        if (partial)
            original.weights <- weights <- CleanWeights(weights)
    }
    row.names <- rownames(data)
    ####################################################################
    ##### Data manipulation specific to kmeans                       #####
    ####################################################################
    # Making categorical variables numeric.
    data <- AsNumeric(data, binary)
    # Treatment of missing values.
    input.formula <- as.formula(paste0("~", paste(names(data), collapse = "+")))
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n <- nrow(.estimation.data)
    if (n < ncol(.estimation.data) + 1)
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
    .weights <- processed.data$weights
    x <- if (is.null(weights))
        .estimation.data
    else
        AdjustDataToReflectWeights(.estimation.data, .weights / sum(.weights) * w.size)

    ####################################################################
    ##### Fitting the model.                                       #####
    ####################################################################
    n.clusters <- if (length(centers) > 1) nrow(centers) else centers
    model <- if (algorithm == "Bagging")
        bclust(x, n.clusters, base.centers = n.clusters, final.kmeans = FALSE)
    else
        kmeans(x, centers, iter.max, nstart, algorithm)
    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # Setting the class.
    result <- list()
    class(result) <- "KMeans"
    # Saving data - generally applicable.
    if (missing == "Imputation (replace missing values with estimates)")
        data <- processed.data$data
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    result$cluster <- predict(model$cluster, data)
    if (partial)
        n <- sum(original.subset)
    else
        result$cluster[!subset] <- model$cluster # This will often do nothing.
    analysis.subset <- if(partial) original.subset else subset
    weights.a <- weights[analysis.subset]
    data.a <- data[analysis.subset, ]
    clusters.a <- result$cluster[analysis.subset]
    if (missing == "Assign partial data to clusters" | weighted)
        result$centers <- MeanByGroup(data.a, clusters.a, weights.a)
    if (weighted)
    {   # Essentially an extra iteration
        result$cluster <- clusters.a <- predict.kmeans(result$centers, data.a)
        result$centers <- MeanByGroup(data.a, clusters.a, weights.a)
    }
    # Goodness-of-fit
    tss <- sum(SumOfSquares(data.a))
    rss <- sum(SumOfSquaresByGroup(data.a, clusters.a, weights.a))
    ess <- tss - rss
    result$omega.squared = ess/tss
    result$calinski.harabasz = (ess/(n.clusters - 1)/(rss/(n - n.clusters)))
    # Saving descriptive information.
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$n.clusters <- n.clusters
    result$estimation.data <- .estimation.data
    # Replacing names with labels
    if (result$show.labels <- show.labels)
    {
        result$outcome.label <- outcome.label
        variable.labels <- Labels(data)
        # Removing the outcome variable
        result$variable.labels <- variable.labels <- variable.labels[-match(outcome.label, variable.labels)]
        colnames(result$original$means) <- variable.labels
        row.names(result$original$scaling) <- variable.labels
    }
    # Saving parameters
    result$output <- output
    result$missing <- missing
    result
}


# revised kmeans to fix problem


#kmeans(zX,zC,nstart=1)
#kmeans2(zX,zC)



