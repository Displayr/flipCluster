#' \code{KMeans}
#'
#' KMeans Cluster Analysis.
#' @param data A \code{\link{data.frame}}.
#' @param centers Either the number of clusters (e.g., 2), or a set of initial cluster
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
#'   \code{"Use partial data"}. This is the default.
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param n.starts The number of times the algorithm should be run, each time with a different
#' number of start points.
#' @param iter.max The number of iterations of the algorithm to run.
#' @param algorithm One of \code{"Hartigan-Wong"}, \code{"Forgy"}, \code{"Lloyd"}, \code{"MacQueen"},\code{"Batch"}, or
#' \code{"Bagging"}.
#' @param output The defaults is \code{"Means"}. A table that is better for exporting is \code{"Means table"}.
#' @param seed The random number seed used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param binary Makes categorical variables into indicator variables (otherwise their values are used).
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
#' @importFrom flipFormat Labels ExtractCommonPrefix Names FormatAsPercent
#' @importFrom flipTransformations CreatingFactorDependentVariableIfNecessary AsNumeric Factor AdjustDataToReflectWeights
#' @importFrom flipU OutcomeName
#' @importFrom stats aggregate complete.cases as.formula kmeans
#' @importFrom flipRegression ConfusionMatrix
#' @importFrom e1071 bclust
#' @importFrom flipStatistics Mean MeanByGroup Frequency
#' @export
KMeans <- function(data = NULL,
                   centers = 2,
                   subset = NULL,
                   weights = NULL,
                   missing = "Use partial data",
                   iter.max = 100,
                   n.starts = 10,
                   algorithm = "Batch",
                   output = "Means",
                   seed = 12321,
                   binary = FALSE,
                   show.labels = FALSE)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    has.subset <- !is.null(subset) & length(subset) != 1
    partial <- missing == "Use partial data"
    n.total <- nrow(data)
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
        if (!algorithm %in% c("Batch", "Bagging"))
            warning("As your data is weighted, only the 'Batch' and 'Bagged' algorithms are appropriate. You have been automaticaly switched to the 'Batch' algorithm.")
    }
    row.names <- rownames(data)
    ####################################################################
    ##### Data manipulation specific to kmeans                       #####
    ####################################################################
    # Making categorical variables numeric.
    data <- AsNumeric(data, binary)
    variable.labels <- Labels(data)
    # Treatment of missing values.
    if (missing == "Use partial data" && algorithm != "Batch" & any(!complete.cases(data)))
        stop(paste0("'Missing' has been set to 'Use partial data', but the algorithm has been set to '", algorithm, "'",
                    "you need need to change one of these settings."))
    input.formula <- as.formula(paste0("~", paste(names(data), collapse = "+")))
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    x <- processed.data$estimation.data
    n <- nrow(x)
    if (n < ncol(x))
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
    .weights <- processed.data$weights
    ####################################################################
    ##### Fitting the model.                                       #####
    ####################################################################
    n.clusters <- if (length(centers) > 1) nrow(centers) else centers
    model <- if (algorithm == "Bagging")
        {
            mod <- try(bclust(x, iter.base = n.starts, weights = .weights, centers = n.clusters, base.centers = n.clusters, final.kmeans = FALSE, verbose = FALSE),
                       silent = TRUE)
            if (any("try-error" %in% class(mod)))
                stop("Bagged clustering algorithm has failed. This may mean that there is too little variation in your data for bagged clustering to be appropriate.")
            mod
        }
        else
        {
            if (algorithm == "Batch")
                BatchKMeans(x, centers, .weights, iter.max, n.starts)
            else
                kmeans(x, centers, iter.max, n.starts, algorithm)
        }
    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # Computing summary statistics and results
    cluster.a <- model$cluster
    centers <- model$centers
        # Replacing names with labels
    cluster.label <- paste0("K-Means: ", n.clusters, " clusters")
    if (show.labels)
    {
        com <- ExtractCommonPrefix(Labels(data))
        variable.labels <- com$shortened.labels
        common <- if(is.na(com$common.prefix)) "" else paste0(" - ", com$common.prefix)
        cluster.label <- paste0(cluster.label, common)
    }
    else
        variable.labels <- Names(data)
    colnames(centers) <- variable.labels
    result <- list(#cluster = rep(NA, nrow(data)),
                   centers = t(centers),
                   cluster.label = cluster.label)#,
    class(result) <- "KMeans"

    # Saving data - generally applicable.
    if (missing == "Imputation (replace missing values with estimates)")
        data <- processed.data$data
    result$subset <- subset <- row.names %in% rownames(x)
    result$weights <- unfiltered.weights
    result$model <- data
    result$cluster <- cluster <- predict.KMeans(centers, data)
    if (has.subset & (partial | weighted))
    {
        if (partial)
            n <- sum(original.subset)
    }
    else
        result$cluster[subset] <- model$cluster # This will often do nothing.
    analysis.subset <- if(partial & has.subset) original.subset else subset
    weights.a <- weights[analysis.subset]
    data.a <- data[analysis.subset, , drop = FALSE]
    result$sizes <- sizes <- Frequency(cluster.a, weights = weights.a)
    sizes <- as.numeric(prop.table(sizes))
    rownames(centers) <- paste0("Cluster ", 1:n.clusters, "\n", FormatAsPercent(sizes, 0))
    result$centers <- centers
    if (output == "Means table")
        return(centers)
    #clusters.a <- result$cluster[analysis.subset]
    if (missing == "Assign partial data to clusters" | weighted)
        result$centers <- MeanByGroup(data.a, cluster.a, weights.a)
    if (weighted)
    {   # Essentially an extra iteration
        result$cluster[analysis.subset] <- clusters.a <- predict.KMeans(result$centers, data.a)
        result$centers <- MeanByGroup(data.a, clusters.a, weights.a)
    }
    result$cluster <- factor(paste("Cluster", cluster))
    # Goodness-of-fit
    tss <- TotalSumOfSquares(data.a, weights.a)
    rss <- ResidualSumOfSquares(data.a, cluster.a, weights.a)
    ess <- tss - rss
    result$variance.explained = ess/tss
    result$calinski.harabasz = (ess/(n.clusters - 1)/(rss/(n - n.clusters)))
    # Saving descriptive information.
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$n.clusters <- n.clusters
    result$estimation.data <- x
    # Saving parameters
    result$show.labels <- show.labels
    result$output <- output
    result$missing <- missing
    result$variable.labels <- variable.labels
    result
}

#' print.KMeans
#'
#' @param x The \link{KMeans} object.
#' @param p.cutoff The alpha value to use when highlighting results.
#' @param digits The number of digits when printing the \code{"detail"} output.
#' @param ... Generic print arguments.
#' @importFrom flipFormat Labels FormatAsPercent FormatAsReal
#' @importFrom flipAnalysisOfVariance MultipleMeans
#' @export
print.KMeans <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    subset <- x$subset
    variables <- x$model[subset, ]
    if (x$show.labels)
        for (i in 1:ncol(variables))
            attr(variables[, i], "label") <- x$variable.labels[i]
    cluster <- x$cluster[subset]
    levels(cluster) <- rownames(x$centers)
    table <- MultipleMeans(variables,
                cluster,
                weights = x$weights[subset],
                show.labels = x$show.labels,
                title = x$cluster.label,
                subtitle = paste0("Variance explained:", FormatAsPercent(x$variance.explained), "; Calinski-Harabasz: ", FormatAsReal(x$calinski.harabasz, 2)),
                footer = x$sample.description)
    print(table)
}




#' BatchKMeans
#'
#' Uses the batch method k-means method for forming clusters.
#' @param x A \code{\link{data.frame}}.
#' @param centers Either the number of clusters (e.g., 2), or a set of initial cluster
#' centers.
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param iter.max The number of iterations of the algorithm.
#' @param n.starts The number of times to run the whole algorihtm.
#' @param seed The seed for the random number generator.
#' @details The batch method works by selecting initial cluster centers, allocating each observation
#' to the closest cluster, recomputing the cluster centers, and repeating these steps until the either the
#' residual sum of squares stops reducing, or, \code{iter.max} is exceeded.
#'
#' The two novel features of this algorithm, relative to traditional k-means algorithms such as Hartigan and Wong (1979) are:
#' (1) The algorithms addresses weights. (2) The algorithm classifies cases that have incomplete data.
#'
#'  The first starting point of the algorithm is computed as follows: (1) Missing values are removed. (2) If the data is weighted,
#'  a new sample is created via resampling. (3) The Hartigan-Wong algorithm is applied to the syntheatic sample. (4) The cluster
#'  centers identified via Hartigan-Wong are used as the initial clusters of the batch method.
#'
#'  Where \code{n.starts} is greater than 1, or, there are less than 100 cases left after removing cases with
#'  incomplete data, the remaining start points are selected by: (1) identifying unique cases, and (2) sampling
#'  without replacement from amongst the unique cases.
#' @references Hartigan, J. A. and Wong, M. A. (1979). A K-means clustering algorithm. Applied Statistics 28, 100–108.
#' @importFrom stats aggregate complete.cases kmeans
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @export
BatchKMeans <- function(x, centers, weights, iter.max, n.starts, seed = 1223)
{
    set.seed(seed)
    best.centers <- NULL
    global.lowest.rss <- Inf
    unique.cases <- unique(x)
    n.clusters <- if(length(centers) == 1) centers else nrow(centers)
    for (start in 1:n.starts)
    {
        iteration <- 0
        lowest.rss <- Inf
        complete.c <- complete.cases(x)
        centers <- if (start == 1 & sum(complete.c) > 100)
        {
            complete.x <- x[complete.c, ]
            if (!is.null(weights))
            {
                complete.weights <- weights[complete.c]
                complete.weights <- complete.weights / sum(complete.weights) * length(complete.weights)
                complete.x <- flipTransformations::AdjustDataToReflectWeights(complete.x, complete.weights, seed = seed, silent = TRUE)
            }
            kmeans(complete.x, centers, iter.max = 10, nstart = 1, algorithm = "Hartigan-Wong")$centers
        }
        else
            unique.cases[sample.int(nrow(unique.cases), n.clusters, replace = FALSE), ]
        cluster <- predict.KMeans(centers, x)
        while (iter.max >= (iteration <- iteration + 1) &
               lowest.rss > (rss <- ResidualSumOfSquares(x, cluster, weights)))
        {
            lowest.rss <- rss
            centers <- MeanByGroup(x, cluster, weights)
            cluster <- predict.KMeans(centers, x)
        }
        if (global.lowest.rss > lowest.rss)
        {
            global.lowest.rss <- lowest.rss
            best.centers <- centers
        }
    }
    list(cluster = predict.KMeans(best.centers, x), centers = best.centers)
}
