
#' ResidualSumOfSquares
#'
#' @param x A \link{data.frame} or \link{matrix}.
#' @param groups	A variable indicating group membership. Either a \link{factor} or coerced to a \link{factor}.
#' @param weights The sampling or replication weights.
#' @details Missing values are ignored.
#' @importFrom flipStatistics SumOfSquaresByGroup
#' @export
ResidualSumOfSquares <- function(x, groups, weights)
{
    ss <- SumOfSquaresByGroup(x, groups, weights)
    ss <- sum(ss)
    if (is.na(ss))
        ss <- Inf
    ss
}

#' TotalSumOfSquares
#' @param x A \link{data.frame} or \link{matrix}.
#' @param weights The sampling or replication weights.
#' @details Missing values are ignored.
#' @importFrom flipStatistics SumOfSquares
#' @export
TotalSumOfSquares <- function(x, weights)
{
    ss <- SumOfSquares(x, weights)
    sum(ss)
}
