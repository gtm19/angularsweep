#' Function to check \link{sweep_latlons} arguments are valid before proceeding
#'
#' @inheritParams sweep_
#'
#' @return NULL, if all conditions are met
pre_sweep_check <- function(df, xcol, ycol, weight, radius, inc_data) {

  stopifnot(
    "df must have at least 2 rows" = nrow(df) > 1,
    "xcol and/or ycol are not names of columns in in df" = c(xcol, ycol) %in% names(df),
    "xcol and ycol must be numeric" = sapply(df[c(xcol, ycol)], is.numeric),
    "xcol and ycol can have no NA values" = !sapply(df[c(xcol, ycol)], is.na),
    "weight must be NULL, or the name of a numeric column in df" = is.null(weight) || is.character(weight) && weight %in% names(df) && is.numeric(df[[weight]]),
    "weight column, if specified, can have no NA values" = is.null(weight) || !is.na(df[[weight]]),
    "radius must be greater than zero" = radius > 0,
    "inc_data must be a logical atomic vector" = is.logical(inc_data) && length(inc_data) == 1
  )
}

#' Calculate distance matrix for \eqn{n} given points
#'
#' @inheritParams stats::dist
#'
#' @importFrom stats dist
#'
#' @return an \eqn{n \times n} distance matrix
dist_matrix <- function(x) {

  as.matrix(
    dist(x, diag = TRUE, upper = TRUE)
  )
}
