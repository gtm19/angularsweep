% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sweep.R
\name{sweep_points}
\alias{sweep_points}
\title{Perform the angular sweep algorithm on a data frame of points}
\usage{
sweep_points(df, xcol, ycol, weight = NULL, radius, inc_data = FALSE)
}
\arguments{
\item{df}{the data frame containing coordinates}

\item{xcol}{the name of the column containing the x coordinate}

\item{ycol}{the name of the column containing the y coordinate}

\item{weight}{name of column containing data with which to weight the points}

\item{radius}{the radius of the circle to sweep with (in metres)}

\item{inc_data}{logical. Whether or not to include the original data alongside
the indices of the points included in each circle}
}
\value{
a data frame
}
\description{
Perform the angular sweep algorithm on a data frame of points
}
