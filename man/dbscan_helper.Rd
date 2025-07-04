% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_clust.R
\name{dbscan_helper}
\alias{dbscan_helper}
\title{dbscan fit helper function}
\usage{
dbscan_helper(object, ...)
}
\arguments{
\item{object}{db_clust object}
}
\value{
numeric vector
}
\description{
This function returns the cluster assignments for the training data
based on their distance to the CLOSEST core point in the data.
}
\keyword{internal}
