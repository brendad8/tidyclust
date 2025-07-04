% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gm_clust.R
\name{gm_clust}
\alias{gm_clust}
\title{Gaussian Mixture Models (GMM)}
\usage{
gm_clust(
  mode = "partition",
  engine = "mclust",
  num_clusters = NULL,
  circular = TRUE,
  shared_size = TRUE,
  zero_covariance = TRUE,
  shared_orientation = TRUE,
  shared_shape = TRUE
)
}
\arguments{
\item{mode}{A single character string for the type of model. The only
possible value for this model is "partition".}

\item{engine}{A single character string specifying what computational engine
to use for fitting. The engine for this model is \code{"mclust"}.}

\item{num_clusters}{Positive integer, number of clusters in model (required).}

\item{circular}{Boolean, whether or not to fit circular MVG distributions for each cluster. Default \code{TRUE}.}

\item{shared_size}{Boolean, whether each cluster MVG should have the same size/volume. Default \code{TRUE}.}

\item{zero_covariance}{Boolean, whether or not to assign covariances of 0 for each MVG. Default \code{TRUE}.}

\item{shared_orientation}{Boolean, whether each cluster MVG should have the same orientation. Default \code{TRUE}.}

\item{shared_shape}{Boolean, whether each cluster MVG should have the same shape. Default \code{TRUE}.}
}
\value{
A \code{gm_clust} cluster specification.
}
\description{
\code{gm_clust} defines a model that fits clusters based on fitting a specified number of
multivariate Gaussian distributions (MVG) to the data.

There are multiple implementations for this model, and the implementation is
chosen by setting the model engine. The engine-specific pages for this model
are listed below.
\itemize{
\item \link[=details_gm_clust_mclust]{mclust}
}
}
\details{
\subsection{What does it mean to predict?}{

To predict the cluster assignment for a new observation, we determine which cluster
a point has the highest probability of belonging to.
}
}
\examples{
# Show all engines
modelenv::get_from_env("gm_clust")

gm_clust()
}
