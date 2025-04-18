% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gm_clust.R
\name{gm_clust}
\alias{gm_clust}
\title{Gaussian Mixture Models (GMM)}
\usage{
gm_clust(mode = "partition", engine = "mclust", num_clusters = NULL)
}
\arguments{
\item{mode}{A single character string for the type of model. The only
possible value for this model is "partition".}

\item{engine}{A single character string specifying what computational engine
to use for fitting. The engine for this model is \code{"mclust"}.}

\item{num_clusters}{Positive integer, number of clusters in model (required).}
}
\value{
A \code{gm_clust} cluster specification.
}
\description{
\code{gm_clust} defines a model that fits clusters based on fitting a specified number of
multivariate normal distributions to the data.

There are different ways to fit this model, and the method of estimation is
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
