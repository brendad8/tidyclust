#' Density-Based Spatial Clustering of Applications with Noise
#'
#' @description
#'
#' `db_clust` defines a model that fits clusters based on areas with observations
#' that are densely packed together
#'
#' There are different ways to fit this model, and the method of estimation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[=details_hier_clust_stats]{stats}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The engine for this model is `"dbscan"`.
#' @param radius Positive integer, Radius used to determine core-points and cluster points together (required).
#' @param minpts Positive double, Minimum number of points needed to form a cluster (required)
#'
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' To predict the cluster assignment for a new observation, we determine if a point
#' is within the radius of a core point. If so, we predict the same cluster as the core point.
#' If not, we predict the observation to be an outlier.
#'
#'
#' @return A `db_clust` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("db_clust")
#'
#' dbscan()
#' @export
db_clust <-
  function(mode = "partition",
           engine = "dbscan",
           radius = NULL,
           minpts = NULL) {
    args <- list(
      radius = enquo(radius),
      minpts = enquo(minpts)
    )

    new_cluster_spec(
      "db_clust",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.db_clust <- function(x, ...) {
  cat("DBSCAN Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update db_clust
#' @rdname tidyclust_update
#' @export
update.db_clust <- function(object,
                              parameters = NULL,
                              radius = NULL,
                              minpts = NULL,
                              fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    radius = enquo(radius),
    minpts = enquo(minpts),
  )

  args <- parsnip::update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
    object$eng_args <- eng_args
  } else {
    null_args <- map_lgl(args, null_value)
    if (any(null_args)) {
      args <- args[!null_args]
    }
    if (length(args) > 0) {
      object$args[names(args)] <- args
    }
    if (length(eng_args) > 0) {
      object$eng_args[names(eng_args)] <- eng_args
    }
  }

  new_cluster_spec(
    "db_clust",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.db_clust <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$minpts)) && any(args$minpts < 0)) {
    rlang::abort("The number of points in a cluster should be > 0.")
  }

  if (all(is.numeric(args$radius)) && any(args$radius < 0)) {
    rlang::abort("The radius used to create a cluster should be > 0.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.db_clust <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around dbscan function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `dbscan::dbscan` and retains the parameters `radius` or `minpts` as an
#' attribute.
#'
#' @param x matrix or data frame
#' @param radius Radius used to determine core-points and cluster points together
#' @param minpts Minimum number of points needed to form a cluster
#'
#' @return dbscan object
#' @keywords internal
#' @export
.db_clust_fit_dbscan <- function(x,
                                  radius = NULL,
                                  minpts = NULL) {
  res <- dbscan::dbscan(x, eps = raidus, minPts = minPts)
  attr(res, "radius") <- radius
  attr(res, "minpts") <- minpts
  attr(res, "training_data") <- x
  return(res)
}
