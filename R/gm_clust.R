#' Gaussian Mixture Models (GMM)
#'
#' @description
#'
#' `gm_clust` defines a model that fits clusters based on fitting a specified number of
#' multivariate normal distributions to the data.
#'
#' There are different ways to fit this model, and the method of estimation is
#' chosen by setting the model engine. The engine-specific pages for this model
#' are listed below.
#'
#' - \link[=details_gm_clust_mclust]{mclust}
#'
#' @param mode A single character string for the type of model. The only
#'   possible value for this model is "partition".
#' @param engine A single character string specifying what computational engine
#'   to use for fitting. The engine for this model is `"mclust"`.
#' @param num_clusters Positive integer, number of clusters in model (required).
#'
#'
#' @details
#'
#' ## What does it mean to predict?
#'
#' To predict the cluster assignment for a new observation, we determine which cluster
#' a point has the highest probability of belonging to.
#'
#'
#' @return A `gm_clust` cluster specification.
#'
#' @examples
#' # Show all engines
#' modelenv::get_from_env("gm_clust")
#'
#' gm_clust()
#' @export
gm_clust <-
  function(mode = "partition",
           engine = "mclust",
           num_clusters = NULL) {
    args <- list(
      num_clusters = enquo(num_clusters)
    )

    new_cluster_spec(
      "gm_clust",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.gm_clust <- function(x, ...) {
  cat("GMM Clustering Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update gm_clust
#' @rdname tidyclust_update
#' @export
update.gm_clust <- function(object,
                            parameters = NULL,
                            num_clusters = NULL,
                            fresh = FALSE, ...) {
  eng_args <- parsnip::update_engine_parameters(
    object$eng_args,
    fresh = fresh, ...
  )

  if (!is.null(parameters)) {
    parameters <- parsnip::check_final_param(parameters)
  }
  args <- list(
    num_clusters = enquo(num_clusters)
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
    "gm_clust",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}

# # ----------------------------------------------------------------------------

#' @export
check_args.gm_clust <- function(object) {
  args <- lapply(object$args, rlang::eval_tidy)

  if (all(is.numeric(args$num_clusters)) && any(args$num_clusters <= 0)) {
    rlang::abort("The number of clusters should be > 0.")
  }

  invisible(object)
}

#' @export
translate_tidyclust.gm_clust <- function(x, engine = x$engine, ...) {
  x <- translate_tidyclust.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

#' Simple Wrapper around Mclust function
#'
#' This wrapper prepares the data into a distance matrix to send to
#' `mclust::Mclust` and retains the parameters `num_clusters` as an
#' attribute.
#'
#' @param x matrix or data frame
#' @param num_clusters Number of clusters
#'
#' @return mclust object
#' @keywords internal
#' @export
.gm_clust_fit_mclust <- function(x,
                                 G = NULL,
                                 ...) {
  if (is.null(G)) {
    rlang::abort(
      "Please specify `num_clusters` to be able to fit specification.",
      call = call("fit")
    )
  }

  res <- mclust::Mclust(x, G = G)
  attr(res, "num_clusters") <- G
  attr(res, "training_data") <- x
  res
}
