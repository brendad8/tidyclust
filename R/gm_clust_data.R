# nocov start

make_gm_clust <- function() {
  modelenv::set_new_model("gm_clust")

  modelenv::set_model_mode("gm_clust", "partition")

  # ----------------------------------------------------------------------------

  modelenv::set_model_engine("gm_clust", "partition", "mclust")
  modelenv::set_dependency(
    model = "gm_clust",
    mode = "partition",
    eng = "mclust",
    pkg = "mclust"
  )
  modelenv::set_dependency(
    model = "gm_clust",
    mode = "partition",
    eng = "mclust",
    pkg = "tidyclust"
  )

  modelenv::set_fit(
    model = "gm_clust",
    eng = "mclust",
    mode = "partition",
    value = list(
      interface = "matrix",
      protect = c("data"),
      func = c(pkg = "tidyclust", fun = ".gm_clust_fit_mclust"),
      defaults = list()
    )
  )

  modelenv::set_encoding(
    model = "gm_clust",
    eng = "mclust",
    mode = "partition",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  modelenv::set_model_arg(
    model = "gm_clust",
    eng = "mclust",
    exposed = "num_clusters",
    original = "G",
    func = list(pkg = "dials", fun = "num_clusters"),
    has_submodel = TRUE
  )


  modelenv::set_pred(
    model = "gm_clust",
    eng = "mclust",
    mode = "partition",
    type = "cluster",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = ".gm_clust_predict_mclust"),
      args =
        list(
          object = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )

}

# nocov end
