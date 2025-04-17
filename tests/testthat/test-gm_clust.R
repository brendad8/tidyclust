test_that("primary arguments", {
  basic <- gm_clust(mode = "partition")
  basic_mclust <- translate_tidyclust(basic %>% set_engine("mclust"))
  expect_equal(
    basic_mclust$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),

      G= rlang::expr(missing_arg()),
      circular = rlang::expr(missing_arg()),
      zero_covariance = rlang::expr(missing_arg()),
      shared_orientation = rlang::expr(missing_arg()),
      shared_shape = rlang::expr(missing_arg()),
      shared_size = rlang::expr(missing_arg()),

      circular = new_empty_quosure(TRUE),
      zero_covariance = new_empty_quosure(TRUE),
      shared_orientation = new_empty_quosure(TRUE),
      shared_shape = new_empty_quosure(TRUE),
      shared_size = new_empty_quosure(TRUE)
    )
  )
})

test_that("engine arguments", {
  mclust_print <- gm_clust(mode = "partition")
  expect_equal(
    translate_tidyclust(
      mclust_print %>%
        set_engine("mclust")
    )$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),

      G = rlang::expr(missing_arg()),
      circular = rlang::expr(missing_arg()),
      zero_covariance = rlang::expr(missing_arg()),
      shared_orientation = rlang::expr(missing_arg()),
      shared_shape = rlang::expr(missing_arg()),
      shared_size = rlang::expr(missing_arg()),

      circular = new_empty_quosure(TRUE),
      zero_covariance = new_empty_quosure(TRUE),
      shared_orientation = new_empty_quosure(TRUE),
      shared_shape = new_empty_quosure(TRUE),
      shared_size = new_empty_quosure(TRUE)
    )
  )
})

test_that("bad input", {
  expect_snapshot(
    error = TRUE,
    gm_clust(mode = "bogus")
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- gm_clust(circular = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- gm_clust(zero_covariance = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- gm_clust(shared_orientation = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- gm_clust(shared_shape = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- gm_clust(shared_size = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- gm_clust(num_clusters = "bogus") %>% set_engine("mclust")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(gm_clust(), engine = NULL)
  )
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(gm_clust(formula = ~x))
  )
})

test_that("predictions", {
  set.seed(1234)

  mclust_fit <- gm_clust(num_clusters = 4) %>%
    set_engine("mclust") %>%
    fit(~., mtcars)

  set.seed(1234)
  mclustBIC <- mclust::mclustBIC
  ref_res <- mclust::Mclust(mtcars, G = 4, modelNames = "EII")$classification

  ref_predictions <- ref_res %>% unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    relevel_preds(predict(mclust_fit, mtcars)$.pred_cluster),
    predict(mclust_fit, mtcars)$.pred_cluster %>% as.numeric()
  )

  expect_equal(
    relevel_preds(ref_predictions),
    extract_cluster_assignment(mclust_fit)$.cluster %>% as.numeric()
  )
})


test_that("extract_centroids work", {
  set.seed(1234)
  mclust_fit <- gm_clust(num_clusters = 4) %>%
    set_engine("mclust") %>%
    fit(~., mtcars)

  set.seed(1234)
  mclustBIC <- mclust::mclustBIC
  ref_res <- mclust::Mclust(mtcars, G = 4, modelNames = "EII")$classification

  ref_predictions <- ref_res %>% unname()

  expect_identical(
    extract_centroids(mclust_fit) %>%
      dplyr::mutate(.cluster = as.double(.cluster)),
    mtcars %>%
      dplyr::group_by(.cluster = ref_predictions) %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), mean))
  )
})


test_that("predictions with new data", {
  set.seed(1234)
  mclust_fit <- gm_clust(num_clusters = 4) %>%
    set_engine("mclust") %>%
    fit(~., mtcars)

  set.seed(1234)
  mclustBIC <- mclust::mclustBIC
  ref_res <- mclust::Mclust(mtcars, G = 4, modelNames = "EII")$classification

  ref_predictions <- ref_res %>% unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    relevel_preds(predict(mclust_fit, mtcars[1:10, ])$.pred_cluster),
    predict(mclust_fit, mtcars[1:10, ])$.pred_cluster %>% as.numeric()
  )
})

test_that("Right classes", {
  expect_equal(
    class(gm_clust()),
    c("gm_clust", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    gm_clust()
  )
  expect_snapshot(
    gm_clust(num_clusters = 3)
  )
})

test_that("updating", {
  expect_snapshot(
    gm_clust(num_clusters = 5) %>%
      update(num_clusters = tune())
  )
})

test_that("reordering is done correctly for gm_clust", {
  set.seed(42)

  gm_fit <- gm_clust(num_clusters = 6) %>%
    set_engine("mclust") %>%
    fit(~., data = mtcars)

  summ <- extract_fit_summary(gm_fit)

  expect_identical(
    summ$n_members,
    unname(as.integer(table(summ$cluster_assignments)))
  )
})

test_that("model errors when parameters cannot be estimated", {
  set.seed(42)

  expect_error(
    gm_clust(num_clusters = 10, circular=F,zero_covariance=F,shared_orientation=F,shared_shape=F,shared_size=F) %>%
      set_engine("mclust") %>%
      fit(~., data = mtcars)
  )

})


test_that("mappings to different model names are correct", {
  expect_identical(
    tidyclust:::mclust_helper(T, NA, NA, NA, T),
    "EII"
  )
  expect_identical(
    tidyclust:::mclust_helper(T, NA, NA, NA, F),
    "VII"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, T, NA, T, T),
    "EEI"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, T, NA, F, T),
    "EVI"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, T, NA, T, F),
    "VEI"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, T, NA, F, F),
    "VVI"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, T, T, T),
    "EEE"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, T, F, T),
    "EVE"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, T, T, F),
    "VEE"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, T, F, F),
    "VVE"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, F, T, T),
    "EEV"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, F, F, T),
    "EVV"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, F, T, F),
    "VEV"
  )
  expect_identical(
    tidyclust:::mclust_helper(F, F, F, F, F),
    "VVV"
  )
})
