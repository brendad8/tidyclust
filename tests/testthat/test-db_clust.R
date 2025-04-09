test_that("primary arguments", {
  basic <- db_clust(mode = "partition")
  basic_dbscan <- translate_tidyclust(basic %>% set_engine("dbscan"))
  expect_equal(
    basic_dbscan$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      eps = rlang::expr(missing_arg()),
      minPts = rlang::expr(missing_arg())
    )
  )

  db <- db_clust(radius = 2, min_points = 4, mode = "partition")
  db_dbscan <- translate_tidyclust(db %>% set_engine("dbscan"))
  expect_equal(
    db_dbscan$method$fit$args,
    list(
      x = rlang::expr(missing_arg()),
      eps = rlang::expr(missing_arg()),
      minPts = rlang::expr(missing_arg()),
      eps = new_empty_quosure(2),
      minPts = new_empty_quosure(4)
    )
  )
})


test_that("bad input", {
  expect_snapshot(error = TRUE, db_clust(mode = "bogus"))
  expect_snapshot(error = TRUE, {
    bt <- db_clust(radius = -1) %>% set_engine("dbscan")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, {
    bt <- db_clust(min_points = -1) %>% set_engine("dbscan")
    fit(bt, mpg ~ ., mtcars)
  })
  expect_snapshot(error = TRUE, translate_tidyclust(db_clust(), engine = NULL))
  expect_snapshot(error = TRUE, translate_tidyclust(db_clust(formula = ~x)))
})

test_that("predictions", {
  set.seed(1234)
  db_clust_fit <- db_clust(radius = 60, min_points = 5) %>%
    set_engine("dbscan") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- dbscan::dbscan(mtcars, eps = 60, minPts = 5)$cluster

  ref_clusts <- ref_res[ref_res != 0]
  ref_outliers <- which(ref_res == 0)

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    ref_clusts,
    predict(db_clust_fit, mtcars)$.pred_cluster %>% .[. != "Outlier"] %>% as.numeric() - 1
  )

  expect_equal(
    relevel_preds(ref_clusts),
    extract_cluster_assignment(db_clust_fit)$.cluster %>% .[. != "Outlier"] %>% as.numeric()
  )

  expect_equal(
    ref_outliers,
    which(predict(db_clust_fit, mtcars)$.pred_cluster == "Outlier")
  )
})


test_that("extract_centroids work", {
  set.seed(1234)
  dbscan_fit <- db_clust(radius = 20, min_points = 3) %>%
    set_engine("dbscan") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- dbscan::dbscan(mtcars, eps = 20, minPts = 3)

  centers <- bind_cols(mtcars, .cluster = ref_res$cluster) %>%
    dplyr::group_by(.cluster) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::filter(.cluster != 0) %>%
    dplyr::select(-.cluster)

  ref_centroids <- dplyr::bind_cols(
    .cluster = c(1L, 2L, 4L, 5L, 3L),
    centers
  ) %>%
    dplyr::arrange(.cluster) %>%
    dplyr::select(-.cluster)

  expect_identical(
    extract_centroids(dbscan_fit) %>%
      dplyr::filter(.cluster != "Outlier") %>%
      dplyr::select(-.cluster),
    ref_centroids
  )
})

test_that("Right classes", {
  expect_equal(
    class(db_clust()),
    c("db_clust", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    db_clust()
  )
  expect_snapshot(
    db_clust(radius = 20, min_points = 3)
  )
})

test_that("updating", {
  expect_snapshot(
    db_clust(radius = 20, min_points = 5) %>%
      update(radius = tune())
  )
})


test_that("reordering is done correctly for stats k_means", {
  set.seed(42)

  kmeans_fit <- k_means(num_clusters = 6) %>%
    set_engine("stats") %>%
    fit(~., data = mtcars)

  summ <- extract_fit_summary(kmeans_fit)

  expect_identical(
    summ$n_members,
    unname(as.integer(table(summ$cluster_assignments)))
  )
})
#
# test_that("reordering is done correctly for ClusterR k_means", {
#   set.seed(42)
#
#   kmeans_fit <- k_means(num_clusters = 6) %>%
#     set_engine("ClusterR") %>%
#     fit(~., data = mtcars)
#
#   summ <- extract_fit_summary(kmeans_fit)
#
#   expect_identical(
#     summ$n_members,
#     unname(as.integer(table(summ$cluster_assignments)))
#   )
# })

test_that("errors if `radius` and `min_points` aren't specified", {
  expect_snapshot(
    error = TRUE,
    db_clust() %>%
      set_engine("dbscan") %>%
      fit(~ ., data = mtcars)
  )
})

test_that("errors if `radius` isn't specified", {
  expect_snapshot(
    error = TRUE,
    db_clust(min_points = 10) %>%
      set_engine("dbscan") %>%
      fit(~ ., data = mtcars)
  )
})

test_that("errors if `min_points` isn't specified", {
  expect_snapshot(
    error = TRUE,
    db_clust(radius = 20) %>%
      set_engine("dbscan") %>%
      fit(~ ., data = mtcars)
  )
})
