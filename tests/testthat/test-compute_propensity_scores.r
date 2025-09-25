test_that("propensity score computation works", {

  label_distribution <- dnb_label_distribution

  expect_silent(
    compute_propensity_scores(label_distribution)
  )

  test_ids <- c("04256235X", "040118827", "118515500")

  result <- compute_propensity_scores(label_distribution) |>
    dplyr::filter(label_id %in% test_ids) |>
    dplyr::arrange(label_weight)


  expected <- tibble::tribble(
    ~label_id, ~label_weight,
    "040118827", 1.03659472212516, #head label with low weight
    "118515500", 6.96995526877882,
    "04256235X", 14.1449272483688 # tail label with high weight
  )

  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("propensity scores work out on toy example", {

  label_distribution <- tibble::tribble(
    ~label_id, ~label_freq, ~n_docs,
    "a", 10000, 10100,
    "b", 1000, 10100,
    "c", 100, 10100,
    "d", 1, 10100,
    "e", 1, 10100,
    "f", 2, 10100,
    "g", 0, 10100
  )

  a <- 0.55
  b <- 1.5
  n <- 10100
  c <- (log(n) - 1) * ((b + 1)**a)
  propensity_score <- 1 /
    (1 + c * exp(-a * log(label_distribution$label_freq + b)))

  res <- compute_propensity_scores(label_distribution)

  expect_equal(res$label_weight, 1 / propensity_score)
})
