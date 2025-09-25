test_that("compute_ranked_retrieval_scores works", {

  # some dummy results
  gold <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "b",
    "A", "c",
    "A", "d",
    "A", "e",
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id, ~score,
    "A", "f",	0.3277,
    "A", "e",	0.32172,
    "A", "b",	0.13517,
    "A", "g",	0.10134,
    "A", "h",	0.09152,
    "A", "a",	0.07483,
    "A", "i",	0.03649,
    "A", "j",	0.03551,
    "A", "k",	0.03397,
    "A", "c",	0.03364
  )

  expect_silent(
    observed <- compute_ranked_retrieval_scores(
      gold,
      pred,
      compute_bootstrap_ci = FALSE
    )
  )

  expected <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "dcg",    "doc-avg", 1.776202, 1,
    "ndcg",   "doc-avg", 0.602417, 1,
    "lrap",   "doc-avg", 0.413333, 1
  )

  expect_equal(observed, expected, tolerance = 10e-6)

})

test_that("grouped ranked retrieval works", {

  # some dummy results
  gold <- tibble::tribble(
    ~hsg, ~doc_id, ~label_id,
    "001", "A", "a",
    "001", "A", "b",
    "001", "A", "c",
    "001", "A", "d",
    "001", "A", "e",
    "001", "B", "b",
    "001", "B", "i",
    "001", "B", "l",
    "001", "C", "a",
    "001", "C", "c",
    "001", "C", "f",
    "001", "C", "j",
    "001", "C", "k",
    "002", "D", "c",
    "002", "D", "e",
    "002", "D", "h",
    "002", "D", "k",
    "002", "E", "d",
  )

  set.seed(2)

  pred <- expand.grid(
    doc_id = LETTERS[1:5],
    label_id = letters[1:11]
  ) |>
    dplyr::mutate(score = runif(dplyr::n()))

  observed <- compute_ranked_retrieval_scores(
    gold,
    pred,
    doc_strata = "hsg",
    compute_bootstrap_ci = FALSE
  )

  doc_wise_results <- create_comparison(gold, pred, doc_strata = "hsg") |>
    dplyr::group_by(doc_id, hsg) |>
    dplyr::do(ndcg = ndcg_score(.), # according to Reference implementation
              dcg = dcg_score(.), # according to Reference implementation
              lrap = lrap_score(.)) |> # according to Reference implementation
    tidyr::unnest(c(ndcg, dcg, lrap))

  expected <- doc_wise_results |>
    dplyr::group_by(hsg) |>
    dplyr::summarise(
      mode = "doc-avg",
      ndcg = mean(ndcg),
      dcg = mean(dcg),
      lrap = mean(lrap),
      support = dplyr::n()) |>
    tidyr::pivot_longer(
      cols = c("dcg", "ndcg", "lrap"),
      names_to = "metric",
      values_to = "value") |>
    dplyr::select(hsg, metric, mode, value, support)


  expect_equal(observed, expected, tolerance = 10e-6)

})
