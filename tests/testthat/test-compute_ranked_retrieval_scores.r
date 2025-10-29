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
    "A", "f", 0.3277,
    "A", "e", 0.32172,
    "A", "b", 0.13517,
    "A", "g", 0.10134,
    "A", "h", 0.09152,
    "A", "a", 0.07483,
    "A", "i", 0.03649,
    "A", "j", 0.03551,
    "A", "k", 0.03397,
    "A", "c", 0.03364
  )

  expect_silent(
    observed <- compute_ranked_retrieval_scores(pred, gold)
  )

  expected <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "dcg", "doc-avg", 1.776202, 1,
    "ndcg", "doc-avg", 0.602417, 1,
    "lrap", "doc-avg", 0.413333, 1
  )

  expect_equal(observed, expected, tolerance = 10e-6)
})

test_that("grouped ranked retrieval works", {
  # some dummy results
  gold <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "b",
    "A", "c",
    "A", "d",
    "A", "e",
    "B", "b",
    "B", "i",
    "B", "l",
    "C", "a",
    "C", "c",
    "C", "f",
    "C", "j",
    "C", "k",
    "D", "c",
    "D", "e",
    "D", "h",
    "D", "k",
    "E", "d",
  )

  doc_groups <- tibble::tribble(
    ~doc_id, ~hsg,
    "A", "001",
    "B", "002",
    "C", "003",
    "D", "004",
    "E", "005",
    "F", "006"
  ) |>
    dplyr::mutate(hsg = as.factor(hsg))

  set.seed(2)

  pred <- expand.grid(
    doc_id = LETTERS[1:5],
    label_id = letters[1:11],
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(score = runif(dplyr::n()))

  observed <- compute_ranked_retrieval_scores(
    pred,
    gold,
    doc_groups = doc_groups,
    drop_empty_groups = FALSE
  )

  # test that empty factor group is reported back
  expect_equal(
    nrow(dplyr::filter(observed, hsg == "006")), 3L
  )

  doc_wise_results <- create_comparison(pred, gold, doc_groups = doc_groups) |>
    dplyr::group_by(doc_id, hsg) |>
    dplyr::do(
      ndcg = ndcg_score(.), # according to reference implementation
      dcg = dcg_score(.), # according to reference implementation
      lrap = lrap_score(.)
    ) |> # according to reference implementation
    tidyr::unnest(c(ndcg, dcg, lrap))

  expected <- doc_wise_results |>
    dplyr::group_by(hsg, .drop = FALSE) |>
    dplyr::summarise(
      mode = "doc-avg",
      ndcg = mean(ndcg),
      dcg = mean(dcg),
      lrap = mean(lrap),
      support = dplyr::n()
    ) |>
    tidyr::pivot_longer(
      cols = c("dcg", "ndcg", "lrap"),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::select(hsg, metric, mode, value, support)

  expect_equal(observed, expected, tolerance = 10e-6)
})
