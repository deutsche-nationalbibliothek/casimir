test_that("threshold application works", {

  gold <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "b",
    "A", "c",
    "B", "a",
    "B", "d",
    "C", "a",
    "C", "b",
    "C", "d",
    "C", "f"
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id, ~score, ~rank,
    "A", "a", 0.9, 1L,
    "A", "d", 0.7, 2L,
    "A", "f", 0.3, 3L,
    "A", "c", 0.1, 4L,
    "B", "a", 0.8, 1L,
    "B", "e", 0.6, 2L,
    "B", "d", 0.1, 3L,
    "C", "f", 0.1, 1L,
    "C", "c", 0.2, 2L,
    "C", "e", 0.2, 2L
  )

  base_compare <- casimir:::create_comparison(gold, pred)
  # apply zero as threshold should not change anything
  res_0 <- casimir:::apply_threshold(threshold = 0, base_compare = base_compare)
  expect_equal(res_0, base_compare)

  res_1 <- casimir:::apply_threshold(base_compare = base_compare, threshold = 1)
  # expect that no labels are suggested for threshold 1
  expect_true(sum(res_1$suggested) == 0)
  # all gold labels should still be present in dataset
  expect_equal(
    dplyr::select(res_1, "doc_id", "label_id"),
    gold)

  # expect error when threshold is above 1
  expect_error(
    casimir:::apply_threshold(base_compare = base_compare, threshold = 1.3),
    regexp = "threshold >= 0 & threshold <= 1 is not TRUE"
  )

  # expect some detailed specific results for the test case
  res_0.5 <- casimir:::apply_threshold(
    base_compare = base_compare,
    threshold = 0.5
  ) |>
    dplyr::select(-rank)
  exp_0.5 <- tibble::tribble(
    ~doc_id, ~label_id, ~gold, ~score , ~suggested, ~relevance,
    "A",   "a", TRUE, 0.9, TRUE, 0,
    "A",   "b", TRUE, NA,  FALSE, 0,
    "A",   "c", TRUE, 0.1, FALSE, 0,
    "B",   "a", TRUE, 0.8, TRUE, 0,
    "B",   "d", TRUE, 0.1, FALSE, 0,
    "C",   "a", TRUE, NA,  FALSE, 0,
    "C",   "b", TRUE, NA,  FALSE, 0,
    "C",   "d", TRUE, NA,  FALSE, 0,
    "C",   "f", TRUE, 0.1, FALSE, 0,
    "A",   "d", FALSE, 0.7,TRUE, 0,
    "B",   "e", FALSE, 0.6,TRUE, 0
  )

  expect_equal(res_0.5, exp_0.5)

  pred2 <-  tibble::tribble(
    ~doc_id, ~label_id, ~score,
    "A", "a", 0.9,
    "A", "d", 0.7,
    "A", "f", 1.3,
    "A", "c", 0.1,
    "B", "a", 0.8,
    "B", "e", 0.6,
    "B", "d", 0.1,
    "C", "f", 0.1,
    "C", "c", 0.2,
    "C", "e", 0.2
  )
  compare2 <- casimir:::create_comparison(gold, pred2)

  expect_error(
    apply_threshold(base_compare = compare2, limit = 0.5),
    regexp = 'sum(base_compare[["score"]] > 1, na.rm = TRUE) == 0 is not TRUE',
    fixed=TRUE
  )
})

test_that("limits are applied correctly",{

  base_compare <- casimir:::create_comparison(
    dnb_gold_standard, dnb_test_predictions)

  res <- apply_threshold(
    base_compare = base_compare,
    threshold = 0,
    limit = 10L)

  expect_equal(
    nrow(dplyr::filter(res, .data$suggested, .data$rank > 10L)),
    0
  )

  # inputing a limit requries a rank column
  expect_error(
    apply_threshold(
      threshold = 0.1,
      base_compare = dplyr::select(base_compare, -rank),
      limit = 5),
    regexp = '.*\\"rank\\" %in% colnames\\(base_compare\\) is not TRUE'
  )

})

test_that("Handling of NULL and inf-limits works", {

  base_compare <- casimir:::create_comparison(
    dnb_gold_standard, dnb_test_predictions)

  # null is not an appropriate input to limit
  expect_error(
    apply_threshold(
      threshold = 0.1,
      base_compare = dplyr::select(base_compare, -rank),
      limit = NULL),
    regexp = "is.numeric\\(limit\\) is not TRUE"
  )

  # limit = Inf requires a rank column as any other limit
  expect_error(
    apply_threshold(
      threshold = 0.1,
      base_compare = dplyr::select(base_compare, -rank),
      limit = Inf),
    regexp = '\\"rank\\" %in% colnames\\(base_compare\\) is not TRUE'
  )

  # limit = Inf is an appropriate input
  expect_silent(
    apply_threshold(
      threshold = 0.1,
      base_compare = base_compare,
      limit = Inf)
  )
})
