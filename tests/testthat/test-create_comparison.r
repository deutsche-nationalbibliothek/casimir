test_that("create_comparison produces no nonesens", {

  # some dummy results
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

  pred_scenario1 <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "C", "f"
  )

  # scenario runs throughs undisturbed
  expect_silent(casimir:::create_comparison(gold, pred_scenario1))


  pred_scenario2 <- tibble::tribble(
    ~doc_id, ~label_id, ~score,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "D", "f"
  )

  expect_error(casimir:::create_comparison(gold, pred_scenario2),
               regexp = "nrow\\(predicted_wo_gold\\) == 0 is not TRUE")

  pred_scenario3 <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
  )

  expect_warning(casimir:::create_comparison(gold, pred_scenario3),
                 regexp = "gold standard data contains documents that are not in predicted set")
})
