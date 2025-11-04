test_that("create_comparison produces no nonsense", {
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
  expect_silent(casimir:::create_comparison(pred_scenario1, gold))

  pred_scenario2 <- tibble::tribble(
    ~doc_id, ~label_id, ~score,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "D", "f"
  )

  expect_error(casimir:::create_comparison(pred_scenario2, gold),
    regexp = "nrow\\(predicted_wo_gold\\) == 0 is not TRUE"
  )

  pred_scenario3 <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
  )

  expect_warning(
    create_comparison(pred_scenario3, gold),
    regexp = "Gold standard data contains documents that are not in predicted set." # nolint
  )

  # test that option ignore_inconsistencies works
  withr::with_options(
    list(casimir.ignore_inconsistencies = TRUE),
    {
      expect_silent(create_comparison(pred_scenario3, gold))
    }
  )
})
