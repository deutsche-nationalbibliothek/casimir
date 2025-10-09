test_that("empty factor levels can be handled", {
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
    "C", "f",
    "D", "a",
    "D", "c",
    "D", "e",
    "D", "f",
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "B", "g",
    "C", "f",
    "D", "a",
    "D", "c"
  )

  label_groups <- tibble::tribble(
    ~label_id, ~gnd_entity,
    "a", "pers",
    "b", "pers",
    "c", "subjh",
    "d", "subjh",
    "e", "conf",
    "f", "conf",
    "g", "subjh",
    "h", "empty_group"
  ) |>
    dplyr::mutate(gnd_entity = as.factor(gnd_entity))
  withr::local_options(list(casimir.drop_empty_groups = FALSE))
  comp <- create_comparison(gold, pred, label_groups = label_groups)
  intermed <- compute_intermediate_results(comp, c("label_id", "gnd_entity"))

  n <- nrow(dplyr::filter(intermed$results_table, gnd_entity == "empty_group"))
  expect_equal(
    nrow(dplyr::filter(intermed$results_table, gnd_entity == "empty_group")),
    1L
  )
  expect_equal(
    nrow(dplyr::filter(
      summarise_intermediate_results(intermed), gnd_entity == "empty_group"
    )),
    4L
  )
  withr::local_options(list(casimir.drop_empty_groups = TRUE))
  comp <- create_comparison(gold, pred, label_groups = label_groups)
  intermed <- compute_intermediate_results(comp, c("label_id", "gnd_entity"))

  n <- nrow(dplyr::filter(intermed$results_table, gnd_entity == "empty_group"))
  expect_equal(
    nrow(dplyr::filter(intermed$results_table, gnd_entity == "empty_group")),
    0L
  )
  expect_equal(
    nrow(dplyr::filter(
      summarise_intermediate_results(intermed), gnd_entity == "empty_group"
    )),
    0L
  )
})

test_that("empty factor levels can be dropped in doc_strata", {
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
    "C", "f",
    "D", "a",
    "D", "c",
    "D", "e",
    "D", "f"
  )

  doc_groups <- structure(
    list(
      doc_id = c("A", "B", "C", "D", "E"),
      hsg = factor(c("001", "001", "002", "002", "003"))
    ),
    row.names = c(NA, -4L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id, ~score,
    "A", "a", 0.1,
    "A", "d", 0.15,
    "A", "f", 0.01,
    "B", "a", 0.9,
    "B", "e", 0.99,
    "C", "f", 0.51,
    "D", "a", 0.1,
    "D", "c", 0.2
  )

  withr::with_options(list(casimir.drop_empty_groups = TRUE), {
    res <- compute_set_retrieval_scores(gold, pred, doc_groups = doc_groups)
    empty_group <- dplyr::filter(res, hsg == "003")

    expect_equal(
      nrow(empty_group),
      0L
    )

    auc <- compute_pr_auc(gold, pred, doc_groups = doc_groups)
    expect_equal(
      nrow(dplyr::filter(auc, hsg == "003")),
      0L
    )
  })


  withr::with_options(list(casimir.drop_empty_groups = FALSE), {
    res <- compute_set_retrieval_scores(gold, pred, doc_groups = doc_groups)
    empty_group <- dplyr::filter(res, hsg == "003")

    expect_equal(
      nrow(empty_group),
      4L
    )

    auc <- compute_pr_auc(gold, pred, doc_groups = doc_groups)
    expect_equal(
      nrow(dplyr::filter(auc, hsg == "003")),
      1L
    )
  })
})
