test_that("summarise_intermediate_results checks out", {
  intermediate_results <-  structure(
    list(
      doc_id = c("A", "B", "C"),
      n_gold = c(3L, 2L, 4L),
      n_suggested = c(3L, 2L, 1L),
      tp = c(1L, 1L, 1L),
      fp = 2:0,
      fn = c(2L, 1L, 3L),
      prec = c(1 / 3, 0.5, 1),
      rprec = c(1 / 3, 0.5, 1),
      rec = c(1 / 3, 0.5, 0.25),
      f1 = c(1 / 3, 0.5, 0.4)
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  expected_res <- tibble::tribble(
    ~metric, ~value, ~support,
    "f1", (1 / 3 + 0.5 + 0.4) / 3, 3,
    "prec", (1 / 3 + 0.5 + 1) / 3, 3,
    "rec", (1 / 3 + 0.5 + 0.25) / 3, 3,
    "rprec", (1 / 3 +  0.5 + 1) / 3, 3,
  )

  intermediate_results_grpd <- dplyr::group_by(
    intermediate_results, .data$doc_id
  )
  actual_res <- summarise_intermediate_results_dplyr(intermediate_results_grpd)
  actual_res_collapse <- summarise_intermediate_results(
    list(
      results_table = intermediate_results,
      grouping_var = c("doc_id")
    )
  )

  expect_equal(actual_res, expected_res)
  expect_equal(actual_res_collapse, expected_res)

})

test_that("f1-score handles missings expectedly in summarise stage", {

  # Scenraio 1: Micro-Averaged precision and recall are both 0
  intermediate_results <- structure(
    list(
      doc_id = c("A", "A"),
      label_id = c("a", "b"),
      n_gold = c(1L, 0L),
      n_suggested = c(0L, 1L),
      tp = c(0L, 0L),
      fp = c(0L, 1L),
      fn = c(1L, 0L),
      delta_relevance = c(0, 0),
      prec = c(NA, 0),
      rprec = c(NA, NA),
      rec = c(0, NA),
      f1 = c(0, 0)
    ),
    class = c("tbl_df", "tbl", "data.frame")
  )


  expected <- structure(
    list(
      metric = c("f1", "prec", "rec", "rprec"),
      value = c(0,  0, 0, 0),
      support = c(1, 1, 1, 1)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L)
  )

  observed <- summarise_intermediate_results(list(
    results_table = intermediate_results, grouping_var = c("doc_id", "label_id")
  ))

  expect_equal(observed, expected)

  # Scenraio 2: Micro-Averaged precision is NA and recall is zero
  intermediate_results <- structure(
    list(
      doc_id = c("A", "A"),
      label_id = c("a", "b"),
      n_gold = c(1L, 1L),
      n_suggested = c(0L, 0L),
      tp = c(0L, 0L),
      fp = c(0L, 0L),
      fn = c(1L, 1L),
      delta_relevance = c(0, 0),
      prec = c(NA, NA),
      rprec = c(NA, NA),
      rec = c(0, 0),
      f1 = c(0, 0)
    ),
    class = c("tbl_df", "tbl", "data.frame")
  )


  expected <- structure(
    list(
      metric = c("f1", "prec", "rec", "rprec"),
      value = c(0,  NA, 0, NA),
      support = c(1, 0, 2, 0)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L)
  )

  observed <- summarise_intermediate_results(list(
    results_table = intermediate_results,
    grouping_var = c("doc_id", "label_id")
  ))

  expect_equal(observed, expected)

  # Scenraio 3: Micro-Averaged precision is zero and recall is NA
  intermediate_results <- structure(
    list(
      doc_id = c("A", "A"),
      label_id = c("a", "b"),
      n_gold = c(0L, 0L),
      n_suggested = c(1L, 1L),
      tp = c(0L, 0L),
      fp = c(1L, 1L),
      fn = c(0L, 0L),
      delta_relevance = c(0, 0),
      prec = c(0, 0),
      rprec = c(NA, NA),
      rec = c(NA, NA),
      f1 = c(0, 0)
    ),
    class = c("tbl_df", "tbl", "data.frame")
  )


  expected <- structure(
    list(
      metric = c("f1", "prec", "rec", "rprec"),
      value = c(0,  0, NA, NA),
      support = c(1, 2, 0, 0)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L)
  )

  observed <- summarise_intermediate_results(list(
    results_table = intermediate_results,
    grouping_var = c("doc_id", "label_id")
  ))

  expect_equal(observed, expected)

  # Scenraio 4: Micro-Averaged precision is NA and recall is NA
  intermediate_results <- structure(
    list(
      doc_id = c("A", "A"),
      label_id = c("a", "b"),
      n_gold = c(0L, 0L),
      n_suggested = c(0L, 0L),
      tp = c(0L, 0L),
      fp = c(0L, 0L),
      fn = c(0L, 0L),
      delta_relevance = c(0, 0),
      prec = c(NA, NA),
      rprec = c(NA, NA),
      rec = c(NA, NA),
      f1 = c(NA, NA)
    ),
    class = c("tbl_df", "tbl", "data.frame")
  )


  expected <- structure(
    list(
      metric = c("f1", "prec", "rec", "rprec"),
      value = c(NA_real_,  NA_real_, NA_real_, NA_real_),
      support = c(0, 0, 0, 0)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L)
  )

  observed <- summarise_intermediate_results(list(
    results_table = intermediate_results,
    grouping_var = c("doc_id", "label_id")
  ))

  expect_equal(observed, expected)
})

test_that("Summarise works with weighted mean", {

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
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "B", "f",
    "B", "c",
    "C", "f"
  )

  label_distribution <- tibble::tribble(
    ~label_id, ~label_freq, ~n_docs,
    "a", 10000, 10100,
    "b", 1000, 10100,
    "c", 100, 10100,
    "d", 1, 10100,
    "e", 1, 10100,
    "f", 2, 10100,
    "g", 0, 10100 # this label is not in the gold or pred set and should not
    # cause an error
  )

  comp <- create_comparison(gold, pred)

  label_wise_res_no_weight <- compute_intermediate_results(
    gold_vs_pred = comp,
    grouping_var = "label_id",
    propensity_scored = FALSE
  )

  weights <- compute_propensity_scores(label_distribution)
  label_wise_res_no_weight$results_table <- label_wise_res_no_weight$results_table |> # nolint
    dplyr::left_join(weights, by = "label_id")
  expected_res_weighted_mean <- dplyr::summarise(
    .data = label_wise_res_no_weight$results_table,
    dplyr::across(
      prec:f1,
      .fns =  ~ sum(.x * .data$label_weight, na.rm = TRUE) /
        sum(as.numeric(!is.na(.x)) * .data$label_weight, na.rm = TRUE)
    )
  )

  expected_res_weighted_mean <- expected_res_weighted_mean |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "metric",
      values_to = "value"
    )

  res_weighted_mean <- summarise_intermediate_results(
    label_wise_res_no_weight,
    propensity_scored = TRUE,
    label_distribution
  )

  expected_vs_observed <- dplyr::full_join(
    res_weighted_mean,
    expected_res_weighted_mean,
    by = "metric",
    suffix = c(".obs", ".exp")
  )

  expect_equal(
    expected_vs_observed$value.obs,
    expected_vs_observed$value.exp
  )

})
