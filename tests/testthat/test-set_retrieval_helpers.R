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

test_that("compute_intermediate_results checks out", {
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

  compare_gold_vs_pred <- casimir:::create_comparison(gold, pred_scenario1)
  res_per_doc_dplyr <- casimir:::compute_intermediate_results_dplyr_version(
    compare_gold_vs_pred,
    grouping_var = rlang::syms(c("doc_id"))
  )
  res_per_doc_collapse <- compute_intermediate_results(
    compare_gold_vs_pred,
    grouping_var = c("doc_id")
  )


  expected_per_doc <- structure(
    list(
      doc_id = c("A", "B", "C"),
      n_gold = c(3L, 2L, 4L),
      n_suggested = c(3L, 2L, 1L),
      tp = c(1L, 1L, 1L),
      fp = 2:0,
      fn = c(2L, 1L, 3L),
      delta_relevance = c(0, 0, 0),
      rprec_deno = c(3L, 2L, 1L),
      prec = c(1/3, 0.5, 1),
      rprec = c(1/3, 0.5, 1),
      rec = c(1/3, 0.5, 0.25),
      f1 = c(1/3, 0.5, 0.4)),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame"))
  expected_per_doc_grouped <- dplyr::group_by(expected_per_doc, .data$doc_id)


  expect_equal(res_per_doc_dplyr, expected_per_doc_grouped)
  # also for collapse variant
  expect_equal(res_per_doc_collapse$results_table, expected_per_doc)

  res_per_subj_dplyr <- casimir:::compute_intermediate_results_dplyr_version(
    compare_gold_vs_pred,
    grouping_var = rlang::syms(c("label_id"))
  )
  res_per_subj_collapse <- compute_intermediate_results(
    compare_gold_vs_pred,
    grouping_var = c("label_id")
  )

  expected_per_subj <- structure(
    list(label_id = c("a", "b", "c", "d", "e", "f"),
         n_gold = c(3L, 2L, 1L, 2L, 0L, 1L),
         n_suggested = c(2L, 0L, 0L, 1L, 1L, 2L),
         tp = c(2L, 0L, 0L, 0L, 0L, 1L),
         fp = c(0L, 0L, 0L, 1L, 1L, 1L),
         fn = c(1L, 2L, 1L, 2L, 0L, 0L),
         delta_relevance = c(0, 0, 0, 0, 0, 0),
         rprec_deno = c(2L, 0L, 0L, 1L, 0L, 1L),
         prec = c(1, NA, NA, 0, 0, 0.5),
         rprec = c(1, NA, NA, 0, NA, 1),
         rec = c(2/3, 0, 0, 0, NA, 1),
         f1 = c(0.8, 0, 0, 0, 0, 2/3)),
    row.names = c(NA, -6L),
    class = c("tbl_df", "tbl", "data.frame"))
  expected_per_subj_grouped <- dplyr::group_by(expected_per_subj, .data$label_id)

  expect_equal(res_per_subj_dplyr, expected_per_subj_grouped)
  # check out collapse variant
  expect_equal(res_per_subj_collapse$results_table, expected_per_subj)

  #detach("package:purrr")
})

test_that("grouping vars with dots are rejected", {

  gold <- tibble::tribble(
    ~doc_id, ~label_id, ~hsg,
    "A", "a", "0.1.1",
    "A", "b", "0.1.1",
    "A", "c", "0.1.1",
    "B", "a", "0.1.1",
    "B", "d", "0.1.1",
    "C", "a", "0.2.1",
    "C", "b", "0.2.1",
    "C", "d", "0.2.1",
    "C", "f", "0.2.1"
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "C", "f"
  )

  cmp <- create_comparison(gold, pred, doc_strata = "hsg")
  expect_error(
    object =  compute_intermediate_results(
      cmp, grouping_var = c("doc_id", "hsg")),
    regexp = "grouping variable must not contain levels that contain dots"
  )

  label_dict <- tibble::tribble(
    ~label_id, ~strata,
    "a", "12.1",
    "b", "12.1",
    "c", "12.1",
    "d", "12.2",
    "e", "12.2",
    "f", "12.3"
  )

  cmp <- create_comparison(gold, pred, label_dict = label_dict)

  expect_error(
    object = compute_intermediate_results(
      cmp, grouping_var = c("label_id", "strata")
    ),
    regexp = "grouping variable must not contain levels that contain dots"
  )
})

test_that("summarise_intermediate_results checks out", {
  intermediate_results <-  structure(
    list(
      doc_id = c("A", "B", "C"),
      n_gold = c(3L, 2L, 4L),
      n_suggested = c(3L, 2L, 1L),
      tp = c(1L, 1L, 1L),
      fp = 2:0,
      fn = c(2L, 1L, 3L),
      prec = c(1/3, 0.5, 1),
      rprec = c(1/3, 0.5, 1),
      rec = c(1/3, 0.5, 0.25),
      f1 = c(1/3, 0.5, 0.4)),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame"))

  expected_res <- tibble::tribble(
    ~metric, ~value, ~support,
    "f1", (1/3 + 0.5 + 0.4)/3, 3,
    "prec", (1/3 + 0.5 + 1)/3 , 3,
    "rec", (1/3 + 0.5 + 0.25)/3, 3,
    "rprec", (1/3 +  0.5 + 1)/3, 3,
  )

  intermediate_results_grpd <- dplyr::group_by(intermediate_results, .data$doc_id)
  actual_res <- summarise_intermediate_results_dplyr_version(intermediate_results_grpd)
  actual_res_collapse <- summarise_intermediate_results(
    list(
      results_table = intermediate_results,
      grouping_var = c("doc_id")
    ))

  expect_equal(actual_res, expected_res)
  expect_equal(actual_res_collapse, expected_res)

})


test_that("f1-score handles missings expectedly in intermediate stage",{

  # label a: 1 gold, no pred --> f1 = 0
  # label b: 0 gold, 1 pred --> f1 = 0
  # label c: 1 gold, 1 pred --> f1 = 1

  gold <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "c",
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "b",
    "A", "c"
  )

  comp <- create_comparison(gold, pred)

  expected_res <- tibble::tribble(
    ~label_id, ~n_gold, ~n_suggested, ~tp, ~fp, ~fn, ~delta_relevance, ~rprec_deno, ~prec, ~rprec, ~rec, ~f1,
    "a", 1L, 0L, 0L, 0L, 1L, 0, 0L, NA_real_, NA_real_, 0, 0,
    "b", 0L, 1L, 0L, 1L, 0L, 0, 0L, 0, NA_real_, NA_real_, 0,
    "c", 1L, 1L, 1L, 0L, 0L, 0, 1L, 1, 1, 1, 1,
  )

  res_collapse <- compute_intermediate_results(comp, "label_id")
  res_dplyr <- compute_intermediate_results_dplyr_version(comp, rlang::syms(c("label_id")))

  expect_equal(
    res_collapse$results_table,
    expected_res
  )

  expect_equal(
    dplyr::ungroup(res_dplyr),
    expected_res
  )


})

test_that("f1-score handles missings expectedly in summarise stage",{

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
      f1 = c(0, 0)),
    class = c("tbl_df", "tbl", "data.frame"))


  expected <- structure(list(
    metric = c("f1", "prec", "rec", "rprec"),
    value = c(0,  0, 0, 0),
    support = c(1, 1, 1, 1)), class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L))

  observed <- summarise_intermediate_results(list(results_table = intermediate_results, grouping_var = c("doc_id", "label_id")))

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
      f1 = c(0, 0)),
    class = c("tbl_df", "tbl", "data.frame"))


  expected <- structure(list(
    metric = c("f1", "prec", "rec", "rprec"),
    value = c(0,  NA, 0, NA),
    support = c(1, 0, 2, 0)), class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L))

  observed <- summarise_intermediate_results(
    list(results_table = intermediate_results,
         grouping_var = c("doc_id", "label_id")))

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
      f1 = c(0, 0)),
    class = c("tbl_df", "tbl", "data.frame"))


  expected <- structure(list(
    metric = c("f1", "prec", "rec", "rprec"),
    value = c(0,  0, NA, NA),
    support = c(1, 2, 0, 0)), class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L))

  observed <- summarise_intermediate_results(
    list(results_table = intermediate_results,
         grouping_var = c("doc_id", "label_id")))

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
      f1 = c(NA, NA)),
    class = c("tbl_df", "tbl", "data.frame"))


  expected <- structure(list(
    metric = c("f1", "prec", "rec", "rprec"),
    value = c(NA_real_,  NA_real_, NA_real_, NA_real_),
    support = c(0, 0, 0, 0)), class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -4L))

  observed <- summarise_intermediate_results(
    list(results_table = intermediate_results,
         grouping_var = c("doc_id", "label_id")))

  expect_equal(observed, expected)
})

test_that("propensity scored rprecision is correct on intermediate level", {
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

  comp <- create_comparison(
    gold = gold,
    pred = pred
  )

  ps_scores <- compute_propensity_scores(label_distribution)

  comp <- comp |>
    dplyr::left_join(ps_scores, by = "label_id")

  mlw <- comp |>
    dplyr::filter(gold) |>
    dplyr::summarise(mean = mean(label_weight)) |>
    dplyr::pull(mean)

  doc_wise_res <- compute_intermediate_results(
    gold_vs_pred = comp,
    grouping_var = "doc_id",
    propensity_scored = TRUE,
    cost_fp = mlw
  )

  # correct denominator for doc-avg propensity scored rprec computation
  # A: 3 gold, 3 suggested, cumsum of propensity scores of
  # first 3 gold labels: 4.462219
  # B: 2 gold, 4 suggested, cumsum of propensity scores of
  # first two gold labels: 10.306136
  # C: 4 gold, 1 suggested, cumsum of propensity scores
  # first gold label: 9.220291
  exp_docwise_res <- tibble::tribble(
    ~doc_id, ~n_gold, ~n_suggested, ~tp, ~fp, ~fn, ~rprec_deno,
    "A", 3, 3, 1.085846, 2*mlw, 1.304366 + 2.072008, 2.072008 + 1.304366 + 1.085846,
    "B", 2, 4, 1.085846, 3*mlw, 9.220291, 9.220291 + 1.085846,
    "C", 4, 1, 7.831511, 0, 9.220291 + 1.304366 + 1.085846, 9.220291
  )

  expect_equal(
    doc_wise_res$results_table |>
      dplyr::select(-c(rprec, prec,rec,f1, delta_relevance)),
    exp_docwise_res, tolerance = 1e-6
  )


  label_wise_res <- compute_intermediate_results(
    gold_vs_pred = comp,
    grouping_var = "label_id",
    propensity_scored = TRUE,
    cost_fp = mlw
  )

  exp_label_wise_res <- tibble::tribble(
    ~label_id, ~n_gold, ~n_suggested,        ~tp, ~fp,        ~fn, ~rprec_deno,
    "a",             3,            2, 2*1.085846,   0,   1.085846,  2*1.085846,
    "b",             2,            0,          0,   0, 2*1.304366,           0,
    "c",             1,            1,          0,   mlw,   2.072009,    2.072009,
    "d",             2,            1,          0,   mlw, 2*9.220291,    9.220291,
    "e",             0,            1,          0,   mlw,          0,           0,
    "f",             1,            3,   7.831511,   2*mlw,          0,    7.831511
  )

  expect_equal(
    label_wise_res$results_table |>
      dplyr::select(-c(rprec, prec,rec,f1, delta_relevance)),
    exp_label_wise_res, tolerance = 1e-6
  )

  case_wise_res <- compute_intermediate_results(
    gold_vs_pred = comp,
    grouping_var = c("doc_id", "label_id"),
    propensity_scored = TRUE,
    cost_fp = mlw
  )

  exp_case_wise_res <- tibble::tribble(
    ~doc_id, ~label_id, ~n_gold, ~n_suggested,      ~tp, ~fp, ~fn,
        "A",       "a",       1,            1, 1.085846,   0,   0,
        "A",       "b",       1,            0,        0,   0,   1.304366,
        "A",       "c",       1,            0,        0,   0,   2.072008,
        "B",       "a",       1,            1, 1.085846,   0,   0,
        "B",       "d",       1,            0,        0,   0,   9.220291,
        "C",       "a",       1,            0,        0,   0,   1.085846,
        "C",       "b",       1,            0,        0,   0,   1.304366,
        "C",       "d",       1,            0,        0,   0,   9.220291,
        "C",       "f",       1,            1, 7.831511,   0,   0,
        "A",       "d",       0,            1,        0,   mlw,   0,  # 9.220291
        "A",       "f",       0,            1,        0,   mlw,   0,  # 7.831511
        "B",       "e",       0,            1,        0,   mlw,   0,  #9.220291
        "B",       "f",       0,            1,        0,   mlw,   0,  #7.831511
        "B",       "c",       0,            1,        0,   mlw,   0,  #2.072008
  )

  expect_equal(
    case_wise_res$results_table |>
      dplyr::select(-c(rprec, prec,rec,f1,rprec_deno, delta_relevance)),
    dplyr::arrange(exp_case_wise_res, doc_id, label_id), tolerance = 1e-6
  )
})

test_that(
  "intermediate results for graded relevance are computed correctly",
  {
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

    pred_w_relevance <- tibble::tribble(
      ~doc_id, ~label_id, ~relevance,
      "A", "a", 1.0,
      "A", "d", 0.0,
      "A", "f", 0.0,
      "B", "a", 1.0,
      "B", "e", 1/3,
      "C", "f", 1.0,
      "C", "e", 1/3
    )

    comp <- create_comparison(
      gold, pred_w_relevance,
      graded_relevance = TRUE
      )

    res_collapse <- compute_intermediate_results(
      gold_vs_pred = comp,
      grouping_var = "doc_id")

    res_dplyr <- compute_intermediate_results_dplyr_version(
      gold_vs_pred = comp,
      grouping_var = rlang::syms("doc_id")
    )

    expected <- tibble::tribble(
      ~doc_id, ~prec, ~rec,
      "A", 0.333, 0.333,
      "B", 0.667, 0.571,
      "C", 0.667, 0.308
    )

   expect_equal(
      res_dplyr |>
        dplyr::select(dplyr::all_of(c("doc_id", "prec", "rec"))) |>
        dplyr::ungroup(),
      expected,
      tolerance = 1e-3
    )

   expect_equal(
     res_collapse$results_table |>
       dplyr::select(dplyr::all_of(c("doc_id", "prec", "rec"))),
     expected,
     tolerance = 1e-3
   )

   micro_res_dplyr <- compute_intermediate_results_dplyr_version(
     gold_vs_pred = comp,
     grouping_var = rlang::syms(c("doc_id", "label_id"))
   ) |>
   summarise_intermediate_results_dplyr_version()

   micro_res_collapse <- compute_intermediate_results(
     gold_vs_pred = comp,
     grouping_var = c("doc_id", "label_id")
   ) |>
   summarise_intermediate_results()

   # summarise intermediate results should account for delta_delevance
   # when computing mirro averages
   micro_expected <- tibble::tribble(
     ~metric, ~value, ~support,
     "f1", 0.44, 8,
     "prec",(3 + 2/3) / 7, 7,
     "rec", (3 + 2/3) / (9 + 2/3), 9,
     "rprec", 0.5238095, 7
   )

   expect_equal(
     micro_res_dplyr,
     micro_expected,
     tolerance = 1e-6
   )

   expect_equal(
     micro_res_collapse,
     micro_expected,
     tolerance = 1e-6
   )
  }
)

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
  label_wise_res_no_weight$results_table <- label_wise_res_no_weight$results_table |>
    dplyr::left_join(weights, by = "label_id")
  expected_res_weighted_mean <- dplyr::summarise(
    .data = label_wise_res_no_weight$results_table,
    dplyr::across(
      prec:f1,
      .fns =  ~ sum(.x * .data$label_weight, na.rm = TRUE) / sum(as.numeric(!is.na(.x)) * .data$label_weight, na.rm = TRUE))
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
