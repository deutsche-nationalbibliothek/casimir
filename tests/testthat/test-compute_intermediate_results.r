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
  res_per_doc_dplyr <- casimir:::compute_intermediate_results_dplyr(
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
      prec = c(1 / 3, 0.5, 1),
      rprec = c(1 / 3, 0.5, 1),
      rec = c(1 / 3, 0.5, 0.25),
      f1 = c(1 / 3, 0.5, 0.4)
    ),
    row.names = c(NA, -3L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expected_per_doc_grouped <- dplyr::group_by(expected_per_doc, .data$doc_id)


  expect_equal(res_per_doc_dplyr, expected_per_doc_grouped)
  # also for collapse variant
  expect_equal(res_per_doc_collapse$results_table, expected_per_doc)

  res_per_subj_dplyr <- casimir:::compute_intermediate_results_dplyr(
    compare_gold_vs_pred,
    grouping_var = rlang::syms(c("label_id"))
  )
  res_per_subj_collapse <- compute_intermediate_results(
    compare_gold_vs_pred,
    grouping_var = c("label_id")
  )

  expected_per_subj <- structure(
    list(
      label_id = c("a", "b", "c", "d", "e", "f"),
      n_gold = c(3L, 2L, 1L, 2L, 0L, 1L),
      n_suggested = c(2L, 0L, 0L, 1L, 1L, 2L),
      tp = c(2L, 0L, 0L, 0L, 0L, 1L),
      fp = c(0L, 0L, 0L, 1L, 1L, 1L),
      fn = c(1L, 2L, 1L, 2L, 0L, 0L),
      delta_relevance = c(0, 0, 0, 0, 0, 0),
      rprec_deno = c(2L, 0L, 0L, 1L, 0L, 1L),
      prec = c(1, NA, NA, 0, 0, 0.5),
      rprec = c(1, NA, NA, 0, NA, 1),
      rec = c(2 / 3, 0, 0, 0, NA, 1),
      f1 = c(0.8, 0, 0, 0, 0, 2 / 3)
    ),
    row.names = c(NA, -6L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expected_per_subj_grouped <- dplyr::group_by(
    expected_per_subj, .data$label_id
  )

  expect_equal(res_per_subj_dplyr, expected_per_subj_grouped)
  # check out collapse variant
  expect_equal(res_per_subj_collapse$results_table, expected_per_subj)
})

test_that("grouping vars with dots are rejected", {

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
  )

  doc_groups <- tibble::tribble(
    ~doc_id, ~hsg,
    "A", "0.1.1",
    "B", "0.1.1",
    "C", "0.2.1"
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

  cmp <- create_comparison(gold, pred, doc_groups = doc_groups)
  expect_error(
    object =  compute_intermediate_results(
      cmp, grouping_var = c("doc_id", "hsg")
    ),
    regexp = "grouping variable must not contain levels that contain dots"
  )

  label_groups <- tibble::tribble(
    ~label_id, ~strata,
    "a", "12.1",
    "b", "12.1",
    "c", "12.1",
    "d", "12.2",
    "e", "12.2",
    "f", "12.3"
  )

  cmp <- create_comparison(gold, pred, label_groups = label_groups)

  expect_error(
    object = compute_intermediate_results(
      cmp, grouping_var = c("label_id", "strata")
    ),
    regexp = "grouping variable must not contain levels that contain dots"
  )
})

test_that("f1-score handles missings expectedly in intermediate stage", {

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
    ~label_id, ~n_gold, ~n_suggested, ~tp, ~fp, ~fn, ~delta_relevance, ~rprec_deno, ~prec, ~rprec, ~rec, ~f1, # nolint
    "a", 1L, 0L, 0L, 0L, 1L, 0, 0L, NA_real_, NA_real_, 0, 0,
    "b", 0L, 1L, 0L, 1L, 0L, 0, 0L, 0, NA_real_, NA_real_, 0,
    "c", 1L, 1L, 1L, 0L, 0L, 0, 1L, 1, 1, 1, 1,
  )

  res_collapse <- compute_intermediate_results(comp, "label_id")
  res_dplyr <- compute_intermediate_results_dplyr(
    comp, rlang::syms(c("label_id"))
  )

  expect_equal(
    res_collapse$results_table,
    expected_res
  )

  expect_equal(
    dplyr::ungroup(res_dplyr),
    expected_res
  )


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
    "A", 3, 3, 1.085846, 2 * mlw, 1.304366 + 2.072008, 2.072008 + 1.304366 + 1.085846, # nolint
    "B", 2, 4, 1.085846, 3 * mlw, 9.220291, 9.220291 + 1.085846,
    "C", 4, 1, 7.831511, 0, 9.220291 + 1.304366 + 1.085846, 9.220291
  )

  expect_equal(
    doc_wise_res$results_table |>
      dplyr::select(-c(rprec, prec, rec, f1, delta_relevance)),
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
    "a",             3,            2, 2*1.085846,   0,   1.085846,  2 * 1.085846, # nolint
    "b",             2,            0,          0,   0, 2 * 1.304366,           0, # nolint
    "c",             1,            1,          0,   mlw,   2.072009,    2.072009, # nolint
    "d",             2,            1,          0,   mlw, 2*9.220291,    9.220291, # nolint
    "e",             0,            1,          0,   mlw,          0,           0, # nolint
    "f",             1,            3,   7.831511,   2*mlw,          0,    7.831511 # nolint
  )

  expect_equal(
    label_wise_res$results_table |>
      dplyr::select(-c(rprec, prec, rec, f1, delta_relevance)),
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
      dplyr::select(-c(rprec, prec, rec, f1, rprec_deno, delta_relevance)),
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
      "B", "e", 1 / 3,
      "C", "f", 1.0,
      "C", "e", 1 / 3
    )

    comp <- create_comparison(
      gold, pred_w_relevance,
      graded_relevance = TRUE
    )

    res_collapse <- compute_intermediate_results(
      gold_vs_pred = comp,
      grouping_var = "doc_id"
    )

    res_dplyr <- compute_intermediate_results_dplyr(
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

    micro_res_dplyr <- compute_intermediate_results_dplyr(
      gold_vs_pred = comp,
      grouping_var = rlang::syms(c("doc_id", "label_id"))
    ) |>
      summarise_intermediate_results_dplyr()

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
      "prec", (3 + 2 / 3) / 7, 7,
      "rec", (3 + 2 / 3) / (9 + 2 / 3), 9,
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
