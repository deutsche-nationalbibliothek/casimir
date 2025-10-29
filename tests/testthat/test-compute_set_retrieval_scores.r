test_that("compute_set_retrieval_scores works", {
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)

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
    ~doc_id, ~label_id, ~relevance,
    "A", "a", 1.0,
    "A", "d", 0.0,
    "A", "f", 0.0,
    "B", "a", 1.0,
    "B", "e", 1 / 3,
    "C", "f", 1.0,
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

  expected_results <- list()
  expected_results[["doc-avg"]] <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "f1", "doc-avg", (1 / 3 + 1 / 2 + 2 / 5) / 3, 3L,
    "prec", "doc-avg", (1 + 1 / 2 + 1 / 3) / 3, 3L,
    "rec", "doc-avg", (1 / 3 + 1 / 2 + 1 / 4) / 3, 3L,
    "rprec", "doc-avg", (1 / 3 + 1 / 2 + 1) / 3, 3L
  )

  expected_results[["subj-avg"]] <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "f1", "subj-avg", (0.8 + 2 / 3) / 6, 6L,
    "prec", "subj-avg", (1 + 1 / 2) / 4, 4L,
    "rec", "subj-avg", (2 / 3 + 1) / 5, 5L,
    "rprec", "subj-avg", (1 + 1 + 0) / 3, 3L
  )

  expected_results[["micro"]] <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "f1", "micro", 2 * (3 / 6 * 3 / 9) / (3 / 6 + 3 / 9), 7.5,
    "prec", "micro", 3 / 6, 6,
    "rec", "micro", 3 / 9, 9,
    "rprec", "micro", 3 / 6, 6
  )

  # check that results are correct across all three aggregation modes
  res <- purrr::imap(
    .x = expected_results,
    .f = ~ expect_equal(
      object = compute_set_retrieval_scores_dplyr(
        pred, gold,
        mode = .y,
        ignore_inconsistencies = TRUE
      ),
      expected = .x
    )
  )
  # results with collapse internals work
  res_collapse <- purrr::imap(
    .x = expected_results,
    .f = ~ expect_equal(
      object = compute_set_retrieval_scores(
        pred, gold,
        mode = .y,
        ignore_inconsistencies = TRUE
      ),
      expected = .x
    )
  )

  # check that all scenarios run through silently
  configuration <- expand.grid(
    .mode = c("doc-avg", "subj-avg", "micro"),
    .compute_bootstrap_ci = c(TRUE, FALSE),
    .graded_relevance = c(TRUE, FALSE),
    .propensity_scored = c(TRUE, FALSE),
    .cost_fp_constant = c(NULL, "mean", "max", "min"),
    stringsAsFactors = FALSE
  )
  configuration <- dplyr::filter(
    configuration,
    !(.graded_relevance & .propensity_scored) # not supported
  )

  res_across_config <- purrr::pmap(
    configuration,
    .f = function(.mode,
                  .compute_bootstrap_ci,
                  .graded_relevance,
                  .propensity_scored,
                  .cost_fp_constant) {
      expect_silent(
        object = compute_set_retrieval_scores_dplyr(
          pred,
          gold,
          mode = .mode,
          seed = 10,
          graded_relevance = .graded_relevance,
          compute_bootstrap_ci = .compute_bootstrap_ci,
          propensity_scored = .propensity_scored,
          label_distribution = label_distribution,
          cost_fp_constant = .cost_fp_constant,
          ignore_inconsistencies = TRUE,
          progress = FALSE
        )
      )
    }
  )

  # test the same for collapse version
  res_across_config_collapse <- purrr::pmap(
    configuration,
    .f = function(.mode, .compute_bootstrap_ci, .graded_relevance,
                  .propensity_scored, .cost_fp_constant) {
      expect_silent(
        object = compute_set_retrieval_scores(
          pred,
          gold,
          mode = .mode,
          seed = 10,
          graded_relevance = .graded_relevance,
          compute_bootstrap_ci = .compute_bootstrap_ci,
          propensity_scored = .propensity_scored,
          label_distribution = label_distribution,
          cost_fp_constant = .cost_fp_constant,
          ignore_inconsistencies = TRUE,
          progress = FALSE
        )
      )
    }
  )

  expect_equal(
    map(res_across_config, dplyr::ungroup),
    res_across_config_collapse
  )

  detach("package:purrr")
})

test_that("f1 score handles missings expectedly for micro-avg", {
  # test if micro average F1 still gives 0 if one of the
  # constituents is missing
  # label a: 1 gold, 0 pred --> prec = NA, rec = 0, f1 = 0

  gold <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id,
  )

  expected_micro <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "f1", "micro", 0, 0.5,
    "prec", "micro", NA_real_, 0,
    "rec", "micro", 0, 1,
    "rprec", "micro", NA_real_, 0
  )

  expect_warning(
    res_collapse <- compute_set_retrieval_scores(pred, gold, mode = "micro"),
    "gold standard data contains documents that are not in predicted set"
  )
  expect_warning(
    res_dplyr <- compute_set_retrieval_scores_dplyr(pred, gold, mode = "micro"),
    "gold standard data contains documents that are not in predicted set"
  )

  expect_equal(
    res_collapse, expected_micro
  )

  expect_equal(
    res_dplyr, expected_micro
  )
})

test_that("graded relevance metrics are computed correctly", {
  # reference implementation adapted from Markus' code
  reference_graded_relevance <- function(gold_standard, predicted) {
    comp <- create_comparison(
      dplyr::filter(predicted, !is.na(relevance)),
      gold_standard,
      ignore_inconsistencies = TRUE
    ) |>
      # remove artificially generated relevance = 0 column
      # because create_comparison overwrites relevance column
      # if graded_relevance == FALSE
      dplyr::select(-relevance) |>
      # join actual relevance column for the test
      dplyr::left_join(
        predicted,
        by = c("doc_id", "label_id")
      ) |>
      dplyr::mutate(relevance = dplyr::case_when(
        gold ~ 1.0, # gold standard (tp and fn)
        # is.na(relevance) ~ 0.0, # fp without relevance
        # relevance == 1.0 ~ 2/3, # fp with inconsistent relevance
        TRUE ~ relevance # remaining fp with relevance != 1.0
      ))

    comp |>
      dplyr::group_by(doc_id) |>
      dplyr::summarise(
        GP = mean(relevance[suggested]),
        GR = sum(relevance[suggested]) / sum(relevance),
      ) |>
      dplyr::summarise(
        GP = mean(GP, na.rm = TRUE),
        GR = mean(GR, na.rm = TRUE)
      )
  }

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

  pred_no_relevance <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e",
    "C", "f",
    "C", "e"
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

  # GP and GR should be larger then binary precision and recall
  binary_relevance <- compute_set_retrieval_scores(
    pred_no_relevance,
    gold,
    graded_relevance = FALSE,
    mode = "doc-avg"
  ) |>
    # prefix all column names by "binary_"
    dplyr::rename_with(~ paste0("binary_", .x))

  graded_relevance <- compute_set_retrieval_scores(
    pred_w_relevance,
    gold,
    graded_relevance = TRUE,
    mode = "doc-avg"
  ) |>
    # prefix all column names by "graded_"
    dplyr::rename_with(~ paste0("graded_", .x))

  n_monotonicity_faults <- dplyr::bind_cols(
    binary_relevance, graded_relevance
  ) |>
    dplyr::filter(graded_value < binary_value) |>
    nrow()

  expect_equal(
    n_monotonicity_faults, 0,
    info = "GP and GR should be larger then binary precision and recall"
  )

  # results should match reference implementation
  reference_res <- reference_graded_relevance(gold, pred_w_relevance)

  casimir_res <- compute_set_retrieval_scores(
    pred_w_relevance,
    gold,
    graded_relevance = TRUE,
    rename_metrics = TRUE,
    mode = "doc-avg"
  )

  casimir_gp <- casimir_res |>
    dplyr::filter(metric == "g-prec") |>
    dplyr::pull(value)

  casimir_gr <- casimir_res |>
    dplyr::filter(metric == "g-rec") |>
    dplyr::pull(value)

  expect_equal(reference_res$GP, casimir_gp,
    info = "results should match reference implementation"
  )
  expect_equal(reference_res$GR, casimir_gr,
    info = "results should match reference implementation"
  )

  # expect a warning if inconsistent data is passed
  inconsistent_pred_w_relevance <- tibble::tribble(
    ~doc_id, ~label_id, ~relevance,
    "A", "a", 2 / 3, # inconsistent with gold standard "A", "a"
    "A", "d", 0.0,
    "A", "f", 0.0,
    "B", "a", 1.0,
    "B", "e", 1 / 3,
    "C", "f", 1.0,
    "C", "e", 1 / 3
  )

  expect_warning(
    compute_set_retrieval_scores(
      inconsistent_pred_w_relevance,
      gold,
      graded_relevance = TRUE,
      mode = "doc-avg"
    ),
    paste(
      "There are 1 inconsistent relevance values with relevance < 1 but",
      "gold_standard = TRUE."
    )
  )

  # expect warning if missing values in relevance occur
  pred_w_missing_relevance <- tibble::tribble(
    ~doc_id, ~label_id, ~relevance,
    "A", "a", 1.0,
    "A", "d", 0.0,
    "A", "f", NA_real_,
    "B", "a", 1.0,
    "B", "e", 1 / 3,
    "C", "f", 1.0,
    "C", "e", 1 / 3
  )

  expect_warning(
    compute_set_retrieval_scores(
      pred_w_missing_relevance,
      gold,
      graded_relevance = TRUE,
      mode = "doc-avg"
    ),
    "NA values in 'relevance' column. Removing rows with NA values."
  )

  # expect warning if relevance is 1 but gold == FALSE
  pred_w_false_gold <- tibble::tribble(
    ~doc_id, ~label_id, ~relevance,
    "A", "a", 1.0,
    "A", "d", 0.0,
    "A", "f", 0.0,
    "B", "a", 1.0,
    "B", "e", 1 / 3,
    "C", "f", 1.0,
    "C", "e", 1.0 # not in gold standard
  )

  expect_warning(
    compute_set_retrieval_scores(
      pred_w_false_gold,
      gold,
      graded_relevance = TRUE,
      mode = "doc-avg"
    ),
    paste(
      "There are 1 inconsistent relevance values with relevance == 1",
      "but gold_standard = FALSE."
    )
  )
})

test_that("propensity scores works", {
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

  expect_silent(
    compute_set_retrieval_scores(
      pred = pred,
      gold = gold,
      mode = "doc-avg",
      propensity_scored = TRUE,
      label_distribution = label_distribution,
      cost_fp_constant = "mean"
    )
  )

  # check that errors occur if a label has no match in label_distribution
  expect_error(
    compute_set_retrieval_scores(
      pred = pred,
      gold = gold,
      mode = "doc-avg",
      propensity_scored = TRUE,
      label_distribution = dplyr::filter(label_distribution, label_id != "a"),
      cost_fp_constant = "mean"
    ),
    "Label distribution does not match input data."
  )

  faulty_label_distribution <- tibble::tribble(
    ~label_id, ~label_freq, ~n_docs,
    "a", 10000, 10100,
    "b", 1000, 10100,
    "b", 1002, 10100, # two entries for label b should cause errors
    "c", 100, 10100,
    "d", 1, 10100,
    "e", 1, 10100,
    "f", 2, 10100,
    "g", 0, 10100
  )

  expect_error(
    compute_set_retrieval_scores(
      pred = pred,
      gold = gold,
      mode = "doc-avg",
      propensity_scored = TRUE,
      label_distribution = faulty_label_distribution,
      cost_fp_constant = "mean"
    ),
    "Label distribution does not match input data."
  )

  expected_results <- list()
  expected_results[["doc-avg"]] <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "f1", "doc-avg", 0.2782352, 3L,
    "prec", "doc-avg", 0.403974, 3L,
    "rec", "doc-avg", 0.250505, 3L,
    "rprec", "doc-avg", 0.399360, 3L
  )

  expected_results[["subj-avg"]] <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    # nolint start
    "f1", "subj-avg", (1.085846 * 0.8 + 7.831511 * 0.5) / sum(1.085846 + 1.304366 + 2.072008 + 9.220291 + 9.220291 + 7.831511), 6L,
    "prec", "subj-avg", (1.085846 * 1.0 + 7.831511 * 1 / 3) / sum(1.085846 + 2.072008 + 9.220291 + 9.220291 + 7.831511), 5L,
    "rec", "subj-avg", (1.085846 * 2 / 3 + 7.831511 * 1.0) / sum(1.085846 + 1.304366 + 2.072008 + 9.220291 + 7.831511), 5L,
    "rprec", "subj-avg", (1.085846 * 1.0 + 7.831511 * 1.0) / sum(1.085846 + 2.072008 + 9.220291 + 7.831511), 4L
    # nolint end
  )

  expected_results[["micro"]] <- tibble::tribble(
    ~metric, ~mode, ~value, ~support,
    "f1", "micro", 0.3164602, 8.5,
    "prec", "micro", 0.3448314, 8L,
    "rec", "micro", 0.29240, 9L,
    "rprec", "micro", 0.3448314, 8L
  )

  res_collapse <- purrr::imap(
    .x = expected_results,
    .f = ~ expect_equal(
      object = compute_set_retrieval_scores(
        pred, gold,
        mode = .y, propensity_scored = TRUE,
        label_distribution = label_distribution,
        cost_fp_constant = "mean"
      ),
      expected = .x,
      tolerance = 1e-5
    )
  )

  res_dplyr <- purrr::imap(
    .x = expected_results,
    .f = ~ expect_equal(
      object = compute_set_retrieval_scores_dplyr(
        pred, gold,
        mode = .y, propensity_scored = TRUE,
        label_distribution = label_distribution,
        cost_fp_constant = "mean"
      ),
      expected = .x,
      tolerance = 1e-5
    )
  )
})

test_that("altering cost_fp works", {
  test_cost_fp <- function(cost_fp) {
    compute_set_retrieval_scores(
      gold = dnb_gold_standard,
      pred = dnb_test_predictions,
      mode = "doc-avg",
      propensity_scored = TRUE,
      label_distribution = dnb_label_distribution,
      cost_fp_constant = cost_fp
    )
  }

  res <- c("mean", "max", "min") |>
    purrr::map(
      .f = ~ expect_silent(
        test_cost_fp(.x)
      )
    )
  # nolint start styler: off
  if (all(res[[1]] == res[[2]]) ||
        all(res[[1]] == res[[3]]) ||
        all(res[[2]] == res[[3]])
  ) {
    stop("altering cost_fp_constant does not change the results")
  }
  # nolint end styler on

  expect_error(
    test_cost_fp("foo"),
    "cost_fp_constant must be a numeric value > 0 or one of
           'max', 'min', 'mean'; not cost_fp_constant = foo"
  )
})

test_that(paste(
  "compute_set_retrieval_scores with propensity_scores",
  "is equivalent to building blocks"
), {
  res_wrapped <- compute_set_retrieval_scores(
    gold = dnb_gold_standard,
    pred = dnb_test_predictions,
    mode = "doc-avg",
    propensity_scored = TRUE,
    label_distribution = dnb_label_distribution,
    cost_fp_constant = NULL
  )

  compare <- create_comparison(
    predicted = dnb_test_predictions,
    gold_standard = dnb_gold_standard,
    propensity_scored = TRUE,
    label_distribution = dnb_label_distribution
  )

  intermed <- compute_intermediate_results(
    gold_vs_pred = compare,
    grouping_var = "doc_id",
    propensity_scored = TRUE
  )

  res <- summarise_intermediate_results(intermed)

  expect_equal(
    res,
    dplyr::select(res_wrapped, -"mode")
  )
})

test_that(paste(
  "compute_set_retrieval_scores with graded relevance is",
  "equivalent to building blocks"
), {
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

  res_wrapped <- compute_set_retrieval_scores(
    pred_w_relevance,
    gold,
    graded_relevance = TRUE,
    mode = "doc-avg"
  )

  comp <- create_comparison(
    predicted = pred_w_relevance,
    gold_standard = gold,
    graded_relevance = TRUE
  )

  intermed <- compute_intermediate_results(comp, "doc_id")
  res <- summarise_intermediate_results(intermed)

  expect_equal(
    res,
    dplyr::select(res_wrapped, -"mode")
  )
})

test_that("limit k is set correctly", {
  df <- dnb_test_predictions |>
    dplyr::group_by(doc_id) |>
    dplyr::mutate(rank = dplyr::row_number(-score)) |>
    dplyr::ungroup()

  pred_at_5 <-  dplyr::filter(df, rank <= 5)

  expected_res <- compute_set_retrieval_scores(
    pred_at_5, dnb_gold_standard
  )

  observed_res <- compute_set_retrieval_scores(
    df, dnb_gold_standard, k = 5
  )

  expect_equal(
    observed_res,
    expected_res
  )

  observed_res_dplyr <- compute_set_retrieval_scores_dplyr(
    df, dnb_gold_standard, k = 5
  )

  expect_equal(observed_res_dplyr, observed_res)

})
