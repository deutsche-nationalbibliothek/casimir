test_that("pr_auc is computed correctly",{

  pr_curve_ideal <- tibble::tribble(
    ~searchspace_id, ~rec, ~prec_cummax,
    1, 1.0, 1.0,
    2, 0.0, 1.0
  )

  # ideal data gives auc 1
  expect_equal(
    compute_pr_auc_from_curve(pr_curve_ideal, grouping_vars = NULL)$pr_auc[1],
    1.0
  )

  pr_curve_diag <- tibble::tribble(
    ~searchspace_id, ~rec, ~prec_cummax,
    5, 0.0, 1.0,
    4, 0.25, 0.75,
    3, 0.5, 0.5,
    2, 0.75, 0.25,
    1, 1.0, 0.0
  )

  # diagonal curve gives auc 0.5
  expect_equal(
    compute_pr_auc_from_curve(pr_curve_diag, grouping_vars = NULL)$pr_auc[1],
    0.5
  )

  pr_curve_non_monotone <- tibble::tribble(
    ~searchspace_id, ~rec, ~prec_cummax,
    5, 0.0, 1.0,
    4, 0.25, 0.75,
    3, 0.5, 0.8,
    2, 0.75, 0.25,
    1, 1.0, 0.0
  )

  # expect that non-monotone input is rejected
  expect_error(
    compute_pr_auc_from_curve(pr_curve_non_monotone, grouping_vars = NULL),
    regexp = "all\\(utils::head\\(test_monotonicity\\[\\[\"increment_postive\"\\]\\], -1\\),.* is not TRUE"
  )

  pr_curve <- tibble::tribble(
    ~searchspace_id, ~prec, ~rec, ~prec_cummax,
    1, 0.5, 0.639, 0.5,
    2, 0.5, 0.639, 0.5,
    3, 0.278, 0.278, 0.5,
    4, 0.5, 0.278, 0.5,
    5, 0.5, 0.278, 0.5,
    6, 0.5, 0.278, 0.5,
    7, 0.75, 0.278, 0.75,
    8, 1, 0.278, 1,
    9, 1, 0.278, 1,
    10, 1, 0.111, 1,
    11, NA, 0, 1,
  )

  # test if the auc computation works for the more complicated scenario
  expect_equal(
    compute_pr_auc_from_curve(pr_curve, grouping_vars = NULL)$pr_auc[1],
    0.4585,
    tolerance = 1e-4
  )

  pr_curve_grpd <- tibble::tribble(
    ~searchspace_id, ~rec, ~prec_cummax, ~model,
    5, 0.0, 1.0, "M1",
    4, 0.25, 0.75, "M1",
    3, 0.5, 0.5, "M1",
    2, 0.75, 0.25, "M1",
    1, 1.0, 0.0, "M2",
    5, 0.0, 1.0, "M2",
    4, 0.25, 1.0, "M2",
    3, 0.5, 1.0, "M2",
    2, 0.75, 1.0, "M2",
    1, 1.0, 0.0, "M2",
    5, 0.0, 0.1, "M3",
    4, 0.25, 0.1, "M3",
    3, 0.5, 0.1, "M3",
    2, 0.75, 0.1, "M3",
    1, 1.0, 0.1, "M3",
  )

  res_grpd <- compute_pr_auc_from_curve(pr_curve_grpd, grouping_vars = "model")

  exp_grpd <- tibble::tribble(
    ~model, ~pr_auc,
    "M1", 0.469,
    "M2", 0.875,
    "M3", 0.1,
  )
  expect_equal(
    res_grpd,
    exp_grpd,
    tolerance = 1e-3
  )

})

testthat::test_that("ci for pr_auc work", {

  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  set.seed(20)
  n_docs <- 20
  n_label <- 26
  gold_hsg <- tibble::tibble(
    doc_id = LETTERS[1:n_docs],
    hsg = sample(c("001", "002"), size = n_docs, replace = TRUE)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(label_id = list(sample(letters[1:n_label], size = 5, replace = FALSE))) |>
    tidyr::unnest(label_id)

  pred <- gold_hsg$doc_id |>
    unique() |>
    purrr::map_dfr(
      .f = ~tibble::tibble(
        doc_id = .x,
        label_id = sample(letters[1:n_label], size = n_label, replace = FALSE),
        score = runif(n_label)
      )
    ) |>
    dplyr::group_by(doc_id) |>
    dplyr::mutate(rank = dplyr::min_rank(-.data$score)) |>
    dplyr::ungroup()

  # here are frozen results that have been computed for this random input
  # previously
  expected_pr_auc <-structure(
    list(pr_auc = 0.1923077,
         ci_lower = c(0.1923077),
         ci_upper = c(0.2173132)),
    row.names = c(NA, -1L),
    class = c("data.frame"))

  expect_error(compute_pr_auc(gold_hsg, pred, steps = 15, mode = "subj-avg",
                 compute_bootstrap_ci = TRUE,
                 n_bt = 20L),
               regexp = "Confidence intervals for pr-auc in subj-avg-mode are not supported yet")

  pr_auc <- compute_pr_auc(gold_hsg, pred, steps = 15,
                           compute_bootstrap_ci = TRUE,
                           seed = 3426,
                           n_bt = 20L)


  expect_equal(pr_auc, expected_pr_auc, tolerance = 10e-5)

  detach("package:purrr")
})

test_that("applying limit_range works", {

  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  # purrr would cause an attach massage otherwise

  pr_auc <- expect_silent(compute_pr_auc(
    gold_standard = dnb_gold_standard,
    predicted = dnb_test_predictions,
    limit_range = 1:5,
    steps = 10
  ))

  expect_equal(
    pr_auc$pr_auc,
    0.3194, tolerance = 1e-4
  )
  detach("package:purrr")
})


test_that("Zero AUC for singleton-curve in empty label_strata", {

  library(purrr, quietly = TRUE, warn.conflicts = FALSE)

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
    "C", "i", # only label from gnd_entity location, not predicted
    "D", "a",
    "D", "c",
    "D", "e",
    "D", "f",
  )

  pred <- tibble::tribble(
    ~doc_id, ~label_id, ~score,
    "A", "a", 0.9,
    "A", "d", 0.2,
    "A", "f", 0.1,
    "B", "a", 0.8,
    "B", "e", 0.5,
    "B", "g", 0.3,
    "C", "f", 0.9,
    "D", "a", 0.1,
    "D", "c", 0.2,
    "D", "h", 0.3 # only label from gnd_entity works, not in gold
  )

  label_dict <- tibble::tribble(
    ~label_id, ~gnd_entity,
    "a", "pers",
    "b", "pers",
    "c", "subjh",
    "d", "subjh",
    "e", "conf",
    "f", "conf",
    "g", "subjh",
    "h", "works",
    "i", "location"
  )

  res <- compute_pr_auc(gold, pred, label_dict = label_dict, steps = 10)

  edge_cases_expected <- tibble::tribble(
    ~gnd_entity, ~pr_auc,
    "location", 0.0,
    "works", 0.0
  )

  edge_cases_actual <- res |>
    dplyr::filter(gnd_entity %in% c("location", "works"))

  expect_equal(edge_cases_actual, edge_cases_expected)
  detach("package:purrr")
})

test_that("pr_auc with propensity scored metrics works", {

  library(purrr, quietly = TRUE, warn.conflicts = FALSE)

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
    ~doc_id, ~label_id, ~score,
    "A", "a", 0.9,
    "A", "d", 0.2,
    "A", "f", 0.1,
    "B", "a", 0.81,
    "B", "e", 0.5,
    "B", "f", 0.3,
    "B", "c", 0.1,
    "C", "f", 0.05,
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
    compute_pr_auc(
      gold_standard = gold,
      predicted = pred,
      propensity_scored = TRUE,
      label_distribution = label_distribution,
      steps = 10,
      mode = "doc-avg",
      compute_bootstrap_ci = FALSE
    )
  )

  detach("package:purrr")
})
