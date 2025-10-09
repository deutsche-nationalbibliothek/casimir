test_that("compute_pr_auc_from_curve is computed correctly", {
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
    regexp = "all\\(utils::head\\(test_monotonicity\\[\\[\"increment_postive\"\\]\\], -1\\),.* is not TRUE" # nolint
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
