test_that("pr curve computation works", {
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
    "A", "d", 0.7,
    "A", "f", 0.3,
    "A", "c", 0.1,
    "B", "a", 0.8,
    "B", "e", 0.6,
    "B", "d", 0.1,
    "C", "f", 0.1,
    "C", "c", 0.2,
    "C", "e", 0.2
  )

  comp <- create_comparison(gold, pred)

  thresholds <- unique(quantile(pred$score, probs = seq(0, 1, 0.1), type = 1))
  # Here is code to "manually" compute the pr curves
  # nolint start
  # library(dplyr)
  # library(purrr)
  # library(tidyr)
  #
  # thresholds |>
  #   map(.f = casimir:::apply_threshold, base_compare = comp) |>
  #   map(
  #     .f = compute_intermediate_results,
  #     grouping_var = c("label_id", "doc_id")
  #   ) |>
  #   map(summarise_intermediate_results) |>
  #   map(.f = ~filter(.x, metric %in% c("prec", "rec"))) |>
  #   map(.f = ~select(.x,-support)) |>
  #   map_dfr(.f = ~pivot_wider(.x, names_from = metric, values_from = value),
  #           .id = "searchspace_id") |>
  #   dput()
  # nolint end


  res <- list(
    "doc-avg" = "doc-avg", "subj-avg" = "subj-avg", "micro" = "micro"
  ) |>
    purrr::map(
      .f = ~ dplyr::arrange(
        expect_silent(
          compute_pr_curve(
            gold, pred,
            mode = .x, thresholds = thresholds
          )$plot_data
        ),
        .data$searchspace_id
      )
    )

  expected <- list()

  expected[["doc-avg"]] <- tibble::tribble(
    ~searchspace_id, ~prec, ~rec, ~prec_cummax, ~mode,
    0L, 0, 0.638888888888889, 0, "doc-avg",
    1L, 0.5, 0.638888888888889, 0.5, "doc-avg",
    2L, 0.277777777777778, 0.277777777777778, 0.5, "doc-avg",
    3L, 0.5, 0.277777777777778, 0.5, "doc-avg",
    4L, 0.75, 0.277777777777778, 0.75, "doc-avg",
    5L, 1, 0.277777777777778, 1, "doc-avg",
    6L, 1, 0.111111111111111, 1, "doc-avg",
    7L, 1, 0, 1, "doc-avg"
  )

  expected[["subj-avg"]] <- tibble::tribble(
    ~searchspace_id, ~prec, ~rec, ~prec_cummax, ~mode,
    0L, 0, 0.633333333333333, 0, "subj-avg",
    1L, 0.5, 0.633333333333333, 0.5, "subj-avg",
    2L, 0.2, 0.133333333333333, 0.5, "subj-avg",
    3L, 0.333333333333333, 0.133333333333333, 0.5, "subj-avg",
    4L, 0.5, 0.133333333333333, 0.5, "subj-avg",
    5L, 1, 0.133333333333333, 1, "subj-avg",
    6L, 1, 0.0666666666666667, 1, "subj-avg",
    7L, 1, 0, 1, "subj-avg"
  )

  expected[["micro"]] <- tibble::tribble(
    ~searchspace_id, ~prec, ~rec, ~prec_cummax, ~mode,
    0L, 0, 0.555555555555556, 0, "micro",
    1L, 0.5, 0.555555555555556, 0.5, "micro",
    2L, 0.285714285714286, 0.222222222222222, 0.5, "micro",
    3L, 0.5, 0.222222222222222, 0.5, "micro",
    4L, 0.666666666666667, 0.222222222222222, 0.666666666666667, "micro",
    5L, 1, 0.222222222222222, 1, "micro",
    6L, 1, 0.111111111111111, 1, "micro",
    7L, 1, 0, 1, "micro"
  )

  # expect the actual curve to be as above
  expect_equal(res, expected, tolerance = 1e-3)

  # test if result can be passed to auc and works out
  expect_equal(
    purrr::map(
      res,
      ~ compute_pr_auc_from_curve(
        .x,
        grouping_vars = NULL
      )$pr_auc[1]
    ),
    list("doc-avg" = 0.4583333, "subj-avg" = 0.3833333, "micro" = 0.3888889),
    tolerance = 1e-4
  )

  # check that all scenarios run through undisturbed
  configuration <- expand.grid(
    .mode = c("doc-avg", "subj-avg", "micro"),
    .steps = c(5, 10, 100),
    .optimize = c(TRUE, FALSE)
  )
  res_across_config <- purrr::pmap(
    configuration,
    .f = function(.mode, .steps, .optimize) {
      expect_silent(
        object = compute_pr_curve(
          gold_standard = gold,
          predicted = pred,
          mode = .mode,
          steps = .steps,
          optimize_cutoff = .optimize
        )
      )
    }
  )

  detach("package:purrr")
})


test_that("grouped pr-auc computation works with doc_groups", {
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)

  # Randomly generated testdata seems not reproducible in R CMD check
  # so we use this code and save the testdata along with the package
  # nolint start
  # set.seed(20)
  # n_docs <- 20
  # n_label <- 26
  # gold_hsg <- tibble::tibble(
  #   doc_id = LETTERS[1:n_docs],
  #   hsg = sample(c("001", "002"), size = n_docs, replace = TRUE)
  # ) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(
  #     label_id = list(sample(letters[1:n_label], size = 5, replace = FALSE))
  #   ) |>
  #   tidyr::unnest(label_id)
  #
  # pred <- gold_hsg$doc_id |>
  #   unique() |>
  #   purrr::map_dfr(
  #     .f = ~tibble::tibble(
  #       doc_id = .x,
  #       label_id = sample(
  #         letters[1:n_label], size = n_label, replace = FALSE),
  #       score = runif(n_label)
  #     )
  #   )
  # nolint end
  load(test_path("testdata/grouped_pr_curve_data_w_doc_strata.rds"))
  doc_groups <- dplyr::distinct(gold_hsg, doc_id, hsg)
  gold <- dplyr::select(gold_hsg, -hsg)

  steps <- 15
  thresholds <- unique(quantile(
    pred$score,
    probs = seq(0, 1, 1 / steps), type = 1
  ))
  # test whether parallel computation yields the same as sequential computation
  pr_curve_by_hsg_parallel <- compute_pr_curve(
    gold, pred,
    doc_groups = doc_groups,
    thresholds = thresholds
  )$plot_data |>
    dplyr::arrange(.data$hsg, .data$searchspace_id) |>
    dplyr::select("hsg", everything())

  pr_curve_by_hsg_sequential <- c("001" = "001", "002" = "002") |>
    purrr::map_dfr(
      .f = function(x) {
        gold_1hsg <- dplyr::filter(gold_hsg, .data$hsg == x)
        pred_1hsg <- dplyr::inner_join(pred,
          dplyr::distinct(gold_1hsg, .data$doc_id),
          by = c("doc_id")
        )

        compute_pr_curve(
          gold_standard = gold_1hsg,
          predicted = pred_1hsg,
          thresholds = thresholds
        )$plot_data
      },
      .id = "hsg"
    ) |>
    dplyr::arrange(.data$hsg, .data$searchspace_id)

  expect_equal(pr_curve_by_hsg_parallel, pr_curve_by_hsg_sequential)

  # expect 2x(number-of-steps + 1) + 2 = 34 rows in the resulting data.frame
  expect_equal(nrow(pr_curve_by_hsg_parallel), 36)

  pr_auc_by_hsg <- compute_pr_auc_from_curve(pr_curve_by_hsg_parallel,
    grouping_vars = "hsg"
  )

  expected_pr_auc_by_hsg <- structure(
    list(
      hsg = c("001", "002"),
      pr_auc = c(0.1962930, 0.2476526)
    ),
    row.names = c(NA, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(pr_auc_by_hsg, expected_pr_auc_by_hsg, tolerance = 10e-5)
})

test_that("grouped pr-auc computation works with label_strata", {
  # Randomly generated testdata seems not reproducible in R CMD check
  # so we use this code and save the testdata along with the package
  # nolint start
  # set.seed(20)
  # n_docs <- 20
  # n_label <- 26
  # gold <- tibble::tibble(
  #   doc_id = LETTERS[1:n_docs],
  # ) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(
  #     label_id = list(sample(letters[1:n_label], size = 5, replace = FALSE))
  #   ) |>
  #   tidyr::unnest(label_id)
  #
  # set.seed(21)
  # pred <- gold$doc_id |>
  #   unique() |>
  #   purrr::map_dfr(
  #     .f = ~tibble::tibble(
  #       doc_id = .x,
  #       label_id = sample(letters[1:n_label], size = n_label, replace = FALSE),
  #       score = runif(n_label)
  #     )
  #   )
  #
  # set.seed(22)
  # label_dict <- tibble::tibble(
  #   label_id = letters[1:n_label],
  #   label_group = sample(
  #     c("group 1", "group 2"), size = n_label, replace = TRUE
  #    )
  # )
  # nolint end
  load(test_path("testdata/grouped_pr_curve_data_w_label_strata.rds"))
  steps <- 15
  thresholds <- unique(quantile(
    pred$score,
    probs = seq(0, 1, 1 / steps), type = 1
  ))
  pr_curve_by_lbl_grp <- compute_pr_curve(gold, pred,
    label_groups = label_dict,
    thresholds = seq(0, 1, 1 / steps)
  )
  # expect 2x(number-of-steps + 1) + 2 = 34 rows in the resulting data.frame
  expect_equal(nrow(pr_curve_by_lbl_grp$plot_data), 36)

  pr_auc_by_label_group <- compute_pr_auc_from_curve(
    pr_curve_by_lbl_grp$plot_dat,
    grouping_vars = "label_group"
  )

  expected_pr_auc_by_label_group <- structure(
    list(
      label_group = c("group 1", "group 2"),
      pr_auc = c(0.2216564, 0.2170787)
    ),
    row.names = c(NA, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )

  expect_equal(
    pr_auc_by_label_group,
    expected_pr_auc_by_label_group,
    tolerance = 10e-5
  )

  detach("package:purrr")
})

test_that("optimal cutoff works", {
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  # purrr would cause an attach massage otherwise

  pr_curve <- expect_silent(compute_pr_curve(
    gold_standard = dnb_gold_standard,
    predicted = dnb_test_predictions,
    limit_range = 1:5,
    steps = 15,
    optimize_cutoff = TRUE
  ))

  expect_equal(
    pr_curve$opt_cutoff$f1_max,
    0.381083,
    tolerance = 1e-6
  )
  detach("package:purrr")
})

test_that("grouped cutoff works", {
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  # Randomly generated testdata seems not reproducible in R CMD check
  # so we use this code and save the testdata along with the package
  # nolint start
  # set.seed(13436)
  # hsg_mapping <- dnb_gold_standard |>
  #   dplyr::distinct(doc_id) |>
  #   dplyr::mutate(
  #     hsg = sample(LETTERS[1:3], size = dplyr::n(), replace = TRUE)
  #    )
  # nolint end
  hsg_mapping <- readRDS(test_path("testdata/random_hsg_mapping.rds"))

  res <- compute_pr_curve(
    gold_standard = dnb_gold_standard,
    predicted = dnb_test_predictions,
    doc_groups = hsg_mapping,
    limit_range = c(1:5),
    steps = 10,
    optimize_cutoff = TRUE
  )

  expect_equal(
    dplyr::arrange(
      res$opt_cutoff, hsg
    )$f1_max,
    c(0.3710678, 0.3453102, 0.4409370),
    tolerance = 1e-5
  )

  auc <- compute_pr_auc_from_curve(
    res,
    grouping_vars = "hsg"
  )

  expect_equal(
    auc$pr_auc,
    c(0.2792714, 0.2880313, 0.4115362),
    tolerance = 1e-5
  )

  detach("package:purrr")
})


test_that("Empty Recall in label strata gives singleton-curve", {
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

  label_groups <- tibble::tribble(
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

  res <- compute_pr_curve(
    gold, pred,
    label_groups = label_groups, steps = 10, mode = "micro"
  )

  # expected last row for entity type "location" should be
  last_row_entity_location <- tibble::tribble(
    ~searchspace_id, ~prec, ~rec, ~prec_cummax, ~gnd_entity, ~mode,
    5, 0.0, 0.0, 0.0, "location", "micro"
  )

  expect_equal(
    res$plot_data |>
      dplyr::filter(gnd_entity == "location", searchspace_id == 5),
    last_row_entity_location
  )

  first_row_entity_works <- tibble::tribble(
    ~searchspace_id, ~prec, ~rec, ~prec_cummax, ~gnd_entity, ~mode,
    0L, 0.0, 0.0, 0.0, "works", "micro"
  )

  expect_equal(
    res$plot_data |>
      dplyr::filter(gnd_entity == "works", searchspace_id == 0L),
    first_row_entity_works
  )

  detach("package:purrr")
})
