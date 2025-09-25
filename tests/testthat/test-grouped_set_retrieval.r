test_that("grouping (doc-strata) of set retrieval computation works", {
  library(purrr, quietly = TRUE, warn.conflicts = FALSE)
  # some dummy results
  gold_001 <- tibble::tribble(
    ~doc_id, ~label_id, ~hsg,
    "A", "a", "001",
    "A", "b", "001",
    "A", "c", "001",
    "B", "a", "001",
    "B", "d", "001",
  )

  gold_002 <- tibble::tribble(
    ~doc_id, ~label_id, ~hsg,
    "C", "a", "002",
    "C", "b", "002",
    "C", "d", "002",
    "C", "f", "002",
    "D", "a", "002",
    "D", "c", "002",
    "D", "e", "002",
    "D", "f", "002"
  )

  gold <- dplyr::bind_rows(gold_001, gold_002)


  pred_001 <- tibble::tribble(
    ~doc_id, ~label_id,
    "A", "a",
    "A", "d",
    "A", "f",
    "B", "a",
    "B", "e"
  )

  pred_002 <- tibble::tribble(
    ~doc_id, ~label_id,
    "C", "f",
    "D", "a",
    "D", "c"
  )

  pred <- dplyr::bind_rows(pred_001, pred_002)

  # expect no error when adding strata
  expect_silent(casimir:::create_comparison(gold, pred, doc_strata = "hsg"))
  # expect NA in hsg-column of compare, when no doc_strata are defined
  compare_wo_explicit_strata <- casimir:::create_comparison(
    gold, pred, doc_strata = NULL
  )
  expect_equal(
    object = nrow(dplyr::filter(compare_wo_explicit_strata, is.na(hsg))),
    expected = 3L
  )

  # check if invalid input is rejected
  if (Sys.getenv("LANG") == "en_US.UTF-8") {
    expect_error(
      compute_set_retrieval_scores_dplyr(
        gold, pred, mode = "doc-avg", doc_strata = "false_col"
      ),
      regexp = "doc_strata %in% colnames\\(gold_standard\\) is not TRUE"
    )
    expect_error(
      compute_set_retrieval_scores(
        gold, pred, mode = "doc-avg", doc_strata = "false_col"
      ),
      regexp = "doc_strata %in% colnames\\(gold_standard\\) is not TRUE"
    )
  }

  res <- compute_set_retrieval_scores_dplyr(
    gold, pred, mode = "doc-avg", doc_strata = "hsg"
  )
  res_collapse <- compute_set_retrieval_scores(
    gold, pred, mode = "doc-avg", doc_strata = "hsg"
  )

  res_001 <- compute_set_retrieval_scores_dplyr(
    gold_001, pred_001, mode = "doc-avg"
  )
  res_002 <- compute_set_retrieval_scores_dplyr(
    gold_002, pred_002, mode = "doc-avg"
  )

  expected_res <- dplyr::bind_rows(
    dplyr::mutate(res_001, hsg = "001", .before = "metric"),
    dplyr::mutate(res_002, hsg = "002", .before = "metric")
  )
  # sort as in function
  expected_res <- dplyr::arrange(expected_res, .data$metric, .data$hsg)

  expect_equal(res, expected_res)
  expect_equal(res_collapse, expected_res)
  expect_equal(res, res_collapse)

  ##############################################################################
  # test bootstrap#######################
  # if doc_strata is not a factor, it may loose instances during bootstrap
  expect_warning(
    object = compute_set_retrieval_scores_dplyr(
      gold, pred,
      mode = "doc-avg",
      doc_strata = "hsg",
      compute_bootstrap_ci = TRUE,
      n_bt = 10L, seed = 134
    ),
    regexp = "hsg is not a factor variable. Some levels may be lost in bootstrap replications" # nolint
  )

  gold_w_factor <- dplyr::mutate(gold, hsg = as.factor(hsg))
  compare <- create_comparison(gold_standard = gold_w_factor,
                               predicted = pred,
                               doc_strata = "hsg")
  boot_res <- generate_replicate_results_dplyr(
    base_compare = compare,
    n_bt = 10L,
    seed = 134,
    grouping_var = rlang::syms(c("hsg"))
  )
  expect_equal(nrow(boot_res), 88L,
               info = "10-fold boot-strap of 4 metrics across two test-strata
               should result in 2x10x4 = 80 rows plus 2x4 originals")


  ##############################################################################
  # test general radio silence across all input-configurations
  ##############################################################################
  set.seed(10)
  configuration <- expand.grid(
    mode = c("doc-avg", "subj-avg", "micro"),
    compute_bootstrap_ci = c(TRUE, FALSE)
  )
  res_across_config <- purrr::map2(
    configuration$mode, configuration$compute_bootstrap_ci,
    .f = ~ expect_silent(
      object = compute_set_retrieval_scores_dplyr(
        gold_w_factor,
        pred,
        mode = .x,
        doc_strata = "hsg",
        compute_bootstrap_ci = .y,
        seed = 10
      )
    )
  )

  res_across_config_collapse <- purrr::map2(
    configuration$mode, configuration$compute_bootstrap_ci,
    .f = ~ expect_silent(
      object = compute_set_retrieval_scores(
        gold_w_factor,
        pred,
        mode = .x,
        doc_strata = "hsg",
        seed = 10,
        compute_bootstrap_ci = .y
      )
    )
  )

  expect_equal(
    res_across_config,
    res_across_config_collapse
  )

  detach("package:purrr")
})

test_that("grouping (label-strata) of set retrieval computation works", {
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

  label_dict <- tibble::tribble(
    ~label_id, ~gnd_entity,
    "a", "pers",
    "b", "pers",
    "c", "subjh",
    "d", "subjh",
    "e", "conf",
    "f", "conf",
    "g", "subjh"
  )


  # expect no error when adding strata
  expect_silent(
    casimir:::create_comparison(gold, pred, label_dict = label_dict)
  )

  # check if invalid colname in label dictionary is rejected
  expect_error(
    compute_set_retrieval_scores_dplyr(
      gold, pred, mode = "doc-avg",
      label_dict = dplyr::rename(label_dict, gnd_idn = label_id)
    ),
    regexp = "\"label_id\" %in% colnames\\(label_dict\\) is not TRUE"
  )
  expect_error(
    compute_set_retrieval_scores(
      gold, pred, mode = "doc-avg",
      label_dict = dplyr::rename(label_dict, gnd_idn = label_id)
    ),
    regexp = "\"label_id\" %in% colnames\\(label_dict\\) is not TRUE"
  )

  #############################################
  # check intermediate results level
  ####################################################
  comparison_w_label_groups <- casimir:::create_comparison(
    gold, pred, label_dict = label_dict
  )
  interm_res_grpd_act <- dplyr::select(
    compute_intermediate_results_dplyr(
      comparison_w_label_groups, rlang::syms(c("gnd_entity"))
    ),
    "gnd_entity", "tp", "fn", "fp"
  )
  interm_res_grpd_act_collapse <- dplyr::select(
    compute_intermediate_results(
      comparison_w_label_groups,
      c("gnd_entity")
    )$results_table,
    "gnd_entity", "tp", "fn", "fp"
  )
  interm_res_exp <- tibble::tribble(
    ~gnd_entity, ~tp, ~fn, ~fp,
    "conf", 1L, 2L, 2L,
    "pers", 3L, 3L, 0L,
    "subjh", 1L, 3L, 2L
  )

  interm_res_exp_grpd <- dplyr::group_by(interm_res_exp, .data$gnd_entity)

  expect_equal(interm_res_grpd_act, interm_res_exp_grpd)
  expect_equal(interm_res_grpd_act_collapse, interm_res_exp)


  #############################################
  # check across aggregation modes
  ####################################################
  expected_res <- list()
  expected_res[["micro"]] <- tibble::tribble(
    ~gnd_entity, ~metric, ~mode, ~value, ~support,
    "conf", "f1", "micro", 2 * 1 / (2 * 1 + 2 + 2), 3,
    "pers", "f1", "micro", 2 * 3 / (2 * 3 + 3 + 0), 4.5,
    "subjh", "f1", "micro", 2 * 1 / (2 * 1 + 3 + 2), 3.5,
    "conf", "prec", "micro", 1 / 3, 3,
    "pers", "prec", "micro", 3 / 3, 3,
    "subjh", "prec", "micro", 1 / 3, 3,
    "conf", "rec", "micro", 1 / 3, 3,
    "pers", "rec", "micro", 3 / 6, 6,
    "subjh", "rec", "micro", 1 / 4, 4,
    "conf", "rprec", "micro", 1 / 3, 3,
    "pers", "rprec", "micro", 3 / 3, 3,
    "subjh", "rprec", "micro", 1 / 3, 3
  )

  expected_res[["subj-avg"]] <- tibble::tribble(
    ~gnd_entity, ~metric, ~mode, ~value, ~support,
    "conf", "f1", "subj-avg", (0 + 1 / 2) / 2, 2,
    "pers", "f1", "subj-avg", (0 + 6 / 7) / 2, 2,
    "subjh", "f1", "subj-avg", (0 + 0 + 2 / 3) / 3, 3,
    "conf", "prec", "subj-avg", (0 + 1 / 2) / 2, 2,
    "pers", "prec", "subj-avg", 1 / 1, 1,
    "subjh", "prec", "subj-avg", 1 / 3, 3,
    "conf", "rec", "subj-avg", (0 + 1 / 2) / 2, 2,
    "pers", "rec", "subj-avg", (0 + 3 / 4) / 2, 2,
    "subjh", "rec", "subj-avg", (0 + 1 / 2) / 2, 2,
    "conf", "rprec", "subj-avg", (0 + 1 / 2) / 2, 2,
    "pers", "rprec", "subj-avg", 1 / 1, 1,
    "subjh", "rprec", "subj-avg", 1 / 2, 2
  )

  expected_res[["doc-avg"]] <- tibble::tribble(
    ~gnd_entity, ~metric, ~mode, ~value, ~support,
    "conf", "f1", "doc-avg", (0 + 0 + 0 + 1) / 4, 4,
    "pers", "f1", "doc-avg", (0 + 1 + 1 + 2 / 3) / 4, 4,
    "subjh", "f1", "doc-avg", 1 / 4, 4,
    "conf", "prec", "doc-avg", 1 / 3, 3,
    "pers", "prec", "doc-avg", 3 / 3, 3,
    "subjh", "prec", "doc-avg", 1 / 3, 3,
    "conf", "rec", "doc-avg", 1 / 2, 2,
    "pers", "rec", "doc-avg", (1 + 1 + 0 + 1 / 2) / 4, 4,
    "subjh", "rec", "doc-avg", 1 / 4, 4,
    "conf", "rprec", "doc-avg", 1 / 1, 1,
    "pers", "rprec", "doc-avg", 3 / 3, 3,
    "subjh", "rprec", "doc-avg", 1 / 3, 3
  )

  # check that results are correct across all three aggregation modes
  res_dplyr <- purrr::imap(
    .x = expected_res,
    .f = ~expect_equal(
      object = compute_set_retrieval_scores_dplyr(
        gold, pred,
        mode = .y,
        label_dict = label_dict
      ),
      expected = .x
    )
  )
  res_collapse <- purrr::imap(
    .x = purrr::map(expected_res, .f = dplyr::ungroup),
    .f = ~expect_equal(
      object = compute_set_retrieval_scores(
        gold, pred,
        mode = .y,
        label_dict = label_dict
      ),
      expected = .x
    )
  )

  # check consistency of the tow methods
  expect_equal(
    res_dplyr,
    res_collapse
  )

  ##############################################################################
  # test bootstrap#######################
  # if label_strata is not a factor, it may loose instances during bootstrap
  expect_warning(
    object = compute_set_retrieval_scores_dplyr(
      gold, pred,
      mode = "doc-avg",
      label_dict = label_dict,
      compute_bootstrap_ci = TRUE, n_bt = 10L
    ),
    regexp = "gnd_entity is not a factor variable. Some levels may be lost in bootstrap replications" # nolint
  )

  label_dict_w_factor <- dplyr::mutate(
    label_dict, gnd_entity = as.factor(gnd_entity)
  )
  compare <- create_comparison(gold, pred, label_dict = label_dict_w_factor)
  boot_res <- generate_replicate_results_dplyr(
    base_compare = compare,
    n_bt = 10L,
    grouping_var = rlang::syms(c("doc_id", "gnd_entity"))
  )

  expect_equal(nrow(boot_res), 132L,
               info = "10-fold boot-strap of 4 metrics across three test-strata
               should result in 3x10x4 = 120 rows plus 3*4 originals")


  ##############################################################################
  # test general radio silence across all input-configurations
  ##############################################################################
  set.seed(10)
  configuration <- expand.grid(
    .mode = c("doc-avg", "subj-avg", "micro"),
    .compute_bootstrap_ci = c(TRUE, FALSE),
    ..progress = c(TRUE, FALSE)
  )
  messages <- purrr::pmap(
    configuration,
    .f = function(.mode, .compute_bootstrap_ci, ..progress) { # nolint
      expect_silent(
        object = compute_set_retrieval_scores_dplyr(
          gold,
          pred,
          mode = .mode,
          label_dict = label_dict_w_factor,
          compute_bootstrap_ci = .compute_bootstrap_ci,
          seed = 10,
          .progress = ..progress
        )
      )
    }
  )
  messages_collapse <- purrr::pmap(
    configuration,
    .f = function(.mode, .compute_bootstrap_ci, ..progress) { # nolint
      expect_silent(
        object = compute_set_retrieval_scores(
          gold,
          pred,
          mode = .mode,
          label_dict = label_dict_w_factor,
          compute_bootstrap_ci = .compute_bootstrap_ci,
          seed = 10,
          .progress = ..progress
        )
      )
    }
  )

  # check that both methods are consistents
  expect_equal(
    messages,
    messages_collapse
  )

  detach("package:purrr")
})
