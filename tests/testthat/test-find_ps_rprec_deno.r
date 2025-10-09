test_that("ps_rprec_deno is computed correctly", {
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
    dplyr::summarise(mlw = mean(label_weight)) |>
    dplyr::pull(mlw)

  comp <- comp |>
    collapse::ftransform(
      n_gold = gold,
      n_suggested = suggested,
      tp = (gold & suggested) * label_weight,
      fp = (!gold & suggested) * mlw,
      fn = gold & !suggested * label_weight,
      relevance = 0
    )


  docwise_res_collapse <- find_ps_rprec_deno(
    gold_vs_pred = comp,
    grouping_var = c("doc_id"),
    cost_fp = mlw
  ) |>
    dplyr::ungroup()


  docwise_res <- find_ps_rprec_deno_dplyr(
    gold_vs_pred = comp,
    grouping_var = rlang::syms(c("doc_id")),
    cost_fp = mlw
  ) |>
    dplyr::ungroup()

  expected_res <- list()
  # correct denominator for doc-avg propensity scored rprec computation
  # A: 3 gold, 3 suggested, cumsum of propensity scores of
  # first 3 gold labels: 4.462219
  # B: 2 gold, 4 suggested, cumsum of propensity scores of
  # first two gold labels: 10.306136
  # C: 4 gold, 1 suggested, cumsum of propensity scores
  # first gold label: 9.220291
  expected_res[["docwise"]] <- tibble::tribble(
    ~doc_id, ~n_gold, ~n_suggested, ~tp, ~fp, ~fn, ~rprec_deno,
    "A", 3, 3, 1.085846, 2 * mlw, 1.304366 + 2.072008, 2.072008 + 1.304366 + 1.085846, # nolint
    "B", 2, 4, 1.085846, 3 * mlw, 9.220291, 9.220291 + 1.085846,
    "C", 4, 1, 7.831511, 0, 9.220291 + 1.304366 + 1.085846, 9.220291
  ) |>
    dplyr::mutate(
      delta_relevance = 0,
      .after = fn
    )

  expected_res[["labelwise"]] <- tibble::tribble(
    ~label_id, ~n_gold, ~n_suggested, ~tp, ~fp, ~fn, ~rprec_deno,
    # nolint start
    "a", 3, 2, 2 * 1.085846, 0, 1.085846, 2 * 1.085846,
    "b", 2, 0, 0, 0, 2 * 1.304366, 0,
    "c", 1, 1, 0, mlw, 2.072009, 2.072009,
    "d", 2, 1, 0, mlw, 2 * 9.220291, 9.220291,
    "e", 0, 1, 0, mlw, 0, 0,
    "f", 1, 3, 7.831511, 2 * mlw, 0, 7.831511
    # nolint end
  ) |>
    dplyr::mutate(
      delta_relevance = 0,
      .after = fn
    )

  expected_res[["casewise"]] <- tibble::tribble(
    ~doc_id, ~label_id, ~n_gold, ~n_suggested, ~tp, ~fp, ~fn, ~rprec_deno,
    "A", "a", 1, 1, 1.085846, 0, 0, 1.085846,
    "A", "b", 1, 0, 0, 0, 1.304366, 0,
    "A", "c", 1, 0, 0, 0, 2.072008, 0,
    "B", "a", 1, 1, 1.085846, 0, 0, 1.085846,
    "B", "d", 1, 0, 0, 0, 9.220291, 0,
    "C", "a", 1, 0, 0, 0, 1.085846, 0,
    "C", "b", 1, 0, 0, 0, 1.304366, 0,
    "C", "d", 1, 0, 0, 0, 9.220291, 0,
    "C", "f", 1, 1, 7.831511, 0, 0, 7.831511,
    "A", "d", 0, 1, 0, mlw, 0, 0,
    "A", "f", 0, 1, 0, mlw, 0, 0,
    "B", "e", 0, 1, 0, mlw, 0, 0,
    "B", "f", 0, 1, 0, mlw, 0, 0,
    "B", "c", 0, 1, 0, mlw, 0, 0
  ) |>
    dplyr::arrange(doc_id, label_id) |>
    dplyr::mutate(
      delta_relevance = 0,
      .after = fn
    )

  res_dplyr <- purrr::map2(
    .x = expected_res,
    .y = list(c("doc_id"), c("label_id"), c("doc_id", "label_id")),
    .f = ~ expect_equal(
      object = find_ps_rprec_deno_dplyr(
        gold_vs_pred = comp,
        grouping_var = rlang::syms(c(.y)),
        cost_fp = mlw
      ) |>
        dplyr::ungroup(),
      expected = .x,
      tolerance = 1e-5
    )
  )

  convert_grp_names <- function(expected_res) {
    if (all(c("doc_id", "label_id") %in% colnames(expected_res))) {
      res <- expected_res |>
        dplyr::mutate(
          grp_names = paste0(doc_id, ".", label_id),
          .before = n_gold
        ) |>
        dplyr::select(-doc_id, -label_id)
    } else if ("doc_id" %in% colnames(expected_res)) {
      res <- expected_res |>
        dplyr::mutate(
          grp_names = doc_id,
          .before = n_gold
        ) |>
        dplyr::select(-doc_id)
    } else if ("label_id" %in% colnames(expected_res)) {
      res <- expected_res |>
        dplyr::mutate(
          grp_names = label_id,
          .before = n_gold
        ) |>
        dplyr::select(-label_id)
    } else {
      res <- expected_res
    }

    res
  }

  res_collapse <- purrr::map2(
    .x = expected_res,
    .y = list(c("doc_id"), c("label_id"), c("doc_id", "label_id")),
    .f = ~ expect_equal(
      object = find_ps_rprec_deno(
        gold_vs_pred = comp,
        grouping_var = c(.y),
        cost_fp = mlw
      ) |>
        dplyr::ungroup(),
      expected = convert_grp_names(.x),
      tolerance = 1e-5
    )
  )
})
