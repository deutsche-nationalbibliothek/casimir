#' Compute the denominator for rprecision, based on propensity scored
#' ranking of gold_standrad labels
#'
#' @inheritParams compute_intermediate_results
#'
#' @return data.frame with cols "n_gold", "n_suggested", "tp", "fp",
#'   "fn", "rprec_deno"
find_ps_rprec_deno <- function(gold_vs_pred, grouping_var, cost_fp) {

  stopifnot(
    all(c("label_weight") %in% colnames(gold_vs_pred))
  )

  g <- collapse::GRP(gold_vs_pred, grouping_var)

  # apply custom costs to tp, fp, fn
  if (!is.null(cost_fp)) {
    # note in this cost scheme fp gets a constant weight
    rowwise_trans <- collapse::fcompute(
      .data = gold_vs_pred,
      n_gold = gold,
      n_suggested = suggested,
      tp = (gold & suggested) * label_weight,
      fp = (!gold & suggested) * cost_fp,
      fn = (gold & !suggested) * label_weight,
      # graded results are weighted by cost_fp, as they are false positives too
      # but this is something to think about in the future
      delta_relevance = (!gold & suggested) * relevance * cost_fp
    )
  } else { # apply label weights to fp, too
    rowwise_trans <- collapse::fcompute(
      .data = gold_vs_pred,
      n_gold = gold,
      n_suggested = suggested,
      tp = (gold & suggested) * label_weight,
      fp = (!gold & suggested) * label_weight,
      fn = (gold & !suggested) * label_weight,
      delta_relevance = (!gold & suggested) * relevance * label_weight
    )
  }


  gold_vs_pred_smry <- collapse::fsum(rowwise_trans, g, use.g.names = TRUE)

  gold_vs_pred_smry <- collapse::ftransform(
    .data = gold_vs_pred_smry,
    grp_names = row.names(gold_vs_pred_smry)
  )

  prepare_cumsums <- collapse::fcompute(
    .data = gold_vs_pred,
    rank_gold = gold,
    rank_suggested = suggested,
    rprec_deno = gold * label_weight
  )

  cumsums <- collapse::fcumsum(
    prepare_cumsums, g,
    o = -gold_vs_pred$label_weight, na.rm = FALSE
  )

  group_names <- collapse::GRPnames(g) # nolint
  grp_id_col <- collapse::GRPid(g)

  cumsums <- collapse::ftransform(
    cumsums,
    grp_names = purrr::map_chr(grp_id_col, ~group_names[.x])
  )

  gold_vs_pred_smry <- collapse::join(
    gold_vs_pred_smry, cumsums, on = "grp_names",
    how = "left",
    multiple = TRUE,
    validate = "1:m",
    verbose = 0
  )

  gold_vs_pred_smry <- collapse::fsubset(
    gold_vs_pred_smry,
    rank_gold == pmin(n_suggested, n_gold)
    | pmin(n_gold, n_suggested) == 0
  )

  gold_vs_pred_smry <- collapse::ftransform(
    gold_vs_pred_smry,
    rprec_deno = ifelse(
      pmin(n_gold, n_suggested) == 0,
      0,
      rprec_deno + delta_relevance
    )
  )

  gold_vs_pred_smry <- collapse::fslice(
    gold_vs_pred_smry,
    how = "first",
    n = 1,
    cols = "grp_names"
  )

  collapse::fselect(
    gold_vs_pred_smry,
    c(
      "grp_names", "n_gold", "n_suggested", "tp",
      "fp", "fn", "delta_relevance", "rprec_deno"
    )
  )

}


#' @describeIn find_ps_rprec_deno variant with dplyr based
#' internals rather then collapse internals
find_ps_rprec_deno_dplyr <- function(gold_vs_pred, grouping_var, cost_fp) {

  stopifnot(
    all(c("label_weight") %in% colnames(gold_vs_pred))
  )

  gold_vs_pred_aggr <- gold_vs_pred |>
    dplyr::group_by(!!!grouping_var) |>
    dplyr::mutate(
      rprec_deno = dplyr::order_by(
        -.data$label_weight, cumsum(.data$label_weight * .data$gold)
      ),
      rank_gold = dplyr::order_by(
        -.data$label_weight, cumsum(.data$gold)
      ),
      n_gold = sum(.data$gold),
      n_suggested = sum(.data$suggested),
      # apply custom costs to tp, fp, fn
      # note in this cost scheme fp gets no extra weight
      tp = sum((.data$gold & .data$suggested) * .data$label_weight),
      fp = sum((!.data$gold & .data$suggested) * cost_fp),
      fn = sum((.data$gold & !.data$suggested) * .data$label_weight),
      # graded results are weighted by cost_fp, as they are false positives too
      # but this is something to think about in the future
      delta_relevance = sum(
        .data$relevance * cost_fp * (!.data$gold & .data$suggested)
      ),
      rprec_deno = dplyr::if_else(
        pmin(n_gold, n_suggested) == 0,
        0,
        rprec_deno + delta_relevance,
        NA_real_
      )
    )

  gold_vs_pred_smry <- dplyr::filter(
    gold_vs_pred_aggr,
    rank_gold == pmin(n_gold, n_suggested) | pmin(n_gold, n_suggested) == 0
  ) |>
    dplyr::slice(1) |>
    dplyr::select(
      !!!grouping_var,
      dplyr::all_of(c(
        "n_gold", "n_suggested", "tp", "fp", "fn",
        "delta_relevance", "rprec_deno"
      ))
    )

  gold_vs_pred_smry

}
