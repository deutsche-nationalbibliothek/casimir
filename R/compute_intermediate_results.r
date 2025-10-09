#' Compute true positives, false positives on aggregation level specified
#'
#' @param gold_vs_pred \code{data.frame} with logical columns
#'   \emph{"suggested", "gold"} as produced by \code{create_comparison}
#' @param grouping_var a character vector of variables that must be present in
#'  gold_vs_pred (dplyr version requires rlang symbols)
#' @inheritParams compute_set_retrieval_scores
#' @param cost_fp numeric > 0, default is NULL
#' @inheritParams option_params
#'
#' @return data.frame with cols "n_gold", "n_suggested", "tp", "fp", "fn",
#'  "prec", "rec", "f1"
#'
#' @export
#'
#' @examples
#'
#' gold <- tibble::tribble(
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "b",
#'   "A", "c",
#'   "B", "a",
#'   "B", "d",
#'   "C", "a",
#'   "C", "b",
#'   "C", "d",
#'   "C", "f"
#' )
#'
#' pred <- tibble::tribble(
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "d",
#'   "A", "f",
#'   "B", "a",
#'   "B", "e",
#'   "C", "f"
#' )
#'
#' gold_vs_pred <- create_comparison(gold, pred)
#' casimir:::compute_intermediate_results(gold_vs_pred, "doc_id")
#'
compute_intermediate_results <- function(
    gold_vs_pred,
    grouping_var,
    propensity_scored = FALSE,
    cost_fp = NULL,
    drop_empty_groups = options::opt("drop_empty_groups"),
    check_group_names = options::opt("check_group_names")) {
  stopifnot(all(c("suggested", "gold") %in% colnames(gold_vs_pred)))

  stopifnot(!is.null(grouping_var))
  # check that no levels of the grouping variables contain dots
  if (check_group_names) {
    old_grouping_columns <- collapse::fselect(gold_vs_pred, grouping_var)
    seperator <- "__SEP__4928873"
    for (var in grouping_var) {
      gold_vs_pred[[var]] <- stringr::str_replace_all(
        gold_vs_pred[[var]], "\\.", seperator
      )
    }
  }

  g <- collapse::GRP(gold_vs_pred, grouping_var)

  if (!("relevance") %in% colnames(gold_vs_pred)) {
    gold_vs_pred <- dplyr::mutate(
      gold_vs_pred,
      relevance = 0.0
    )
  }

  if (propensity_scored) {
    stopifnot("label_weight" %in% colnames(gold_vs_pred))

    gold_vs_pred_smry <- find_ps_rprec_deno(gold_vs_pred, grouping_var, cost_fp)
  } else {
    rowwise_trans <- collapse::fcompute(
      .data = gold_vs_pred,
      n_gold = gold,
      n_suggested = suggested,
      tp = gold & suggested,
      fp = !gold & suggested,
      fn = gold & !suggested,
      delta_relevance = (!gold & suggested) * relevance
    )
    gold_vs_pred_smry <- collapse::fsum(rowwise_trans, g)

    gold_vs_pred_smry <- collapse::ftransform(
      .data = gold_vs_pred_smry,
      rprec_deno = pmin(n_gold + delta_relevance, n_suggested),
      grp_names = row.names(gold_vs_pred_smry)
    )
  }

  gold_vs_pred_smry <- collapse::ftransform(
    .data = gold_vs_pred_smry,
    prec = ifelse(n_suggested == 0,
      NA_real_,
      (tp + delta_relevance) / (tp + fp)
    ),
    # compute rprecision as in Manning etal.
    rprec = ifelse(pmin(n_gold, n_suggested) == 0,
      NA_real_,
      (tp + delta_relevance) / rprec_deno
    ),
    rec = ifelse(n_gold == 0,
      NA_real_,
      (tp + delta_relevance) / (tp + fn + delta_relevance)
    ),
    # NA-Handling for F1:
    # return NA if both prec and rec are NA
    # return 0 if only one of them is NA
    f1 = ifelse(
      n_gold + n_suggested == 0,
      NA_real_,
      2 * (tp + delta_relevance) / (2 * tp + fp + fn + delta_relevance)
    )
  )

  gold_vs_pred_smry <- tidyr::separate(
    data = gold_vs_pred_smry,
    col = grp_names,
    into = grouping_var,
    sep = "\\.",
    remove = TRUE
  )

  # restore the factor structure of the original grouping_var
  restore_factor_levels <- function(df, source_df, var, drop) {
    if (is.factor(source_df[[var]])) {
      df[[var]] <- factor(
        x = df[[var]],
        levels = levels(source_df[[var]])
      )
      if (!drop) {
        df <- tidyr::complete(df, !!!rlang::syms(var))
      }
    }
    df
  }

  for (var in grouping_var) {
    if (check_group_names) {
      gold_vs_pred_smry[[var]] <- stringr::str_replace_all(
        gold_vs_pred_smry[[var]], seperator, "\\."
      )
      gold_vs_pred_smry <- restore_factor_levels(
        gold_vs_pred_smry,
        source_df = old_grouping_columns,
        var = var,
        drop = drop_empty_groups
      )
    } else {
      gold_vs_pred_smry <- restore_factor_levels(
        gold_vs_pred_smry,
        source_df = gold_vs_pred,
        var = var,
        drop = drop_empty_groups
      )
    }
  }

  res_df <- dplyr::select(
    gold_vs_pred_smry, dplyr::all_of(grouping_var), dplyr::everything()
  )

  list(
    results_table = res_df,
    grouping_var = grouping_var
  )
}

#' @describeIn compute_intermediate_results variant with dplyr based
#' internals rather then collapse internals
compute_intermediate_results_dplyr <- function( # nolint styler: off
    gold_vs_pred,
    grouping_var,
    propensity_scored = FALSE,
    cost_fp = NULL) {
  stopifnot(all(c("suggested", "gold") %in% colnames(gold_vs_pred)))

  if (!("relevance") %in% colnames(gold_vs_pred)) {
    gold_vs_pred <- dplyr::mutate(
      gold_vs_pred,
      relevance = 0.0
    )
  }

  if (propensity_scored) {
    stopifnot("label_weight" %in% colnames(gold_vs_pred))

    gold_vs_pred_smry <- find_ps_rprec_deno_dplyr(
      gold_vs_pred,
      grouping_var,
      cost_fp
    )
  } else {
    gold_vs_pred_smry <- dplyr::summarise(
      dplyr::group_by(gold_vs_pred, !!!grouping_var),
      n_gold = sum(.data$gold),
      n_suggested = sum(.data$suggested),
      tp = sum(.data$gold & .data$suggested),
      fp = sum(!.data$gold & .data$suggested),
      fn = sum(.data$gold & !.data$suggested),
      delta_relevance = sum(.data$relevance * (!.data$gold & .data$suggested)),
      rprec_deno = dplyr::if_else(
        pmin(n_gold, n_suggested) == 0,
        0,
        pmin(n_gold + delta_relevance, n_suggested),
        NA_real_
      ),
      .groups = "keep"
    )
  }

  gold_vs_pred_smry <- dplyr::mutate(
    gold_vs_pred_smry,
    prec = ifelse(.data$n_suggested == 0,
      NA_real_,
      (.data$tp + .data$delta_relevance) / (.data$tp + .data$fp)
    ),
    rec = ifelse(.data$n_gold == 0,
      NA_real_,
      (.data$tp + .data$delta_relevance) /
        (.data$tp + .data$fn + .data$delta_relevance)
    ),
    # NA-Handling for F1:
    # return NA if both prec and rec are NA
    # return 0 if only one of them is NA
    f1 = ifelse(
      .data$n_gold + .data$n_suggested == 0,
      NA_real_,
      2 * (.data$tp + .data$delta_relevance) /
        (2 * .data$tp + .data$fp + .data$fn + .data$delta_relevance)
    )
  )

  # compute rprecision as in Manning etal.
  gold_vs_pred_smry <- dplyr::mutate(
    gold_vs_pred_smry,
    rprec = ifelse(.data$rprec_deno == 0,
      NA_real_,
      (.data$tp + .data$delta_relevance) / .data$rprec_deno
    ),
    .before = "rec"
  )

  gold_vs_pred_smry
}
