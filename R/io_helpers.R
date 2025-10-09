#' determine the appropriate grouping variables for each aggregation mode
#'
#' @inheritParams compute_set_retrieval_scores
#' @param var additional variables to include
#'
#' @return a character vector of variables that determine the grouping structure
set_grouping_var <- function(mode, doc_groups, label_groups, var = NULL) {
  if (!is.null(doc_groups)) {
    stopifnot("doc_id" %in% colnames(doc_groups))
    doc_strata <- setdiff(colnames(doc_groups), "doc_id")
  } else {
    doc_strata <- NULL
  }

  if (!is.null(label_groups)) {
    stopifnot("label_id" %in% colnames(label_groups))
    label_strata <- setdiff(colnames(label_groups), "label_id")
  } else {
    label_strata <- NULL
  }

  stopifnot(mode %in% c("doc-avg", "subj-avg", "micro"))
  if (mode == "doc-avg") {
    grouping_var <- c("doc_id", doc_strata, label_strata, var)
  }

  if (mode == "subj-avg") {
    grouping_var <- c("label_id", doc_strata, label_strata, var)
  }

  if (mode == "micro") {
    grouping_var <- c("doc_id", "label_id", doc_strata, label_strata, var)
  }

  grouping_var
}

#' Process input for cost_fp
#'
#' @inheritParams compute_set_retrieval_scores
#' @inheritParams compute_intermediate_results
#'
#' @return a numeric value > 0
process_cost_fp <- function(cost_fp_constant, gold_vs_pred) {
  label_stats <- gold_vs_pred |>
    collapse::fsubset(gold == TRUE) |>
    collapse::fsummarise(
      max = max(label_weight),
      min = min(label_weight),
      mean = mean(label_weight)
    )

  if (is.numeric(cost_fp_constant) && cost_fp_constant > 0) {
    cost_fp_processed <- cost_fp_constant
  } else if (cost_fp_constant %in% c("max", "min", "mean")) {
    cost_fp_processed <- switch(cost_fp_constant,
      max = label_stats$max,
      min = label_stats$min,
      mean = label_stats$mean
    )
  } else {
    stop("cost_fp_constant must be a numeric value > 0 or one of
           'max', 'min', 'mean'; not cost_fp_constant = ", cost_fp_constant)
  }
  cost_fp_processed
}

#' Generate flags, if propensity scores should be applied to intermediate
#' results or summarise results
#'
#' @inheritParams compute_set_retrieval_scores
#'
#' @returns list containing logical flags `intermed` and `summarise`
set_ps_flags <- function(mode, propensity_scored) {
  stopifnot(mode %in% c("micro", "subj-avg", "doc-avg"))
  stopifnot(is.logical(propensity_scored))
  # set flag, if propensity scores should be applied to intermediate results
  # (not wanted for mode subj-avg)
  if (mode == "subj-avg") {
    intermed <- FALSE
  } else {
    intermed <- propensity_scored
  }

  # set flag, if propensity scores should be applied to summarise results stage
  # (only wanted for mode subj-avg)
  if (mode == "subj-avg") {
    summarise <- propensity_scored
  } else {
    summarise <- FALSE
  }

  list("intermed" = intermed, "summarise" = summarise)
}
