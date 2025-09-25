#' determine the appropriate grouping variables for each aggregation mode
#'
#' @param mode aggregation mode for later computation. One of c("doc-avg", "subj-avg", "micro")
#' @param doc_strata as in compute_set_retrieval_scores
#' @param label_dict as in compute_set_retrieval_scores
#' @param var additional variables to include
#'
#' @return a character vector of variables that determine the grouping structure
set_grouping_var <- function(mode, doc_strata, label_dict, var = NULL) {
  if (!is.null(label_dict)) {
    stopifnot("label_id" %in% colnames(label_dict))
    label_strata <- setdiff(colnames(label_dict), "label_id")
  } else {
    label_strata <- NULL
  }

  stopifnot(mode %in% c("doc-avg", "subj-avg", "micro"))
  if (mode == "doc-avg")
    grouping_var <- c("doc_id", doc_strata, label_strata, var)

  if (mode == "subj-avg")
    grouping_var <- c("label_id", doc_strata, label_strata, var)

  if (mode == "micro")
    grouping_var <- c("doc_id", "label_id", doc_strata, label_strata, var)

  grouping_var
}

#' Process input for cost_fp
#'
#' @param cost_fp Constant cost assigned to false positives. cost_fp must be
#'  a numeric value > 0 or one of 'max', 'min', 'mean' (computed with reference
#'   to the \code{label_weights})
#' @param gold_vs_pred expects \code{data.frame} with cols
#'   \emph{"gold", "label_weight"}.
#'
#' @return a numeric value > 0
process_cost_fp <- function(cost_fp, gold_vs_pred) {

  label_stats <- gold_vs_pred |>
    collapse::fsubset(gold == TRUE) |>
    collapse::fsummarise(
      max = max(label_weight),
      min = min(label_weight),
      mean = mean(label_weight)
    )

  if (is.numeric(cost_fp) && cost_fp > 0) {
    cost_fp_processed <- cost_fp
  } else if (cost_fp %in% c("max", "min", "mean")) {
    cost_fp_processed <- switch(
      cost_fp,
      max = label_stats$max,
      min = label_stats$min,
      mean = label_stats$mean
    )
  } else {
    stop("cost_fp must be a numeric value > 0 or one of
           'max', 'min', 'mean'; not cost_fp = ", cost_fp)
  }
  return(cost_fp_processed)
}

#' Generate flags, if propensity scores should be applied to intermediate
#' results or summarise results
#'
#' @param mode aggregation mode: \emph{"doc-avg", "subj-avg", "micro"}
#' @param propensity_scored logical, whether to use propensity scores as weights
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

  return(list("intermed" = intermed, "summarise" = summarise))
}
