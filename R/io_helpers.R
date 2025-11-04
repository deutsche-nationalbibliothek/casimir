#' Set grouping variables
#'
#' Determine the appropriate grouping variables for each aggregation mode.
#'
#' @inheritParams compute_set_retrieval_scores
#' @param var Additional variables to include.
#'
#' @return A character vector of variables determining the grouping structure.
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

#' Process cost for false positives
#'
#' Calculate the cost for false positives depending on the chosen
#' \code{cost_fp_constant}.
#'
#' @inheritParams compute_set_retrieval_scores
#' @inheritParams compute_intermediate_results
#'
#' @return A numeric value > 0.
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
    stop("`cost_fp_constant` must be a numeric value > 0 or one of \n",
         "'max', 'min', 'mean'; not `cost_fp_constant == \"", cost_fp_constant, "\"`.")
  }
  cost_fp_processed
}

#' Set flags for propensity scores
#'
#' Generate flags if propensity scores should be applied to intermediate results
#' or summarised results.
#'
#' @inheritParams compute_set_retrieval_scores
#'
#' @returns A list containing logical flags \code{"intermed"} and
#'   \code{"summarise"}.
set_ps_flags <- function(mode, propensity_scored) {
  stopifnot(mode %in% c("micro", "subj-avg", "doc-avg"))
  stopifnot(is.logical(propensity_scored))
  # set flag if propensity scores should be applied to intermediate results
  # (not wanted for mode subj-avg)
  if (mode == "subj-avg") {
    intermed <- FALSE
  } else {
    intermed <- propensity_scored
  }

  # set flag if propensity scores should be applied to summarise results stage
  # (only wanted for mode subj-avg)
  if (mode == "subj-avg") {
    summarise <- propensity_scored
  } else {
    summarise <- FALSE
  }

  list("intermed" = intermed, "summarise" = summarise)
}

#' Coerce column to character
#'
#' Check an arbitrary column in a data.frame for factor type and coerce to
#' character.
#'
#' @param df An input data.frame.
#' @param col The name of the column to check.
#'
#' @returns The input data.frame \code{df} with the specified column being no
#'   longer a factor variable.
check_id_vars_col <- function(df, col) {
  if (col %in% colnames(df) && is.factor(df[[col]])) {
    warning("`", col, "` should never be factor. Coercing to character.")
    df[[col]] <- as.character(df[[col]])
  }
  df
}

#' Coerce id columns to character
#'
#' Internal helper function designed to ensure that id columns are not passed as
#' factor variables. Factor variables in id columns may cause undesired
#' behaviour with the \code{drop_empty_group} argument.
#'
#' @param df An input data.frame.
#'
#' @returns The input data.frame \code{df} with the id columns being no
#'   longer factor variables.
check_id_vars <- function(df) {
  df <- check_id_vars_col(df, "doc_id")
  df <- check_id_vars_col(df, "label_id")
  df
}
