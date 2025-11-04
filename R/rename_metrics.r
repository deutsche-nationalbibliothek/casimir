#' Rename metrics
#'
#' Rename metric names for generalised precision etc. The output will be renamed
#' if:
#'   \describe{
#'     \item{\code{graded_relevance == TRUE}}{prefixed with \emph{"g-"} to
#'       indicate that metrics are computed with graded relevance.}
#'     \item{\code{propensity_scored == TRUE}}{prefixed with \emph{"ps-"} to
#'       indicate that metrics are computed with propensity scores.}
#'     \item{\code{!is.null(k)}}{suffixed with \emph{"@@k"} to indicate
#'       that metrics are limited to top k predictions.}
#'    }
#'
#' @param res_df A data.frame with a column \code{"metric"} containing metric
#'   names \code{"f1", "prec", "rec", "rprec"}.
#' @inheritParams compute_set_retrieval_scores
#'
#' @return The input data.frame \code{res_df} with renamed metrics for
#'   generalised precision etc.
rename_metrics <- function(
    res_df,
    k = NULL,
    propensity_scored = FALSE,
    graded_relevance = FALSE) {
  if ("metric" %in% colnames(res_df)) {
    # rename the content of the metric column
    var <- "metric"
    if (graded_relevance) {
      res_df[[var]] <- paste0("g-", res_df[[var]])
    }
    if (!is.null(k)) {
      res_df[[var]] <- paste0(res_df[[var]], "@", k)
    }
    if (propensity_scored) {
      res_df[[var]] <- paste0("ps-", res_df[[var]])
    }
  } else if ("pr_auc" %in% colnames(res_df)) {
    # rename the pr_auc column
    new_name <- "pr_auc"
    if (graded_relevance) {
      new_name <- paste0("g-", new_name)
    }
    if (!is.null(k)) {
      new_name <- paste0(new_name, "@", k)
    }
    if (propensity_scored) {
      new_name <- paste0("ps-", new_name)
    }
    colnames(res_df)[colnames(res_df) == "pr_auc"] <- new_name
  } else {
    stop("`res_df` must contain either a `metric` or `pr_auc` column.")
  }

  res_df
}
