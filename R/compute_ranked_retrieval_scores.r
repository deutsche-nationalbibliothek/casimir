#' Compute ranked retrieval scores
#'
#' This function computes the ranked retrieval scores Discounted
#' Cumulative Gain (DCG), Ideal Discounted Cumulative Gain (IDCG), Normalised
#' Discounted Cumulative Gain (NDCG) and Label Ranking Average Precision (LRAP).
#' Ranked retrieval, unlike set retrieval, assumes ordered predictions. Unlike
#' set retrieval metrics, ranked retrieval metrics are logically bound to a
#' document-wise evaluation. Thus, only the aggregation mode "doc-avg" is
#' available for these scores.
#'
#' @inheritParams compute_set_retrieval_scores
#' @param predicted Multi-label prediction results. Expects a data.frame with
#'   columns \code{"label_id", "doc_id", "score"}.
#'
#' @return A data.frame with columns \code{"metric", "mode", "value", "support"}
#'   and optional grouping variables supplied in \code{doc_groups}. Here,
#'   \code{support} is defined as number of documents that contribute to the
#'   document average in aggregation of the overall result.
#' @export
#'
#' @examples
#' # some dummy results
#' gold <- tibble::tribble(
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "b",
#'   "A", "c",
#'   "A", "d",
#'   "A", "e",
#' )
#'
#' pred <- tibble::tribble(
#'   ~doc_id, ~label_id, ~score,
#'   "A", "f", 0.3277,
#'   "A", "e", 0.32172,
#'   "A", "b", 0.13517,
#'   "A", "g", 0.10134,
#'   "A", "h", 0.09152,
#'   "A", "a", 0.07483,
#'   "A", "i", 0.03649,
#'   "A", "j", 0.03551,
#'   "A", "k", 0.03397,
#'   "A", "c", 0.03364
#' )
#'
#' results <- compute_ranked_retrieval_scores(
#'   pred,
#'   gold
#' )
#'
compute_ranked_retrieval_scores <- function( # nolint styler: off
    predicted,
    gold_standard,
    doc_groups = NULL,
    drop_empty_groups = options::opt("drop_empty_groups"),
    progress = options::opt("progress")) {
  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id", "score") %in% colnames(predicted)))

  gold_vs_pred <- create_comparison(
    predicted = predicted,
    gold_standard = gold_standard,
    doc_groups = doc_groups
  )

  grouping_var <- rlang::syms(colnames(doc_groups))
  doc_strata <- setdiff(colnames(doc_groups), "doc_id")

  intermed <- compute_intermediate_results_rr(
    # note: usually adding drop_empty_groups = TRUE here is wrong
    # e.g. this would show empty subject groups for all documents
    gold_vs_pred, grouping_var,
    drop_empty_groups = TRUE
  )

  intermed |>
    dplyr::group_by(!!!rlang::syms(c(doc_strata)), .drop = drop_empty_groups) |>
    dplyr::summarise(
      support = dplyr::n(),
      mode = "doc-avg",
      across(.cols = c(dcg, ndcg, lrap), .fns = mean)
    ) |>
    tidyr::pivot_longer(
      cols = c(dcg, ndcg, lrap),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::select(
      dplyr::all_of(doc_strata),
      "metric", "mode", "value", "support"
    )
}
