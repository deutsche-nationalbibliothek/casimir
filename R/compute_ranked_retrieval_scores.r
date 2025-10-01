#' Compute ranked retrieval scores
#'
#' Rankied retrieval, unlike set retrieval, assumes ordered predictions. This
#' function computes dcg, ndcg, idcg and lrap
#'
#' Unlike set retrieval metrics, ranked retrieval metrics are logically bound to
#' a document wise evaluation. Thus, there is only the aggregation mode
#' "doc-avg" for these scores available.
#'
#' @inheritParams compute_set_retrieval_scores
#' @param predicted multi-label prediction results. expects \code{data.frame}
#'   with cols \emph{"label_id", "doc_id", "score"}
#'
#' @return a \code{data.frame} with cols
#'    \emph{"metric", "mode", "value", "support"}
#'    and optionally grouping
#'    variables supplied in doc_strata. Here, \strong{support}
#'    is defined number of documents that contribute to the document average
#'    in aggregation of the overall result.
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
#'   "A", "f",	0.3277,
#'   "A", "e",	0.32172,
#'   "A", "b",	0.13517,
#'   "A", "g",	0.10134,
#'   "A", "h",	0.09152,
#'   "A", "a",	0.07483,
#'   "A", "i",	0.03649,
#'   "A", "j",	0.03551,
#'   "A", "k",	0.03397,
#'   "A", "c",	0.03364
#' )
#'
#' results <- compute_ranked_retrieval_scores(
#'     gold,
#'     pred,
#'     compute_bootstrap_ci = FALSE
#' )
#'
compute_ranked_retrieval_scores <- function( # nolint
  gold_standard,
  predicted,
  doc_strata = NULL,
  compute_bootstrap_ci = FALSE,
  n_bt = 10L,
  seed = NULL,
  progress = options::opt("progress")
) {

  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id", "score") %in% colnames(predicted)))
  stopifnot(is.logical(compute_bootstrap_ci))
  if (!is.null(doc_strata)) {
    stopifnot(doc_strata %in% colnames(gold_standard))
  }

  gold_vs_pred <- create_comparison(gold_standard, predicted,
                                    doc_strata = doc_strata)

  grouping_var <- rlang::syms(c("doc_id", doc_strata))

  iterm <- compute_intermediate_results_rr(
    gold_vs_pred, grouping_var
  )

  iterm |>
    dplyr::group_by(!!!rlang::syms(c(doc_strata))) |>
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
