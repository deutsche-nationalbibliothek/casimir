#' Join gold standard and predicted results in one table
#'
#' @inheritParams compute_set_retrieval_scores
#'
#' @return data.frame with cols "label_id", "doc_id", "suggested", "gold"
#'
#' @export
#'
#' @examples
#' library(casimir)
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
#' casimir::create_comparison(gold, pred)
create_comparison <- function(
    gold_standard, predicted,
    doc_groups = NULL, label_groups = NULL,
    graded_relevance = FALSE,
    propensity_scored = FALSE,
    label_distribution = NULL,
    ignore_inconsistencies = options::opt("ignore_inconsistencies")) {
  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))

  predicted <- check_id_vars(predicted)
  gold_standard <- check_id_vars(gold_standard)
  if (!is.null(doc_groups)) {
    doc_groups <- check_id_vars(doc_groups)
  }
  if (!is.null(label_groups)) {
    label_groups <- check_id_vars(label_groups)
  }



  if (graded_relevance) {
    predicted <- check_repair_relevance_pred(predicted)
  }

  if (graded_relevance && propensity_scored) {
    warning(
      "Mixing graded relevance and propensity_scores is not tested. ",
      "You sure this is what you want?"
    )
  }

  if (propensity_scored) {
    if (is.null(label_distribution)) {
      stop("If propensity_scored = TRUE, label_distribution must be provided.")
    }
    stopifnot(all(
      c("label_id", "label_freq") %in% colnames(label_distribution)
    ))
  }

  if (!is.null(doc_groups)) {
    stopifnot("doc_id" %in% colnames(doc_groups))
  }

  if (!is.null(label_groups)) {
    stopifnot("label_id" %in% colnames(label_groups))
  }

  # the set of title ids must always agree in predicted and gold_standard
  gold_wo_predicted <- collapse::join(
    x = gold_standard,
    y = predicted,
    on = "doc_id", how = "anti", verbose = 0
  )
  predicted_wo_gold <- collapse::join(
    x = predicted,
    y = gold_standard,
    on = "doc_id", how = "anti", verbose = 0
  )

  stopifnot(nrow(predicted_wo_gold) == 0)
  if (nrow(gold_wo_predicted) > 0 && !ignore_inconsistencies) {
    warning(
      "gold standard data contains documents ",
      "that are not in predicted set"
    )
  }

  compare <- collapse::join(
    x = collapse::ftransform(gold_standard, gold = TRUE),
    y = collapse::ftransform(predicted, suggested = TRUE),
    how = "full",
    on = c("doc_id", "label_id"),
    validate = "1:1",
    verbose = 0,
    suffix = c(".gold", ".pred") # this is intended to differentiate columns
    # that might be dragged along later
  )

  if (!is.null(doc_groups)) {
    compare <- collapse::join(
      x = compare,
      y = doc_groups,
      on = c("doc_id"),
      how = "left",
      validate = "m:1",
      verbose = 0
    )
  }


  if (!is.null(label_groups)) {
    compare <- collapse::join(
      x = compare,
      y = label_groups,
      on = c("label_id"),
      how = "left",
      validate = "m:1",
      verbose = 0
    )
  }

  if (propensity_scored) {
    label_weights <- compute_propensity_scores(label_distribution)
    compare <- join_propensity_scores(compare, label_weights)
  }

  result <- tidyr::replace_na(
    compare,
    replace = list(
      gold = FALSE,
      suggested = FALSE
    )
  )


  if (graded_relevance) {
    result <- check_repair_relevance_compare(result)
  } else {
    # test if column relevane exists
    if ("relevance" %in% colnames(result) && !ignore_inconsistencies) {
      warning("column 'relevance' in predicted is ignored, as
              graded_relevance = FALSE. Overwriting with relevance = 0.
              Silence this warning by setting ignore_inconsistencies = TRUE")
    }
    result <- collapse::ftransform(result, relevance = 0)
  }
}
