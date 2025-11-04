#' Join propensity scores
#'
#' Helper function to perform a secure join of a comparison matrix with
#' propensity scores.
#'
#' @param input_data A data.frame containing at least the column
#'   \code{"label_id"}.
#' @param label_weights Expects a data.frame with columns \code{"label_id",
#'   "label_weight"}.
#' @return The input data.frame \code{input_data} with an additional column
#'   \code{"label_weight"}.
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
#' label_distribution <- tibble::tribble(
#'   ~label_id, ~label_freq, ~n_docs,
#'   "a", 10000, 10100,
#'   "b", 1000, 10100,
#'   "c", 100, 10100,
#'   "d", 1, 10100,
#'   "e", 1, 10100,
#'   "f", 2, 10100,
#'   "g", 0, 10100
#' )
#'
#' comp <- create_comparison(gold, pred)
#' label_weights <- compute_propensity_scores(label_distribution)
#' comp_w_label_weights <- join_propensity_scores(
#'   input_data = comp,
#'   label_weights = label_weights
#' )
join_propensity_scores <- function(
    input_data,
    label_weights) {
  rlang::try_fetch(
    {
      compare <- collapse::join(
        x = input_data,
        y = label_weights,
        on = "label_id",
        how = "left",
        validate = "m:1",
        # expect every record in x to be matched
        require = list(x = 1, fail = "error"),
        verbose = 0
      ) # i.e. every label in input_data must have
      # exactly one weight but the label_weights
      # may contain more labels than compare
    },
    error = function(cnd) {
      rlang::abort(
        "Label distribution does not match input data.",
        parent = cnd
      )
    }
  )

  compare
}

#' @describeIn join_propensity_scores Variant with dplyr based
#' internals rather than collapse internals.
join_propensity_scores_dplyr <- function(
    input_data,
    label_weights) {
  compare <- dplyr::inner_join(
    x = input_data,
    y = label_weights,
    by = c("label_id"),
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  )

  compare
}
