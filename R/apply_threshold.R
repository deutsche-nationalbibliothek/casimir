#' Helper function for filtering preditions with score above a certain threshold
#'   or rank below some limit rank.
#'
#' @param threshold numeric threshold between 0 and 1
#' @param limit integer cutoff >= 1 for rankbased thresholding. Requires column
#'   \code{"gold"} in input \code{base_compare}
#' @param base_compare \code{data.frame} as created by \code{create_comparison},
#'   containing cols \code{c("gold", "score")}
#'
#' @return \code{data.frame} with observations that satisfy
#'   (\code{score > threshold} AND (if appliccable) \code{rank <= limit})
#'   OR \code{gold == TRUE}
#'   A new logical column \code{suggested} indicates true, if
#'   score > threshold AND (if appliccable) rank <= limit
#'   Otherwise false for false negative observations (that may have no score,
#'   a score below the threshold or rank above limit)
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
#'   ~doc_id, ~label_id, ~score,
#'   "A", "a", 0.9,
#'   "A", "d", 0.7,
#'   "A", "f", 0.3,
#'   "A", "c", 0.1,
#'   "B", "a", 0.8,
#'   "B", "e", 0.6,
#'   "B", "d", 0.1,
#'   "C", "f", 0.1,
#'   "C", "c", 0.2,
#'   "C", "e", 0.2
#' )
#'
#' base_compare <- casimir:::create_comparison(gold, pred)
#' # apply zero as threshold should not change anything
#' res_0 <- casimir:::apply_threshold(
#'   threshold = 0.3,
#'   base_compare = base_compare
#' )
apply_threshold <- function(threshold, limit = NA_real_, base_compare) {

  stopifnot(all(c("gold", "score") %in% colnames(base_compare)))
  stopifnot(is.numeric(base_compare[["score"]]))
  stopifnot(sum(base_compare[["score"]] > 1, na.rm = TRUE) == 0)
  stopifnot(sum(base_compare[["score"]] < 0, na.rm = TRUE) == 0)
  stopifnot(is.logical(base_compare[["gold"]]))
  stopifnot(threshold >= 0 & threshold <= 1)
  stopifnot(is.numeric(limit))

  compare_w_limit <- base_compare
  if (!is.na(limit)) {
    stopifnot(limit >= 1)
    # only require rank-column, if limit is set
    stopifnot("rank" %in% colnames(base_compare))

    compare_w_limit <- dplyr::filter(
      compare_w_limit, .data$gold | .data$rank <= limit
    )
  }

  compare_w_cutoff <- dplyr::filter(
    compare_w_limit, .data$gold | .data$score >= threshold
  )

  compare_w_cutoff <- dplyr::mutate(
    compare_w_cutoff,
    suggested = dplyr::if_else(.data$score >= threshold, TRUE, FALSE, FALSE)
  )

  if (!is.na(limit)) {
    compare_w_cutoff <- dplyr::mutate(
      compare_w_cutoff,
      suggested = dplyr::if_else(.data$suggested & .data$rank <= limit,
                                 TRUE, FALSE, FALSE)
    )
  }
  compare_w_cutoff
}
