#' Computes inverse propensity scores
#'
#' Propensity scores for extreme multi label learning are proposed in
#' Jain, H., Prabhu, Y., & Varma, M. (2016).
#' Extreme Multi-label Loss Functions for Recommendation, Tagging, Ranking and
#' Other Missing Label Applications.
#' Proceedings of the 22nd ACM SIGKDD
#' International Conference on Knowledge Discovery and Data Mining,
#' 13-17-Augu, 935–944. https://doi.org/10.1145/2939672.2939756
#'
#' @param label_distribution expects \code{data.frame} with cols
#'   \emph{"label_id", "label_freq", "n_docs"}. \code{label_freq} corresonds to
#'   the number of occurences a label has in the gold_standard. \code{n_docs}
#'   corresponds to the total number of documents in the gold_standard.
#' @param A \code{numeric} parameter for the propensity score calculation,
#'   defaults to 0.55
#' @param B \code{numeric} parameter for the propensity score calculation,
#'   defaults to 1.5
#'
#' @return a \code{data.frame} with cols \emph{"label_id", "label_weight"}
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(casimir)
#'
#' label_distribution <- dnb_label_distribution
#'
#' compute_propensity_scores(label_distribution)
#'
#'
compute_propensity_scores <- function(
    label_distribution,
    A = 0.55,
    B = 1.5
){

  stopifnot(all(c("label_id", "label_freq", "n_docs") %in% colnames(label_distribution)))

  N = dplyr::pull(label_distribution, n_docs)[1]
  stopifnot(N > 0)
  C <- (log(N) - 1) * (B +1 )**A

  # apply formula (2) from Jain et al. (2016)
  dplyr::transmute(
    label_distribution,
    label_id = .data$label_id,
    label_weight = 1 + C*(.data$label_freq + B)**(-A)
  )

}

#' Helper function to perform a secure join of comparision matrix with
#'   propensity scores
#'
#' @param input_data \code{data.frame} containing at least the column `label_id`
#' @param label_weights expects \code{data.frame} with cols
#'   \emph{"label_id", "label_weight"}.
#' @return the input \code{data.frame} \code{gold_vs_pred} with additional cols
#'   \emph{"label_weight"}
#' @export
#'
#' @examples
#' library(casimir)
#'
#' gold <- tibble::tribble(
#'     ~doc_id, ~label_id,
#'     "A", "a",
#'     "A", "b",
#'     "A", "c",
#'     "B", "a",
#'     "B", "d",
#'     "C", "a",
#'     "C", "b",
#'     "C", "d",
#'     "C", "f"
#' )
#'
#' pred<- tibble::tribble(
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
  label_weights
  ) {

  rlang::try_fetch({
    compare <- collapse::join(
        x = input_data,
        y = label_weights,
        on = "label_id",
        how = "left",
        validate = "m:1",
        # expect every record in x to be matched
        require =  list(x = 1, fail = "error"),
        verbose = 0
      ) # i.e. every label in gold_vs_pred must have
    # exactly one weight, but the label_weights
    # may contain more labels than compare
  },
  error = function(cnd)
    rlang::abort(
      "Label distribution does not match input data.",
      parent = cnd
    )
  )

  compare
  # Alternative version with dplyr
  # compare <- dplyr::inner_join(
  #   x = input_data,
  #   y = label_weights,
  #   by = c("label_id"),
  #   relationship = "many-to-one",
  #   unmatched = c("error", "drop"))
}
