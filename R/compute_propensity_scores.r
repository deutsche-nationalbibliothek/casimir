#' Compute inverse propensity scores
#'
#' Compute inverse propensity scores based on a label distribution. Propensity
#' scores for extreme multi-label learning are proposed in Jain, H., Prabhu, Y.,
#' & Varma, M. (2016). Extreme Multi-label Loss Functions for Recommendation,
#' Tagging, Ranking and Other Missing Label Applications. Proceedings of the
#' 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data
#' Mining, 13-17-Aug, 935–944. \url{https://doi.org/10.1145/2939672.2939756}.
#'
#' @param label_distribution Expects a data.frame with columns \code{"label_id",
#'   "label_freq", "n_docs"}. \code{label_freq} corresponds to the number of
#'   occurences a label has in the gold standard. \code{n_docs} corresponds to
#'   the total number of documents in the gold standard.
#' @param a A numeric parameter for the propensity score calculation, defaults
#'   to 0.55.
#' @param b A numeric parameter for the propensity score calculation, defaults
#'   to 1.5.
#'
#' @return A data.frame with columns \code{"label_id", "label_weight"}.
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
compute_propensity_scores <- function(
    label_distribution,
    a = 0.55,
    b = 1.5) {
  stopifnot(all(
    c("label_id", "label_freq", "n_docs") %in% colnames(label_distribution)
  ))

  n <- dplyr::pull(label_distribution, .data$n_docs)[1]
  stopifnot(n > 0)
  c <- (log(n) - 1) * (b + 1)**a

  # apply formula (2) from Jain et al. (2016)
  dplyr::transmute(
    label_distribution,
    label_id = .data$label_id,
    label_weight = 1 + c * (.data$label_freq + b)**(-a)
  )
}
