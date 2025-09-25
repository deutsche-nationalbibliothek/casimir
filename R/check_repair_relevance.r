#' Internal helper function to check comparison matrix for
#' inconsistent gold_standard and relevance values
#'
#' @param gold_vs_pred as created by \code{create_comparison}
#'
#' @return comparison matrix compatible with compute_intermediate_results
check_repair_relevance_compare <- function(gold_vs_pred) {

  # set relevance to 1 if gold_standard = TRUE and relevance missing
  compare <- dplyr::mutate(
    gold_vs_pred,
    relevance = dplyr::if_else(
      .data$gold & is.na(.data$relevance), 1,
      .data$relevance
    )
  )

  inconsistent_values_tp <- nrow(
    dplyr::filter(
      compare,
      .data$gold & !is.na(.data$relevance) & .data$relevance < 1
    )
  )
  if (inconsistent_values_tp > 0) {
    warning(
      paste(
        "There are",
        inconsistent_values_tp,
        "inconsistent relevance values with relevance < 1 but",
        "gold_standard = TRUE. Setting relevance to 1."
      )
    )
    compare <- dplyr::mutate(
      compare,
      relevance = dplyr::if_else(
        .data$gold & !is.na(.data$relevance) &
          .data$relevance < 1, 1, .data$relevance
      )
    )

  }

  inconsistent_values_fp <- nrow(
    dplyr::filter(
      compare,
      !.data$gold & !is.na(.data$relevance) & .data$relevance == 1
    )
  )
  if (inconsistent_values_fp > 0) {
    warning(
      paste(
        "There are",
        inconsistent_values_fp,
        "inconsistent relevance values with relevance == 1 but",
        "gold_standard = FALSE. Please correct relevance or gold_standard."
      )
    )
  }

  compare
}

#' Internal helper to check predicted data frame for valid relevance column
#'
#' @param predicted expects data.frame with column \emph{"relevance"}
#'
#' @return valid predicted data.frame with possibly eliminated missing values
check_repair_relevance_pred <- function(predicted) {
  stopifnot("relevance" %in% colnames(predicted))
  stopifnot(is.numeric(predicted[["relevance"]]))
  # check for missing relevance values
  if (sum(is.na(predicted[["relevance"]])) > 0) {
    warning("NA values in 'relevance' column. Removing rows with NA values.")
    predicted <- dplyr::filter(predicted, !is.na(.data$relevance))
  }
  # check that relevance is between 0 and 1
  if (any(predicted[["relevance"]] < 0) || any(predicted[["relevance"]] > 1)) {
    stop("Relevance values must be between 0 and 1.")
  }

  predicted
}
