#' Check for inconsistent relevance values
#'
#' Internal helper function to check a comparison matrix for inconsistent
#' relevance values of gold standard and predicted labels.
#'
#' @param gold_vs_pred As created by \code{create_comparison}.
#' @inheritParams option_params
#'
#' @return A valid comparison matrix with possibly corrected relevance values,
#'   being compatible with \code{compute_intermediate_results}.
check_repair_relevance_compare <- function(
    gold_vs_pred,
    ignore_inconsistencies = options::opt("ignore_inconsistencies")) {
  # set relevance to 1 if gold == TRUE and relevance missing
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
  if (inconsistent_values_tp > 0 && !ignore_inconsistencies) {
    warning(paste(
      "There are",
      inconsistent_values_tp,
      "inconsistent relevance values with `relevance < 1` but",
      "`gold == TRUE`. Setting relevance to 1."
    ))
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
  if (inconsistent_values_fp > 0 && !ignore_inconsistencies) {
    warning(paste(
      "There are",
      inconsistent_values_fp,
      "inconsistent relevance values with `relevance == 1` but",
      "`gold == FALSE`. Please correct relevance or gold standard."
    ))
  }

  compare
}

#' Check for inconsistent relevance values
#'
#' Internal helper function to check a data.frame with predicted labels for a
#' valid relevance column.
#'
#' @param predicted Multi-label prediction results. Expects a data.frame with
#'   columns \code{"label_id", "doc_id", "relevance"}.
#' @inheritParams option_params
#'
#' @return A valid \code{predicted} data.frame with possibly eliminated missing
#'   values.
check_repair_relevance_pred <- function(
    predicted,
    ignore_inconsistencies = options::opt("ignore_inconsistencies")) {
  stopifnot("relevance" %in% colnames(predicted))
  stopifnot(is.numeric(predicted[["relevance"]]))
  # check for missing relevance values
  if (sum(is.na(predicted[["relevance"]])) > 0 && !ignore_inconsistencies) {
    warning("NA values in `relevance` column. Removing rows with NA values.")
    predicted <- dplyr::filter(predicted, !is.na(.data$relevance))
  }
  # check that relevance is between 0 and 1
  if (any(predicted[["relevance"]] < 0) || any(predicted[["relevance"]] > 1)) {
    stop("Relevance values must be between 0 and 1.")
  }

  predicted
}
