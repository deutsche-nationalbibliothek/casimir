#' Reshape pr_curve_data to a format that is easier for plotting
#'
#' @param results_summary as produced by summarise_intermediate_results
#'
#' @return a \code{data.frame} with cols
#'   \code{c("searchspace_id", "prec", "rec", "prec_cummax")}
#'   and possibly additional stratification variables
pr_curve_post_processing <- function(results_summary) {
  stopifnot(all(
    c("metric", "value", "searchspace_id", "support") %in% colnames(results_summary) # nolint
  ))

  results_summary <- dplyr::mutate(
    results_summary,
    searchspace_id = as.integer(.data$searchspace_id)
  )

  results_summary <- dplyr::filter(
    results_summary,
    .data$metric %in% c("prec", "rec")
  )

  results_summary <- tidyr::pivot_wider(
    dplyr::select(results_summary, -"support"),
    names_from = "metric",
    values_from = "value"
  )

  results_summary <- dplyr::mutate(
    results_summary,
    prec = ifelse(is.na(.data$prec), 0.0, .data$prec),
    rec = ifelse(is.na(.data$rec), 0.0, .data$rec)
  )

  grouping_var <- setdiff(
    colnames(results_summary),
    c(
      "searchspace_id",
      "prec",
      "rec",
      "prec_cummax"
    )
  )

  results_summary <- dplyr::group_by(
    results_summary, !!!rlang::syms(grouping_var)
  )

  results_summary <- dplyr::arrange(
    results_summary,
    dplyr::desc(.data$rec),
    .data$searchspace_id
  )

  results_summary <- dplyr::mutate(
    results_summary,
    prec_cummax = cummax(
      ifelse(is.na(.data$prec),
        0, .data$prec
      )
    )
  )

  # add a zero-precision value at the maximum recall for visualization
  group_max_recall <- dplyr::summarise(
    results_summary,
    rec = max(dplyr::coalesce(.data$rec, 0), na.rm = TRUE)
  )

  row_zero <- dplyr::mutate(
    group_max_recall,
    searchspace_id = 0L,
    prec = 0.0,
    prec_cummax = 0.0,
    rec = ifelse(is.na(rec), 0.0, rec)
  )

  # add a zero-recall value at the maximum precision for visualization
  group_max_precision <- dplyr::summarise(
    results_summary,
    searchspace_id = max(.data$searchspace_id) + 1L,
    prec = max(dplyr::coalesce(.data$prec, 0), na.rm = TRUE)
  )

  row_last <- dplyr::mutate(
    group_max_precision,
    prec_cummax = ifelse(is.na(prec), 0.0, prec),
    rec = 0.0,
  )

  res <- dplyr::bind_rows(
    row_zero,
    results_summary,
    row_last
  )

  # sort results cols
  dplyr::select(
    res, "searchspace_id", "prec", "rec", "prec_cummax",
    dplyr::everything()
  )
}
