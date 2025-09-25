#' Rename metric names for generalized precision etc.
#'
#' @param res_df data.frame with column \emph{"metric"} containing metric names
#'   prec, rec, f1, rprec
#'
#' @return results data.frame with renamed metrics for generalized precision etc.
rename_metrics <- function(res_df) {
  stopifnot("metric" %in% colnames(res_df))

  res_df <- dplyr::mutate(res_df,
                          metric = dplyr::case_when(
                            .data$metric == "rec" ~ "g-rec",
                            .data$metric == "prec" ~ "g-prec",
                            .data$metric == "f1" ~ "g-f1",
                            .data$metric == "rprec" ~ "g-rprec",
                            TRUE ~ .data$metric
                          ))

  res_df
}
