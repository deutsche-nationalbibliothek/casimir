#' Compute area under precision-recall curve
#'
#' Compute the area under the precision-recall curve given pr curve data. This
#' function is mainly intended for generating plot data. For computation of the
#' area under the curve, use \code{compute_pr_auc}. The function uses a simple
#' trapezoidal rule approximation along the steps of the generated curve data.
#'
#' @param pr_curve_data A data.frame as produced by
#'   \code{compute_pr_curve}, containing columns \code{"searchspace_id",
#'   "prec", "rec", "prec_cummax", "mode"}.
#' @param grouping_vars Additional columns of the input data to group by.
#' @inheritParams option_params
#'
#' @return A data.frame with a column \code{"pr_auc"} and optional
#'   \code{grouping_vars}.
#' @export
#'
#' @seealso compute_pr_curve
#'
#' @examples
#'
#' library(ggplot2)
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
#'   ~doc_id, ~label_id, ~score, ~rank,
#'   "A", "a", 0.9, 1,
#'   "A", "d", 0.7, 2,
#'   "A", "f", 0.3, 3,
#'   "A", "c", 0.1, 4,
#'   "B", "a", 0.8, 1,
#'   "B", "e", 0.6, 2,
#'   "B", "d", 0.1, 3,
#'   "C", "f", 0.1, 3,
#'   "C", "c", 0.2, 1,
#'   "C", "e", 0.2, 1
#' )
#'
#' pr_curve <- compute_pr_curve(
#'   gold,
#'   pred,
#'   mode = "doc-avg",
#'   optimize_cutoff = TRUE
#' )
#'
#' auc <- compute_pr_auc_from_curve(pr_curve)
#'
#' # note that pr curves take the cummax(prec), not the precision
#' ggplot(pr_curve$plot_data, aes(x = rec, y = prec_cummax)) +
#'   geom_point(
#'     data = pr_curve$opt_cutoff,
#'     aes(x = rec, y = prec_cummax),
#'     color = "red",
#'     shape = "star"
#'   ) +
#'   geom_text(
#'     data = pr_curve$opt_cutoff,
#'     aes(
#'       x = rec + 0.2, y = prec_cummax,
#'       label = paste("f1_opt =", round(f1_max, 3))
#'     ),
#'     color = "red"
#'   ) +
#'   geom_path() +
#'   coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
compute_pr_auc_from_curve <- function(
    pr_curve_data,
    grouping_vars = NULL,
    drop_empty_groups = options::opt("drop_empty_groups")) {
  if (!is.data.frame(pr_curve_data) && !is.null(pr_curve_data$plot_data)) {
    plot_data <- pr_curve_data$plot_data
  } else {
    plot_data <- pr_curve_data
  }
  stopifnot(is.data.frame(plot_data))
  stopifnot(
    all(c("searchspace_id", "rec", "prec_cummax") %in% colnames(plot_data))
  )
  stopifnot(all(grouping_vars %in% colnames(plot_data)))

  if (!is.null(grouping_vars)) {
    plot_data_grpd <- dplyr::group_by(
      plot_data,
      !!!rlang::syms(c(grouping_vars)),
      .add = TRUE,
      .drop = drop_empty_groups
    )
  } else {
    plot_data_grpd <- plot_data
  }

  # sort the data by recall
  plot_data_grpd <- dplyr::arrange(
    plot_data_grpd,
    .data$rec, dplyr::desc(.data$searchspace_id)
  )

  # expect prec_cummax to be monotone
  test_monotonicity <- dplyr::transmute(
    plot_data_grpd,
    k = .data$prec_cummax,
    k_m_1 = dplyr::lead(.data$prec_cummax),
    increment_postive = .data$prec_cummax - dplyr::lead(.data$prec_cummax) >= 0
  )
  stopifnot(
    all(
      utils::head(test_monotonicity[["increment_postive"]], -1),
      na.rm = TRUE
    )
  )

  prepare_integral <- dplyr::mutate(
    plot_data_grpd,
    prec_cummax_k = .data$prec_cummax,
    rec_k_minus_1 = dplyr::lag(.data$rec),
    prec_cummax_k_minus_1 = dplyr::lag(.data$prec_cummax_k),
    delta_f = (.data$prec_cummax_k + .data$prec_cummax_k_minus_1) / 2,
    delta_h = .data$rec - .data$rec_k_minus_1,
    integrand = .data$delta_f * .data$delta_h
  )

  dplyr::summarise(
    prepare_integral,
    pr_auc = sum(.data$integrand, na.rm = TRUE)
  )
}
