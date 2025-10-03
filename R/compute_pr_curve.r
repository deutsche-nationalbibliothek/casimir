#' compute precision-recall-curve for a given step size and limit_range
#' @inheritParams compute_set_retrieval_scores
#' @param predicted multi-label prediction results. expects \code{data.frame}
#'   with cols \emph{"label_id", "doc_id", "score"}
#' @param steps how many steps to take between c(0,1) as a grid for computing
#'  the pr-curve
#' @param limit_range a vector of limit values to apply on rank-column.
#'   Defaults to NA, applying no cutoff on label-rank of predictions.
#' @param optimize_cutoff logical. If \code{TRUE} performs a grid search to
#'   find optimal limit and threshold  with respect to f1-measure in the
#'   search space specified by \code{limit_range} and \code{steps}.
#' @inheritParams option_params
#'
#' @return a \code{list} with of two elemets:
#'   \enumerate{
#'     \item \code{plot_data} a \code{data.frame} with cols
#'       \code{c("searchspace_id", "prec", "rec", "prec_cummax")}
#'     \item \code{opt_cutoff} a \code{data.frame} with cols
#'       \code{c("thresholds", "limits", "f1_max",
#'       "prec", "rec", "prec_cummax")}
#'   }
#'
#'
#'   and possibly additional stratification variables passed with doc_groups
#'   and label_groups
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' library(casimir)
#'
#' gold <- tibble::tribble(
#' ~doc_id, ~label_id,
#' "A", "a",
#' "A", "b",
#' "A", "c",
#' "B", "a",
#' "B", "d",
#' "C", "a",
#' "C", "b",
#' "C", "d",
#' "C", "f"
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
#'   "C", "f", 0.1, 1,
#'   "C", "c", 0.2, 2,
#'   "C", "e", 0.2, 2
#' )
#'
#' pr_curve <- compute_pr_curve(
#'   gold,
#'   pred,
#'   mode = "doc-avg",
#'   optimize_cutoff = TRUE
#' )
#'
#' auc <- compute_pr_auc_from_curve(pr_curve$plot_data)
#'
#'
#' # note that pr-curves take the cummax(prec), not the precision
#' ggplot(pr_curve$plot_data,   aes(x = rec, y = prec_cummax)) +
#'   geom_point(data = pr_curve$opt_cutoff,
#'              aes(x = rec, y = prec_cummax),
#'              color = "red",
#'              shape = "star"
#'   ) +
#'   geom_text(data = pr_curve$opt_cutoff,
#'             aes(x = rec + 0.2, y = prec_cummax,
#'             label = paste("f1_opt =", round(f1_max,3))),
#'             color = "red"
#'   ) +
#'   geom_path() +
#'   coord_cartesian(xlim = c(0,1), ylim = c(0,1))
compute_pr_curve <- function(
  gold_standard, predicted,
  doc_groups = NULL,
  label_groups = NULL,
  mode = "doc-avg",
  steps = 100,
  limit_range = NA_real_,
  optimize_cutoff = FALSE,
  graded_relevance = FALSE,
  propensity_scored = FALSE,
  label_distribution = NULL,
  cost_fp_constant = NULL,
  ignore_inconsistencies = options::opt("ignore_inconsistencies"),
  verbose = options::opt("verbose"),
  progress = options::opt("progress")
) {

  stopifnot(is.numeric(limit_range))
  if (!all(is.na(limit_range)))
    stopifnot(all(limit_range >= 1L))

  grouping_var <- set_grouping_var(mode, doc_groups, label_groups)

  if (!all(is.na(limit_range)) && !"rank" %in% colnames(predicted)) {
    predicted <- dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(predicted, .data$doc_id),
        rank = dplyr::min_rank(-.data$score)
      )
    )
  }

  base_compare <- create_comparison(
    gold_standard, predicted,
    doc_groups = doc_groups,
    label_groups = label_groups,
    graded_relevance = graded_relevance,
    propensity_scored = propensity_scored,
    label_distribution = label_distribution,
    ignore_inconsistencies = ignore_inconsistencies
  )

  if (propensity_scored && !is.null(cost_fp_constant))
    cost_fp_processed <- process_cost_fp(cost_fp_constant, base_compare)
  else
    cost_fp_processed <- NULL

  ps_flags <- set_ps_flags(mode, propensity_scored)

  searchspace <- tidyr::expand_grid(
    thresholds = seq(0, 1, by = 1 / steps),
    limits = limit_range
  )
  searchspace <- dplyr::mutate(
    searchspace,
    searchspace_id = 1:(dplyr::n())
  )
  thresholds_list <- tibble::deframe(
    dplyr::select(searchspace,
                  "searchspace_id",
                  "thresholds")
  )

  get_results_per_searchspace_id <- function(
    threshold, limit, base_compare, grouping_var
  ) {
    compare_thresholded <- apply_threshold(
      threshold = threshold,
      limit = limit,
      base_compare = base_compare
    )
    intermed_res <- compute_intermediate_results(
      gold_vs_pred = compare_thresholded,
      grouping_var = grouping_var,
      propensity_scored = ps_flags$intermed,
      cost_fp = cost_fp_processed
    )

    summarise_intermediate_results(
      intermed_res,
      propensity_scored = ps_flags$summarise
    )

  }
  if (verbose)
    message("Computing set retrieval metrics for all thresholds and limits.")


  # get results per searchspace_id and
  # glue together data.frames with different searchspace_id
  pr_all_thrsld <- furrr::future_map2_dfr(
    .x = thresholds_list,
    .y = searchspace$limits,
    .f = get_results_per_searchspace_id,
    .id = "searchspace_id",
    .progress = progress,
    base_compare = base_compare,
    grouping_var = grouping_var,
    .options = furrr::furrr_options(seed = 43544)
  )

  if (optimize_cutoff) {
    grouping_var_stripped <- setdiff(
      grouping_var, c("doc_id", "label_id", "searchspace_id")
    )
    f1_results <- dplyr::filter(pr_all_thrsld, metric == "f1")
    f1_results_grpd <- dplyr::group_by(
      f1_results, !!!rlang::syms(grouping_var_stripped)
    )
    f1_results_grpd <- dplyr::mutate(f1_results_grpd, f1_max = max(value))
    f1_opt <- dplyr::filter(f1_results_grpd, value == f1_max)
    f1_opt <- dplyr::mutate(
      f1_opt,
      searchspace_id = as.integer(.data$searchspace_id)
    )

    opt_searchspace_id <- dplyr::summarise(
      f1_opt,
      searchspace_id = dplyr::first(.data$searchspace_id)
    )

    opt_cutoff <- dplyr::inner_join(
      searchspace,
      opt_searchspace_id,
      by = "searchspace_id"
    )

    opt_cutoff <- dplyr::select(
      dplyr::left_join(
        opt_cutoff,
        f1_opt, by = c("searchspace_id", grouping_var_stripped)
      ),
      dplyr::all_of(
        c("thresholds",
          "limits",
          "searchspace_id",
          "f1_max",
          grouping_var_stripped)
      )
    )

  } else {
    opt_cutoff <- NULL
    all_cutoffs <- NULL
  }

  pr_all_thrsld_reshaped <- pr_curve_post_processing(
    results_summary = pr_all_thrsld
  )

  pr_all_thrsld_reshaped <- dplyr::mutate(
    pr_all_thrsld_reshaped,
    mode = mode
  )

  pr_all_thrsld_reshaped <- dplyr::mutate(
    pr_all_thrsld_reshaped,
    searchspace_id = as.integer(.data$searchspace_id)
  )

  if (optimize_cutoff) {
    opt_cutoff <- dplyr::left_join(
      opt_cutoff,
      pr_all_thrsld_reshaped,
      by = c("searchspace_id", grouping_var_stripped)
    )

    all_cutoffs <- dplyr::ungroup(f1_results_grpd)
    all_cutoffs <-  dplyr::mutate(
      all_cutoffs,
      searchspace_id = as.integer(.data$searchspace_id)
    )
    all_cutoffs <- dplyr::left_join(
      searchspace,
      all_cutoffs,
      by = c("searchspace_id")
    )
    all_cutoffs <- dplyr::left_join(
      all_cutoffs,
      pr_all_thrsld_reshaped,
      by = c("searchspace_id",  grouping_var_stripped)
    )
  }

  list(plot_data = pr_all_thrsld_reshaped, opt_cutoff = opt_cutoff,
       all_cutoffs = all_cutoffs)

}
