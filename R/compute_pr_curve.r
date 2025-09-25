#' compute precision-recall-curve for a given step size and limit_range
#'
#' @param gold_standard expects \code{data.frame} with cols \emph{"label_id", "doc_id", "score"}
#' @param predicted multi-label prediction results. expects \code{data.frame} with cols \emph{"label_id", "doc_id", "score"}
#' @param mode aggregation mode: \emph{"doc-avg", "subj-avg", "micro"}
#' @param steps how many steps to take between c(0,1) as a grid for computing
#'  the pr-curve
#' @param doc_strata a column that exists in either gold_standard or predicted,
#'     that results should be grouped by, e.g. strata of document-space.
#'     \code{doc_strata} is recommended to be of type factor, so that levels are not
#'     implicitly dropped during bootstrap replications
#' @param label_dict two-column \code{data.frame} with col \emph{"label_id"}
#'   and a second column
#'   that defines groups of labels to stratify results by. Results in each stratum
#'   will restrict gold_standard and predictions to the specified label-groups,
#'   as if the vocabulary was consisting of the label group only.
#'   All modes \code{c("doc-avg", "subj-avg", "micro") } are supported within
#'   label-strata. Nevertheless, mixing \code{mode = "doc-avg"} with fine-grained
#'   label_strata can result in many missing values on document-level results.
#'   Also rank-based thresholding (e.g. Top-5) will result in inhomogeneous
#'   number of labels per documents within the defined label-strata.
#'   \code{mode = "subj-avg"} or \code{mode = "micro"} can be more appropriate
#'   in these circumstances.
#' @param limit_range a vector of limit values to apply on rank-column.
#'   Defaults to NA, applying no cutoff on label-rank of predictions.
#' @param optimize_cutoff logical. If \code{TRUE} performs a grid search to
#'   find optimal limit and threshold  with respect to f1-measure in the
#'   search space specified by \code{limit_range} and \code{steps}.
#' @param graded_relevance logical indicator for graded relevance. Defaults to
#'  \code{FALSE} for binary relevance. If set to \code{TRUE}, the
#'  \code{predicted} data.frame should contain a numeric column
#'  \emph{"relevance"} with values in the range of \code{c(0, 1)}.
#' @param propensity_scored logical, whether to use propensity scores as weights
#' @param label_distribution expects \code{data.frame} with cols
#'   \emph{"label_id", "label_freq", "n_docs"}. \code{label_freq} corresonds to
#'   the number of occurences a label has in the gold_standard. \code{n_docs}
#'   corresponds to the total number of documents in the gold_standard.
#' @param cost_fp_constant Constant cost assigned to false positives. cost_fp_constant must be
#'  a numeric value > 0 or one of 'max', 'min', 'mean' (computed with reference
#'   to the \code{gold_standard} label distribution). The default is NULL, i.e.
#'   label weights are appplied to false positices as to false negatives and true positives.
#' @param .ignore_relevance_warning logical, if graded_relevance = FALSE, but
#'   column relevance is present in predicted, a warning can be silenced by
#'   setting .ignore_relevance_warning = TRUE
#' @param .verbose logical indicator for verbose output, defaults to FALSE
#' @param .progress logical activating .progress bar in internal
#'  parallel \pkg{furrr}-computation
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
#'   and possibly additional stratification variables passed with doc_strata
#'   and label_dict
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
compute_pr_curve <- function(gold_standard, predicted,
                             doc_strata = NULL,
                             label_dict = NULL,
                             mode = "doc-avg",
                             steps = 100,
                             limit_range = NA_real_,
                             optimize_cutoff = FALSE,
                             graded_relevance = FALSE,
                             propensity_scored = FALSE,
                             label_distribution = NULL,
                             cost_fp_constant = NULL,
                             .ignore_relevance_warning = FALSE,
                             .verbose = FALSE,
                             .progress = FALSE) {

  stopifnot(is.numeric(limit_range))
  if (!all(is.na(limit_range)))
    stopifnot(all(limit_range>=1L))

  grouping_var <- set_grouping_var(mode, doc_strata, label_dict)

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
    doc_strata = doc_strata,
    label_dict = label_dict,
    graded_relevance = graded_relevance,
    propensity_scored = propensity_scored,
    label_distribution = label_distribution,
    .ignore_relevance_warning = .ignore_relevance_warning
  )

  if (propensity_scored & !is.null(cost_fp_constant))
    cost_fp_processed <- process_cost_fp(cost_fp_constant, base_compare)
  else
    cost_fp_processed <- NULL

  ps_flags <- set_ps_flags(mode, propensity_scored)

  searchspace <- tidyr::expand_grid(
    thresholds = seq(0, 1, by = 1/steps),
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
  if (.verbose)
    message("Computing set retrieval metrics for all thresholds and limits.")


  # get results per searchspace_id and
  # glue together data.frames with different searchspace_id
  pr_all_thrsld <- furrr::future_map2_dfr(
    .x = thresholds_list,
    .y = searchspace$limits,
    .f = get_results_per_searchspace_id,
    .id = "searchspace_id",
    .progress = .progress,
    base_compare = base_compare,
    grouping_var = grouping_var,
    .options = furrr::furrr_options(seed = 43544)
  )

  if (optimize_cutoff) {
    grouping_var_stripped <- setdiff(grouping_var, c("doc_id", "label_id", "searchspace_id"))
    f1_results <- dplyr::filter(pr_all_thrsld, metric == "f1")
    f1_results_grpd <- dplyr::group_by(f1_results, !!!rlang::syms(grouping_var_stripped))
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
          grouping_var_stripped))
    )

  } else {
    opt_cutoff = NULL
    all_cutoffs = NULL
  }

  pr_all_thrsld_reshaped <- pr_curve_post_processing(
    results_summary = pr_all_thrsld)

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
      by = c("searchspace_id"))
    all_cutoffs <- dplyr::left_join(
      all_cutoffs,
      pr_all_thrsld_reshaped,
      by = c("searchspace_id",  grouping_var_stripped)
    )
  }

  list(plot_data = pr_all_thrsld_reshaped, opt_cutoff = opt_cutoff,
       all_cutoffs = all_cutoffs)

}

#' Reshape pr_curve_data to a format that is easier for plotting
#'
#' @param results_summary as produced by summarise_intermediate_results
#'
#' @return a \code{data.frame} with cols \code{c("searchspace_id", "prec", "rec", "prec_cummax")}
#'   and possibly additional stratification variables
pr_curve_post_processing <- function(results_summary) {

  stopifnot(all(c("metric", "value", "searchspace_id", "support") %in% colnames(results_summary)))

  results_summary <- dplyr::mutate(
    results_summary,
    searchspace_id = as.integer(.data$searchspace_id)
  )

  results_summary <- dplyr::filter(
    results_summary,
    .data$metric %in% c("prec", "rec"))

  results_summary <- tidyr::pivot_wider(
    dplyr::select(results_summary,
                  -"support"),
    names_from = "metric",
    values_from = "value")

  results_summary <- dplyr::mutate(
    results_summary,
    prec = ifelse(is.na(.data$prec), 0.0, .data$prec),
    rec = ifelse(is.na(.data$rec), 0.0, .data$rec)
  )

  grouping_var <- setdiff(colnames(results_summary),
                          c("searchspace_id",
                            "prec",
                            "rec",
                            "prec_cummax"))

  results_summary <- dplyr::group_by(
    results_summary, !!!rlang::syms(grouping_var)
  )

  results_summary <- dplyr::arrange(
    results_summary,
    dplyr::desc(.data$rec),
    .data$searchspace_id
    #dplyr::desc(.data$prec)
  )

  results_summary <- dplyr::mutate(
    results_summary,
    prec_cummax = cummax(
      ifelse(is.na(.data$prec),
             0, .data$prec)
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

#' Compute area under precision-recall curve given pr-curve data
#'
#' This function is mainly intended for generating plot data. For computation of
#'  the area under the curve use compute_pr_auc. The function uses a simple
#'  trapezoidal rule approximation along the steps of the generated curve data.
#'
#' @param pr_curve_data a \code{data.frame} as produced by
#'   \code{compute_pr_curve}, containing cols "searchspace_id",
#'   "prec", "rec", "prec_cummax", "mode"
#' @param grouping_vars additional columns of the input data to group by
#'
#' @return a \code{data.frame} with col pr_auc and potential grouping_vars
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
compute_pr_auc_from_curve <- function(pr_curve_data, grouping_vars = NULL) {

  if (!is.data.frame(pr_curve_data) && !is.null(pr_curve_data$plot_data)) {
    plot_data = pr_curve_data$plot_data
  } else {
    plot_data = pr_curve_data
  }
  stopifnot(is.data.frame(plot_data))
  stopifnot(
    all(c("searchspace_id", "rec", "prec_cummax") %in% colnames(plot_data))
  )
  stopifnot(all(grouping_vars %in% colnames(plot_data)))

  if (!is.null(grouping_vars))
    plot_data_grpd <- dplyr::group_by(
      plot_data,
      !!!rlang::syms(c(grouping_vars)),
      .add = TRUE)
  else
    plot_data_grpd <- plot_data

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
    increment_postive = .data$prec_cummax - dplyr::lead(.data$prec_cummax) >=0
  )
  stopifnot(
    all(utils::head(test_monotonicity[["increment_postive"]],-1), na.rm = TRUE))

  prepare_integral <- dplyr::mutate(
    plot_data_grpd,
    prec_cummax_k = .data$prec_cummax,
    rec_k_minus_1 = dplyr::lag(.data$rec),
    prec_cummax_k_minus_1 = dplyr::lag(.data$prec_cummax_k),
    delta_f = (.data$prec_cummax_k + .data$prec_cummax_k_minus_1)/2,
    delta_h = .data$rec - .data$rec_k_minus_1,
    integrand = .data$delta_f * .data$delta_h)

  dplyr::summarise(
    prepare_integral,
    pr_auc = sum(.data$integrand, na.rm = TRUE)
  )

}

#' Compute Area under precision recall curve with support for bootstrap based
#' confidence intervals and different stratification and aggregation modes
#' for the underlying precision and recall aggregation
#' Precision is calculated as the best value at a given level of recall for
#' all possible thresholds on score and limits on rank. In essence, compute_pr_auc
#' performs a two dimensional optimisation over thresholds and limits applying
#' both threshold-based cutoff as well as rank-based cutoff.
#'
#' @param gold_standard expects \code{data.frame} with cols \emph{"label_id", "doc_id", "score"}
#' @param predicted multi-label prediction results. expects \code{data.frame} with cols \emph{"label_id", "doc_id", "score"}
#' @param mode aggregation mode: \emph{"doc-avg", "subj-avg", "micro"}
#' @param compute_bootstrap_ci logical indicator for computing bootstrap CIs
#' @param n_bt an integer number of resamples to undergo in bootstrapping
#' @param doc_strata a column that exists in either gold_standard or predicted,
#'     that results should be grouped by, e.g. strata of document-space.
#'     \code{doc_strata} is recommended to be of type factor, so that levels are not
#'     implicitly dropped during bootstrap replications
#' @param label_dict two-column \code{data.frame} with col \emph{"label_id"}
#'   and a second column
#'   that defines groups of labels to stratify results by. Results in each stratum
#'   will restrict gold_standard and predictions to the specified label-groups,
#'   as if the vocabulary was consisting of the label group only.
#'   All modes \code{c("doc-avg", "subj-avg", "micro") } are supported within
#'   label-strata. Nevertheless, mixing \code{mode = "doc-avg"} with fine-grained
#'   label_strata can result in many missing values on document-level results.
#'   Also rank-based thresholding (e.g. Top-5) will result in inhomogeneous
#'   number of labels per documents within the defined label-strata.
#'   \code{mode = "subj-avg"} or \code{mode = "micro"} can be more appropriate
#'   in these circumstances.
#' @param .progress logical activating .progress bar in internal
#'  \pkg{furrr}-computation
#' @param seed pass seed to make bootstrap replication reproducible
#' @param steps number of threshold-steps to use in auc-computation
#' @param limit_range a vector of limit values to apply on rank-column.
#'   Defaults to NA, applying no cutoff on label-rank of predictions.
#' @param graded_relevance logical indicator for graded relevance. Defaults to
#'  \code{FALSE} for binary relevance. If set to \code{TRUE}, the
#'  \code{predicted} data.frame should contain a numeric column
#'  \emph{"relevance"} with values in the range of \code{c(0, 1)}.
#' @param rename_graded_metrics if set to \code{TRUE}, the metric names in
#'   the output will be prefixed with \emph{"g_"} to indicate that metrics
#'   are computed with graded relevance.
#' @param propensity_scored logical, whether to use propensity scores as weights
#' @param label_distribution expects \code{data.frame} with cols
#'   \emph{"label_id", "label_freq", "n_docs"}. \code{label_freq} corresonds to
#'   the number of occurences a label has in the gold_standard. \code{n_docs}
#'   corresponds to the total number of documents in the gold_standard.
#' @param cost_fp_constant Constant cost assigned to false positives. cost_fp_constant must be
#'  a numeric value > 0 or one of 'max', 'min', 'mean' (computed with reference
#'   to the \code{gold_standard} label distribution). The default is NULL, i.e.
#'   label weights are appplied to false positices as to false negatives and true positives.
#' @param .ignore_relevance_warning logical, if graded_relevance = FALSE, but
#'   column relevance is present in predicted, a warning can be silenced by
#'   setting .ignore_relevance_warning = TRUE
#' @param .verbose logical indicator for verbose output, defaults to FALSE
#'
#' @return a \code{data.frame} with cols pr_auc and (if applicable)
#'   ci_lower, ci_upper and additional stratification variables
#' @export
#'
#' @seealso compute_set_retrieval_scores compute_pr_auc_from_curve
#'
#' @examples
#' #' library(ggplot2)
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
#'   "C", "f", 0.1, 3,
#'   "C", "c", 0.2, 1,
#'   "C", "e", 0.2, 1
#' )
#'
#' auc <- compute_pr_auc(gold, pred, mode = "doc-avg")
compute_pr_auc <- function(gold_standard, predicted,
                           doc_strata = NULL,
                           label_dict = NULL,
                           mode = "doc-avg",
                           steps = 100,
                           limit_range = NA_real_,
                           compute_bootstrap_ci = FALSE,
                           n_bt = 10L,
                           seed = NULL,
                           graded_relevance = FALSE,
                           rename_graded_metrics = FALSE,
                           propensity_scored = FALSE,
                           label_distribution = NULL,
                           cost_fp_constant = NULL,
                           .ignore_relevance_warning = FALSE,
                           .verbose = FALSE,
                           .progress = FALSE) {


  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))
  stopifnot(is.logical(compute_bootstrap_ci))
  stopifnot(is.logical(.progress))
  stopifnot(is.integer(n_bt))
  stopifnot(is.numeric(limit_range))
  if (!all(is.na(limit_range)))
    stopifnot(all(limit_range>=1L))


  if (!all(is.na(limit_range)) && !"rank" %in% colnames(predicted)) {
    predicted <- dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(predicted, .data$doc_id),
        rank = dplyr::min_rank(-.data$score)
      )
    )
  }

  if (mode == "subj-avg" & compute_bootstrap_ci == TRUE) {
    stop("Confidence intervals for pr-auc in subj-avg-mode are not supported yet")
  }

  if (!is.null(doc_strata)) {
    stopifnot(doc_strata %in% colnames(gold_standard))
  }

  if (compute_bootstrap_ci == FALSE) {
    if (.verbose)
      message("Computing pr-curve")

    pr_curve <- compute_pr_curve(
      gold_standard = gold_standard,
      predicted = predicted,
      doc_strata = doc_strata,
      label_dict = label_dict,
      mode = mode,
      steps = steps,
      limit_range = limit_range,
      graded_relevance = graded_relevance,
      propensity_scored = propensity_scored,
      label_distribution = label_distribution,
      cost_fp_constant = cost_fp_constant,
      .ignore_relevance_warning = .ignore_relevance_warning,
      .verbose = .verbose,
      .progress = .progress
      )
    if (.verbose)
      message("Computing pr-auc from pr-curve")
    remaining_groupvars <- setdiff(
      set_grouping_var(mode, doc_strata, label_dict),
      c("doc_id", "label_id", "searchspace_id")
      )
    pr_auc <- compute_pr_auc_from_curve(pr_curve, remaining_groupvars)
  } else {

    grouping_var <- set_grouping_var(mode, doc_strata, label_dict)

    base_compare <- create_comparison(
      gold_standard, predicted,
      doc_strata = doc_strata,
      label_dict = label_dict,
      graded_relevance = graded_relevance,
      propensity_scored = propensity_scored,
      label_distribution = label_distribution,
      .ignore_relevance_warning = .ignore_relevance_warning
    )

    if (propensity_scored & !is.null(cost_fp_constant))
      cost_fp_processed <- process_cost_fp(cost_fp_constant, base_compare)
    else
      cost_fp_processed <- NULL

    ps_flags <- set_ps_flags(mode, propensity_scored)

    searchspace = tidyr::expand_grid(
      thresholds = seq(0, 1, by = 1/steps),
      limits = limit_range
    )

    get_intermed_res_per_searchspace_id <- function(
      threshold,
      limit,
      base_compare,
      grouping_var
    ) {
      compare_all_thrsld <- apply_threshold(
        base_compare = base_compare,
        threshold = threshold,
        limit = limit
      )
      res <- compute_intermediate_results(
        gold_vs_pred = compare_all_thrsld,
        grouping_var = grouping_var,
        propensity_scored = ps_flags$intermed,
        cost_fp = cost_fp_processed
      )

      res$results_table
    }

  # glue together data.frames with different searchspace_id
    if (.verbose)
      message("Computing intermediate results for all thresholds and limits")

    intermed_res_all_thrsld <- list()
    intermed_res_all_thrsld[["results_table"]] <- furrr::future_map2_dfr(
      .x = searchspace$thresholds,
      .y = searchspace$limits,
      .f = get_intermed_res_per_searchspace_id,
      .id = "searchspace_id",
      base_compare = base_compare,
      grouping_var = grouping_var,
      .progress = .progress
    )
    grouping_var_w_thrsld <- c("searchspace_id", grouping_var)
    # we have to set the grouping structure explicitly, as this was lost
    # during parallel computation
    intermed_res_all_thrsld[["grouping_var"]] <- grouping_var_w_thrsld

    smry_grouping_var <- setdiff(grouping_var, c("doc_id", "label_id"))

    if (.verbose)
      message("Computing bootstrap confidence intervals")

    boot_results <- generate_pr_auc_replica(
      intermed_res_all_thrsld,
      n_bt = n_bt,
      seed = seed,
      propensity_scored = ps_flags$summarise,
      .progress = .progress
    )

    # take the original as THE result
    pr_auc <- dplyr::select(
      dplyr::filter(boot_results, .data$boot_replicate == "Apparent"),
      -"boot_replicate"
    )

    # compute the confidence intervals as quantiles of the value-distribution
    # coming from the boot-strap copies
    # Note: this has to respect the desired grouping structure for the various
    # stratification variables
    boot_results_grpd <- dplyr::group_by(boot_results,
                                         !!!rlang::syms(smry_grouping_var))
    boot_ci <- dplyr::summarise(
      boot_results_grpd,
      ci_lower = stats::quantile(x = .data$pr_auc, probs = 0.025, na.rm = TRUE),
      ci_upper = stats::quantile(x = .data$pr_auc, probs = 0.975, na.rm = TRUE))

    if (purrr::is_empty(smry_grouping_var)) {
      pr_auc <- cbind(pr_auc, boot_ci)
    } else {
      pr_auc <- dplyr::left_join(
        x = pr_auc,
        y = boot_ci,
        by = smry_grouping_var
      )
    }

  }

  if (graded_relevance & rename_graded_metrics) {
    pr_auc <- dplyr::rename(
      pr_auc,
      g_pr_auc = .data$pr_auc
    )
  }

  pr_auc

}

#' Compute bootstrap replica of pr-auc
#'
#' Helper function whcih performs the major bootstrap operation and wraps the
#' repeated application of \code{summarise_intermediate_results} and
#' \code{compute_pr_auc_from_curve} for each bootstrap run
#'
#' @param intermed_res_all_thrsld intermediate results as produced by
#'   \code{compute_intermediate_results}, with a column \code{"searchspace_id"}
#'   as grouping variable
#' @param seed set seed for reproducibility of bootstrap run
#' @param n_bt integer number of bootstrap-replica to draw
#' @param .progress show progress bar
#' @param propensity_scored as in `compute_pr_auc`
#'
#' @return \code{data.frame} with cols \code{c("boot_replicate", "pr_auc")}
generate_pr_auc_replica <- function(
    intermed_res_all_thrsld,
    seed, n_bt,
    propensity_scored,
    .progress) {
  doc_id_list <- dplyr::distinct(
    intermed_res_all_thrsld$results_table,
    .data$doc_id)
  # core resampling is done by rsample library:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  boot <- rsample::bootstraps(data = doc_id_list, times = n_bt, apparent = TRUE)
  boot_dfs <- stats::setNames(boot$splits, boot$id)
  boot_dfs <- purrr::map(boot_dfs, as.data.frame)

  # set the size that variables shared between the parallel instances may have
  # 5000*1024^2 = 5242880000
  options(future.globals.maxSize = 5242880000)
  # apply wrapper to each of the bootstrap replica
  # note: a call to furrr attaches purrr
  boot_results <- furrr::future_map_dfr(
    boot_dfs,
    .f = boot_worker_fn,
    .progress = .progress,
    .id = "boot_replicate",
    .options = furrr::furrr_options(seed = seed),
    intermed_res = intermed_res_all_thrsld,
    propensity_scored = propensity_scored
    )
}

#' A wrapper for use within bootstrap computation of auc
#' which covers the repeated application of
#'   1. join with resampled doc_ids
#'   2. summarise_intermediate_results
#'   3. post_processing_of_curve data
#'   4. auc computation
#'
#'
#' @param sampled_id_list doc_ids of the examples drawn in each bootstrap
#'   iteration
#' @param intermed_res intermediate results as produced by
#'   \code{compute_intermediate_results}, with a column \code{"searchspace_id"}
#'   as grouping variable
#' @param propensity_scored as in `compute_pr_auc`
#'
#' @return  a \code{data.frame} with col pr_auc and potential grouping_vars
boot_worker_fn <- function(sampled_id_list, intermed_res, propensity_scored) {
  # 1. join with resampled doc_ids
  intermed_resampled <- dplyr::inner_join(
    x = intermed_res$results_table,
    y = sampled_id_list, relationship = "many-to-many",
    by = "doc_id")

  # 2. summarise_intermediate_results on resampled docs
  pr_curve_data <- summarise_intermediate_results(
    list(
      results_table = intermed_resampled,
      grouping_var = intermed_res$grouping_var
    ),
    propensity_scored = propensity_scored
  )

  # 3. post_processing_of_curve data
  pr_curve_data_reshaped <- pr_curve_post_processing(pr_curve_data)

  # 4. auc computation
  compute_pr_auc_from_curve(
    pr_curve_data = pr_curve_data_reshaped,
    grouping_vars = setdiff(
      intermed_res$grouping_var,
      c("doc_id", "label_id", "searchspace_id"))
  )
}
