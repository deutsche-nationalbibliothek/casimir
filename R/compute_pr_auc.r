#' Compute Area under precision recall curve with support for bootstrap based
#' confidence intervals and different stratification and aggregation modes
#' for the underlying precision and recall aggregation
#' Precision is calculated as the best value at a given level of recall for
#' all possible thresholds on score and limits on rank. In essence,
#' compute_pr_auc performs a two dimensional optimisation over thresholds and
#' limits applying both threshold-based cutoff as well as rank-based cutoff.
#'
#' @param gold_standard expects \code{data.frame} with cols
#'   \emph{"label_id", "doc_id", "score"}
#' @param predicted multi-label prediction results. expects \code{data.frame}
#'   with cols \emph{"label_id", "doc_id", "score"}
#' @param mode aggregation mode: \emph{"doc-avg", "subj-avg", "micro"}
#' @param compute_bootstrap_ci logical indicator for computing bootstrap CIs
#' @param n_bt an integer number of resamples to undergo in bootstrapping
#' @param doc_strata a column that exists in either gold_standard or predicted,
#'   that results should be grouped by, e.g. strata of document-space.
#'   \code{doc_strata} is recommended to be of type factor, so that levels are
#'   not implicitly dropped during bootstrap replications
#' @param label_dict two-column \code{data.frame} with col \emph{"label_id"}
#'   and a second column that defines
#'   groups of labels to stratify results by. Results in each stratum
#'   will restrict gold_standard and predictions to the specified label-groups,
#'   as if the vocabulary was consisting of the label group only.
#'   All modes \code{c("doc-avg", "subj-avg", "micro") } are supported within
#'   label-strata. Nevertheless, mixing
#'   \code{mode = "doc-avg"} with fine-grained
#'   label_strata can result in many missing values on document-level results.
#'   Also rank-based thresholding (e.g. Top-5) will result in inhomogeneous
#'   number of labels per documents within the defined label-strata.
#'   \code{mode = "subj-avg"} or \code{mode = "micro"} can be more appropriate
#'   in these circumstances.
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
#' @param cost_fp_constant Constant cost assigned to false positives.
#'   cost_fp_constant must be
#'   a numeric value > 0 or one of 'max', 'min', 'mean' (computed with reference
#'   to the \code{gold_standard} label distribution). The default is NULL, i.e.
#'   label weights are appplied to false positices as to false negatives and
#'   true positives.
#' @inheritParams option_params
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
compute_pr_auc <- function(
  gold_standard, predicted,
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
  ignore_inconsistencies = options::opt("ignore_inconsistencies"),
  verbose = options::opt("verbose"),
  progress = options::opt("progress")
) {


  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))
  stopifnot(is.logical(compute_bootstrap_ci))
  stopifnot(is.logical(progress))
  stopifnot(is.integer(n_bt))
  stopifnot(is.numeric(limit_range))
  if (!all(is.na(limit_range)))
    stopifnot(all(limit_range >= 1L))


  if (!all(is.na(limit_range)) && !"rank" %in% colnames(predicted)) {
    predicted <- dplyr::ungroup(
      dplyr::mutate(
        dplyr::group_by(predicted, .data$doc_id),
        rank = dplyr::min_rank(-.data$score)
      )
    )
  }

  if (mode == "subj-avg" && compute_bootstrap_ci == TRUE) {
    stop(paste(
      "Confidence intervals for pr-auc in",
      "subj-avg-mode are not supported yet"
    ))
  }

  if (!is.null(doc_strata)) {
    stopifnot(doc_strata %in% colnames(gold_standard))
  }

  if (compute_bootstrap_ci == FALSE) {
    if (verbose)
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
      ignore_inconsistencies = ignore_inconsistencies,
      verbose = verbose,
      progress = progress
    )
    if (verbose)
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

    get_intermed_res_per_searchspace_id <- function( # nolint
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
    if (verbose)
      message("Computing intermediate results for all thresholds and limits")

    intermed_res_all_thrsld <- list()
    intermed_res_all_thrsld[["results_table"]] <- furrr::future_map2_dfr(
      .x = searchspace$thresholds,
      .y = searchspace$limits,
      .f = get_intermed_res_per_searchspace_id,
      .id = "searchspace_id",
      base_compare = base_compare,
      grouping_var = grouping_var,
      .progress = progress
    )
    grouping_var_w_thrsld <- c("searchspace_id", grouping_var)
    # we have to set the grouping structure explicitly, as this was lost
    # during parallel computation
    intermed_res_all_thrsld[["grouping_var"]] <- grouping_var_w_thrsld

    smry_grouping_var <- setdiff(grouping_var, c("doc_id", "label_id"))

    if (verbose)
      message("Computing bootstrap confidence intervals")

    boot_results <- generate_pr_auc_replica(
      intermed_res_all_thrsld,
      n_bt = n_bt,
      seed = seed,
      propensity_scored = ps_flags$summarise,
      progress = progress
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
      ci_upper = stats::quantile(x = .data$pr_auc, probs = 0.975, na.rm = TRUE)
    )

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

  if (graded_relevance && rename_graded_metrics) {
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
#' @param propensity_scored as in `compute_pr_auc`
#' @inheritParams option_params
#'
#' @return \code{data.frame} with cols \code{c("boot_replicate", "pr_auc")}
generate_pr_auc_replica <- function(
  intermed_res_all_thrsld,
  seed, n_bt,
  propensity_scored,
  progress = options::opt("progress")
) {
  doc_id_list <- dplyr::distinct(
    intermed_res_all_thrsld$results_table,
    .data$doc_id
  )
  # core resampling is done by rsample library:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  boot <- rsample::bootstraps(data = doc_id_list, times = n_bt, apparent = TRUE)
  boot_dfs <- stats::setNames(boot$splits, boot$id)
  boot_dfs <- purrr::map(boot_dfs, as.data.frame)

  # set the size that variables shared between the parallel instances may have
  # 5000*1024^2 = 5242880000 # nolint
  base::options(future.globals.maxSize = 5242880000)
  # apply wrapper to each of the bootstrap replica
  # note: a call to furrr attaches purrr
  boot_results <- furrr::future_map_dfr(
    boot_dfs,
    .f = boot_worker_fn,
    .progress = progress,
    .id = "boot_replicate",
    .options = furrr::furrr_options(seed = seed),
    intermed_res = intermed_res_all_thrsld,
    propensity_scored = propensity_scored
  )

  boot_results
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
    by = "doc_id"
  )

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
      c("doc_id", "label_id", "searchspace_id")
    )
  )
}
