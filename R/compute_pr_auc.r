#' Compute Area under precision recall curve with support for bootstrap based
#' confidence intervals and different stratification and aggregation modes
#' for the underlying precision and recall aggregation
#' Precision is calculated as the best value at a given level of recall for
#' all possible thresholds on score and limits on rank. In essence,
#' compute_pr_auc performs a two dimensional optimisation over thresholds and
#' limits applying both threshold-based cutoff as well as rank-based cutoff.
#'
#' @param predicted multi-label prediction results. expects \code{data.frame}
#'   with cols \emph{"label_id", "doc_id", "score"}
#' @inheritParams compute_set_retrieval_scores
#' @inheritParams compute_pr_curve
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
#' auc <- compute_pr_auc(gold, pred, mode = "doc-avg")
compute_pr_auc <- function(
    gold_standard, predicted,
    doc_groups = NULL,
    label_groups = NULL,
    mode = "doc-avg",
    steps = 100,
    thresholds = NULL,
    limit_range = NA_real_,
    compute_bootstrap_ci = FALSE,
    n_bt = 10L,
    seed = NULL,
    graded_relevance = FALSE,
    rename_graded_metrics = FALSE,
    propensity_scored = FALSE,
    label_distribution = NULL,
    cost_fp_constant = NULL,
    replace_zero_division_with = options::opt("replace_zero_division_with"),
    drop_empty_groups = options::opt("drop_empty_groups"),
    ignore_inconsistencies = options::opt("ignore_inconsistencies"),
    verbose = options::opt("verbose"),
    progress = options::opt("progress")) {
  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))
  stopifnot(is.logical(compute_bootstrap_ci))
  stopifnot(is.logical(progress))
  stopifnot(is.integer(n_bt))
  stopifnot(is.numeric(limit_range))
  if (!all(is.na(limit_range))) {
    stopifnot(all(limit_range >= 1L))
  }


  if (!all(is.na(limit_range)) && !"rank" %in% colnames(predicted)) {
    predicted <- create_rank_col(predicted)
  }

  if (mode == "subj-avg" && compute_bootstrap_ci == TRUE) {
    stop(paste(
      "Confidence intervals for pr-auc in",
      "subj-avg-mode are not supported yet"
    ))
  }

  if (compute_bootstrap_ci == FALSE) {
    if (verbose) {
      message("Computing pr-curve")
    }

    pr_curve <- compute_pr_curve(
      gold_standard = gold_standard,
      predicted = predicted,
      doc_groups = doc_groups,
      label_groups = label_groups,
      mode = mode,
      steps = steps,
      thresholds = thresholds,
      limit_range = limit_range,
      graded_relevance = graded_relevance,
      propensity_scored = propensity_scored,
      label_distribution = label_distribution,
      cost_fp_constant = cost_fp_constant,
      replace_zero_division_with = replace_zero_division_with,
      drop_empty_groups = drop_empty_groups,
      ignore_inconsistencies = ignore_inconsistencies,
      verbose = verbose,
      progress = progress
    )
    if (verbose) {
      message("Computing pr-auc from pr-curve")
    }
    remaining_groupvars <- setdiff(
      set_grouping_var(mode, doc_groups, label_groups),
      c("doc_id", "label_id", "searchspace_id")
    )
    pr_auc <- compute_pr_auc_from_curve(
      pr_curve,
      remaining_groupvars,
      drop_empty_groups = drop_empty_groups
    )
  } else {
    grouping_var <- set_grouping_var(mode, doc_groups, label_groups)

    base_compare <- create_comparison(
      gold_standard, predicted,
      doc_groups = doc_groups,
      label_groups = label_groups,
      graded_relevance = graded_relevance,
      propensity_scored = propensity_scored,
      label_distribution = label_distribution,
      ignore_inconsistencies = ignore_inconsistencies
    )

    if (propensity_scored && !is.null(cost_fp_constant)) {
      cost_fp_processed <- process_cost_fp(cost_fp_constant, base_compare)
    } else {
      cost_fp_processed <- NULL
    }

    ps_flags <- set_ps_flags(mode, propensity_scored)

    if (is.null(thresholds)) {
      # condition on true positives
      true_positives <- dplyr::filter(
        base_compare, .data$gold & .data$suggested
      )

      thresholds <- unique(stats::quantile(
        true_positives[["score"]],
        probs = seq(0, 1, 1 / steps),
        type = 1, na.rm = TRUE
      ))
    }

    searchspace <- tidyr::expand_grid(
      thresholds = thresholds,
      limits = limit_range
    )

    get_intermed_res_per_searchspace_id <- function(threshold, # nolint
                                                    limit,
                                                    base_compare,
                                                    grouping_var,
                                                    drop_empty_groups) {
      compare_all_thrsld <- apply_threshold(
        base_compare = base_compare,
        threshold = threshold,
        limit = limit
      )
      res <- compute_intermediate_results(
        gold_vs_pred = compare_all_thrsld,
        grouping_var = grouping_var,
        propensity_scored = ps_flags$intermed,
        cost_fp = cost_fp_processed,
        drop_empty_groups = drop_empty_groups
      )

      res$results_table
    }

    # glue together data.frames with different searchspace_id
    if (verbose) {
      message("Computing intermediate results for all thresholds and limits")
    }

    intermed_res_all_thrsld <- list()
    intermed_res_all_thrsld[["results_table"]] <- furrr::future_map2_dfr(
      .x = searchspace$thresholds,
      .y = searchspace$limits,
      .f = get_intermed_res_per_searchspace_id,
      .id = "searchspace_id",
      base_compare = base_compare,
      grouping_var = grouping_var,
      drop_empty_groups = drop_empty_groups,
      .progress = progress
    )
    grouping_var_w_thrsld <- c("searchspace_id", grouping_var)
    # we have to set the grouping structure explicitly, as this was lost
    # during parallel computation
    intermed_res_all_thrsld[["grouping_var"]] <- grouping_var_w_thrsld

    smry_grouping_var <- setdiff(grouping_var, c("doc_id", "label_id"))

    if (verbose) {
      message("Computing bootstrap confidence intervals")
    }

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
    boot_results_grpd <- dplyr::group_by(
      boot_results,
      !!!rlang::syms(smry_grouping_var),
      .drop = drop_empty_groups
    )
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
#' @inheritParams compute_pr_auc
#'
#' @return \code{data.frame} with cols \code{c("boot_replicate", "pr_auc")}
generate_pr_auc_replica <- function(
    intermed_res_all_thrsld,
    seed, n_bt,
    propensity_scored,
    replace_zero_division_with = options::opt("replace_zero_division_with"),
    progress = options::opt("progress")) {
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
  max_size <- getOption("future.globals.maxSize", 500 * 1024^2)
  obj_size <- as.numeric(utils::object.size(intermed_res_all_thrsld))
  if (obj_size > max_size) {
    warning(paste(
      "Shared object size for parallel computation in CI bootstrapping",
      "exceeds default (maxSize = ",
      max_size,
      ". Setting `future.globals.maxSize` to",
      obj_size * 1.1,
      ", locally. To avoid this warning try one of the following:
        * increase `future.globals.maxSize` globally
        * decrease `steps` or `limit_range`
        * disable CI computation"
    ))
    withr::local_options(list(future.globals.maxSize = obj_size * 1.1))
  }
  # apply wrapper to each of the bootstrap replica
  # note: a call to furrr attaches purrr
  boot_results <- furrr::future_map_dfr(
    boot_dfs,
    .f = boot_worker_fn,
    .progress = progress,
    .id = "boot_replicate",
    .options = furrr::furrr_options(seed = seed),
    intermed_res = intermed_res_all_thrsld,
    propensity_scored = propensity_scored,
    replace_zero_division_with = replace_zero_division_with
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
#' @inheritParams compute_pr_auc
#'
#' @return  a \code{data.frame} with col pr_auc and potential grouping_vars
boot_worker_fn <- function(sampled_id_list,
                           intermed_res,
                           propensity_scored,
                           replace_zero_division_with) {
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
    propensity_scored = propensity_scored,
    replace_zero_division_with = replace_zero_division_with,
    set = TRUE
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
