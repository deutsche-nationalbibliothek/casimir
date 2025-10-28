#' Compute multi-label metrics
#'
#' Compute multi-label metrics precision, recall, F1 and R-precision for subject
#' indexing results.
#'
#' @param predicted Multi-label prediction results. Expects a data.frame with
#'   columns \code{"label_id", "doc_id"}.
#' @param gold_standard Expects a data.frame with columns \code{"label_id",
#'   "doc_id"}.
#' @param k An integer limit on the number of predictions per document to
#'   consider. Requires a column \code{"score"} in input \code{predicted}.
#' @param mode One of the following aggregation modes: \code{"doc-avg",
#'   "subj-avg", "micro"}.
#' @param compute_bootstrap_ci A logical indicator for computing bootstrap CIs.
#' @param n_bt An integer number of resamples to be used for bootstrapping.
#' @param doc_groups A two-column data.frame with a column \code{"doc_id"} and a
#'   second column defining groups of documents to stratify results by. It is
#'   recommended that groups are of type factor so that levels are not
#'   implicitly dropped during bootstrap replications.
#' @param label_groups A two-column data.frame with a column \code{"label_id"}
#'   and a second column defining groups of labels to stratify results by.
#'   Results in each stratum will restrict gold standard and predictions to the
#'   specified label groups as if the vocabulary was consisting of the label
#'   group only. All modes \code{"doc-avg", "subj-avg", "micro"} are supported
#'   within label strata. Nevertheless, mixing \code{mode = "doc-avg"} with
#'   fine-grained label strata can result in many missing values on
#'   document-level results. Also rank-based thresholding (e.g. top 5) will
#'   result in inhomogeneous numbers of labels per document within the defined
#'   label strata. \code{mode = "subj-avg"} or \code{mode = "micro"} can be more
#'   appropriate in these circumstances.
#' @param graded_relevance A logical indicator for graded relevance. Defaults to
#'   \code{FALSE} for binary relevance. If set to \code{TRUE}, the
#'   \code{predicted} data.frame should contain a numeric column
#'   \code{"relevance"} with values in the range of \eqn{[0, 1]}.
#' @param rename_metrics If set to \code{TRUE}, the metric names in the output
#'   will be renamed if:
#'   \describe{
#'     \item{\code{graded_relevance == TRUE}}{prefixed with \emph{"g-"} to
#'     indicate that metrics are computed with graded relevance.}
#'     \item{\code{propensity_scored == TRUE}}{prefixed with \emph{"ps-"} to
#'     indicate that metrics are computed with propensity scores.}
#'     \item{\code{!is.null(k)}}{suffixed with \emph{"@@k"} to indicate
#'     that metrics are limited to top k predictions.}
#'    }
#'
#' @param seed Pass a seed to make bootstrap replication reproducible.
#' @param propensity_scored Logical, whether to use propensity scores as
#'   weights.
#' @param label_distribution Expects a data.frame with columns \code{"label_id",
#'   "label_freq", "n_docs"}. \code{label_freq} corresponds to the number of
#'   occurences a label has in the gold standard. \code{n_docs} corresponds to
#'   the total number of documents in the gold standard.
#' @param cost_fp_constant Constant cost assigned to false positives.
#'   \code{cost_fp_constant} must be a numeric value > 0 or one of 'max', 'min',
#'   'mean' (computed with reference to the \code{gold_standard} label
#'   distribution). Defaults to NULL, i.e. label weights are applied to false
#'   positives in the same way as to false negatives and true positives.
#' @inheritParams option_params
#'
#' @return A data.frame with columns \code{"metric", "mode", "value", "support"}
#'   and optional grouping variables supplied in \code{doc_groups} or
#'   \code{label_groups}. Here, \code{support} is defined for each \code{mode}
#'   as:
#' \describe{
#'   \item{\code{mode == "doc-avg"}}{The number of tested documents.}
#'   \item{\code{mode == "subj-avg"}}{The number of labels contributing to the
#'     subj-average.}
#'   \item{\code{mode == "micro"}}{The number of doc-label pairs contributing
#'     to the denominator of the respective metric, e.g. \eqn{tp + fp} for
#'     precision, \eqn{tp + fn} for recall, \eqn{tp + (fp + fn)/2} for F1 and
#'     \eqn{min(tp + fp, tp + fn)} for R-precision.}
#' }
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(casimir)
#' library(furrr)
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
#'   "C", "f",
#' )
#'
#' pred <- tibble::tribble(
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "d",
#'   "A", "f",
#'   "B", "a",
#'   "B", "e",
#'   "C", "f",
#' )
#'
#' plan(sequential) # or whatever resources you have
#'
#' a <- compute_set_retrieval_scores(
#'   pred, gold,
#'   mode = "doc-avg",
#'   compute_bootstrap_ci = TRUE,
#'   n_bt = 100L
#' )
#'
#' ggplot(a, aes(x = metric, y = value)) +
#'   geom_bar(stat = "identity") +
#'   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
#'   facet_wrap(vars(metric), scales = "free")
#'
#' # example with graded relevance
#' pred_w_relevance <- tibble::tribble(
#'   ~doc_id, ~label_id, ~relevance,
#'   "A", "a", 1.0,
#'   "A", "d", 0.0,
#'   "A", "f", 0.0,
#'   "B", "a", 1.0,
#'   "B", "e", 1 / 3,
#'   "C", "f", 1.0,
#' )
#'
#' b <- compute_set_retrieval_scores(
#'   pred_w_relevance, gold,
#'   mode = "doc-avg",
#'   graded_relevance = TRUE
#' )
#'
compute_set_retrieval_scores <- function(
    predicted,
    gold_standard,
    k = NULL,
    mode = "doc-avg",
    compute_bootstrap_ci = FALSE,
    n_bt = 10L,
    doc_groups = NULL,
    label_groups = NULL,
    graded_relevance = FALSE,
    rename_metrics = FALSE,
    seed = NULL,
    propensity_scored = FALSE,
    label_distribution = NULL,
    cost_fp_constant = NULL,
    replace_zero_division_with = options::opt("replace_zero_division_with"),
    drop_empty_groups = options::opt("drop_empty_groups"),
    ignore_inconsistencies = options::opt("ignore_inconsistencies"),
    verbose = options::opt("verbose"),
    progress = options::opt("progress")) {
  stopifnot(mode %in% c("doc-avg", "subj-avg", "micro"))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))
  stopifnot(is.logical(compute_bootstrap_ci))

  if (!is.null(k)) {
    if (!("rank" %in% colnames(predicted))) {
      predicted <- create_rank_col(predicted)
    }
    predicted <- collapse::fsubset(predicted, rank <= k)
  }

  compare <- create_comparison(
    predicted = predicted,
    gold_standard = gold_standard,
    doc_groups = doc_groups,
    label_groups = label_groups,
    graded_relevance = graded_relevance,
    propensity_scored = propensity_scored,
    label_distribution = label_distribution,
    ignore_inconsistencies = ignore_inconsistencies
  )

  if (propensity_scored && !is.null(cost_fp_constant)) {
    cost_fp_processed <- process_cost_fp(cost_fp_constant, compare)
  } else {
    cost_fp_processed <- NULL
  }

  ps_flags <- set_ps_flags(mode, propensity_scored)

  grouping_var <- set_grouping_var(mode, doc_groups, label_groups)

  # give warnings if additional stratification variables are not factors
  test_factor <- function(var) {
    if (!is.factor(compare[[var]])) {
      warning(paste(
        var,
        "is not a factor variable.",
        "Some levels may be lost in bootstrap replications"
      ))
    }
  }
  if (compute_bootstrap_ci) {
    purrr::map(setdiff(grouping_var, c("doc_id", "label_id")),
      .f = test_factor
    )
  }

  if (compute_bootstrap_ci == FALSE) {
    boot_results <- NULL

    if (verbose) {
      message("Computing intermediate results...")
    }

    intermediate_results <- compute_intermediate_results(
      compare,
      grouping_var,
      propensity_scored = ps_flags$intermed,
      cost_fp = cost_fp_processed,
      drop_empty_groups = drop_empty_groups
    )

    if (verbose) {
      message("Summarizing intermediate results...")
    }

    results <- summarise_intermediate_results(
      intermediate_results,
      propensity_scored = ps_flags$summarise,
      label_distribution = label_distribution,
      replace_zero_division_with = replace_zero_division_with,
      set = TRUE
    )
  } else {
    # generate n_bt copies of results, plus one original

    if (verbose) {
      message("Computing bootstrap confidence intervals...")
    }

    boot_results <- generate_replicate_results(
      base_compare = compare,
      n_bt = n_bt,
      grouping_var = grouping_var,
      seed = seed,
      ps_flags = ps_flags,
      label_distribution = label_distribution,
      cost_fp = cost_fp_processed,
      replace_zero_division_with = replace_zero_division_with,
      drop_empty_groups = drop_empty_groups,
      progress = progress
    )

    # take the original as THE result
    results <- dplyr::select(
      dplyr::filter(boot_results, .data$boot_replicate == "Apparent"),
      -"boot_replicate"
    )

    # compute the confidence intervals as quantiles of the value distribution
    # coming from the bootstrap copies
    # note: this has to respect the desired grouping structure for the various
    # stratification variables
    smry_grouping_var <- c(
      "metric",
      setdiff(grouping_var, c("doc_id", "label_id"))
    )
    boot_results_grpd <- dplyr::group_by(
      boot_results,
      !!!rlang::syms(smry_grouping_var)
    )
    boot_ci <- dplyr::summarise(
      boot_results_grpd,
      ci_lower = stats::quantile(x = .data$value, probs = 0.025, na.rm = TRUE),
      ci_upper = stats::quantile(x = .data$value, probs = 0.975, na.rm = TRUE)
    )

    results <- dplyr::left_join(
      x = results,
      y = boot_ci,
      by = smry_grouping_var
    )

    # detect values out of CI boundaries
    skewed_ci <- results |>
      dplyr::summarise(
        ci_lower_too_high = sum(.data$value < .data$ci_lower, na.rm = TRUE),
        ci_upper_too_low = sum(.data$value > .data$ci_upper, na.rm = TRUE)
      )

    if (skewed_ci[["ci_lower_too_high"]] > 0) {
      warning(paste(
        "CI computation produced skewed CI intervals",
        "with metric value below ci_lower for ",
        skewed_ci[["ci_lower_too_high"]],
        "metric results. Extending CI to include metric value.",
        "This is known to happen for subj-avg metrics with highly skewed",
        "label distribution."
      ))
    }

    if (skewed_ci[["ci_upper_too_low"]] > 0) {
      warning(paste(
        "CI computation produced skewed CI intervals",
        "with metric value above ci_upper for",
        skewed_ci[["ci_upper_too_low"]],
        "metric results. Extending CI to include metric value.",
        "This is known to happen for subj-avg metrics with highly skewed",
        "label distribution."
      ))
    }

    results <- dplyr::mutate(
      results,
      ci_lower = pmin(.data$ci_lower, .data$value),
      ci_upper = pmax(.data$ci_upper, .data$value)
    )

    # rearrange cols so that support is last col
    results <- dplyr::select(
      results, setdiff(colnames(results), "support"),
      "support"
    )
  }

  # add column that indicates the aggregation mode
  results <- dplyr::mutate(results, mode = mode, .after = "metric")

  if (rename_metrics) {
    results <- rename_metrics(
      results,
      graded_relevance = graded_relevance,
      propensity_scored = propensity_scored,
      k = k)
  }

  results
}

#' @describeIn compute_set_retrieval_scores Variant with internal usage of
#'  dplyr rather than collapse library. Tends to be slower, but more stable.
compute_set_retrieval_scores_dplyr <- function( # nolint styler: off
    predicted,
    gold_standard,
    k = NULL,
    mode = "doc-avg",
    compute_bootstrap_ci = FALSE,
    n_bt = 10L,
    doc_groups = NULL,
    label_groups = NULL,
    graded_relevance = FALSE,
    rename_metrics = FALSE,
    seed = NULL,
    propensity_scored = FALSE,
    label_distribution = NULL,
    cost_fp_constant = NULL,
    ignore_inconsistencies = FALSE,
    verbose = FALSE,
    progress = FALSE) {
  stopifnot(is.logical(compute_bootstrap_ci))

  if (!is.null(k)) {
    if (!("rank" %in% colnames(predicted))) {
      predicted <- create_rank_col_dplyr(predicted)
    }
    predicted <- dplyr::filter(predicted, .data$rank <= k)
  }

  if (propensity_scored) {
    stopifnot(!is.null(label_distribution))
    stopifnot(all(
      c("label_id", "label_freq") %in% colnames(label_distribution)
    ))
  }

  compare <- create_comparison(
    predicted = predicted,
    gold_standard = gold_standard,
    doc_groups = doc_groups,
    label_groups = label_groups,
    graded_relevance = graded_relevance,
    ignore_inconsistencies = ignore_inconsistencies
  )

  cost_fp_processed <- NULL

  ps_flags <- set_ps_flags(mode, propensity_scored)

  if (propensity_scored) {
    label_weights <- compute_propensity_scores(label_distribution)

    rlang::try_fetch(
      {
        compare <- dplyr::inner_join(
          x = compare,
          y = label_weights,
          by = c("label_id"),
          relationship = "many-to-one",
          unmatched = c("error", "drop") # i.e. every label in compare must have
          # exactly one weight but the label_weights
          # may contain more labels than compare
        )
      },
      error = function(cnd) {
        rlang::abort(
          paste0(
            "Label distribution does not match gold_standard ",
            "or predicted labels."
          ),
          parent = cnd
        )
      }
    )

    cost_fp_processed <- process_cost_fp(cost_fp_constant, compare)
  }

  grouping_var <- set_grouping_var(mode, doc_groups, label_groups)

  # give warnings if additional stratification variables are not factors
  test_factor <- function(var) {
    if (!is.factor(compare[[var]])) {
      warning(paste(
        var,
        "is not a factor variable.",
        "Some levels may be lost in bootstrap replications"
      ))
    }
  }
  if (compute_bootstrap_ci) {
    purrr::map(setdiff(grouping_var, c("doc_id", "label_id")),
      .f = test_factor
    )
  }

  if (compute_bootstrap_ci == FALSE) {
    boot_results <- NULL

    intermediate_results <- compute_intermediate_results_dplyr(
      compare,
      rlang::syms(grouping_var),
      propensity_scored = ps_flags$intermed,
      cost_fp = cost_fp_processed
    )
    results <- summarise_intermediate_results_dplyr(
      intermediate_results,
      propensity_scored = ps_flags$summarise,
      label_distribution = label_distribution
    )
  } else {
    # generate n_bt copies of results, plus one original
    if (verbose) {
      message("Computing bootstrap confidence intervals...")
    }

    boot_results <- generate_replicate_results_dplyr(
      base_compare = compare,
      n_bt = n_bt,
      grouping_var = grouping_var,
      seed = seed,
      ps_flags = ps_flags,
      label_distribution = label_distribution,
      cost_fp = cost_fp_processed,
      progress = progress
    )

    # take the original as THE result
    results <- dplyr::select(
      dplyr::filter(boot_results, .data$boot_replicate == "Apparent"),
      -"boot_replicate"
    )

    # compute the confidence intervals as quantiles of the value distribution
    # coming from the bootstrap copies
    # note: this has to respect the desired grouping structure for the various
    # stratification variables
    smry_grouping_var <- c(
      "metric",
      setdiff(grouping_var, c("doc_id", "label_id"))
    )
    boot_results_grpd <- dplyr::group_by(
      boot_results,
      !!!rlang::syms(smry_grouping_var)
    )
    boot_ci <- dplyr::summarise(
      boot_results_grpd,
      ci_lower = stats::quantile(x = .data$value, probs = 0.025, na.rm = TRUE),
      ci_upper = stats::quantile(x = .data$value, probs = 0.975, na.rm = TRUE)
    )

    results <- dplyr::left_join(
      x = results,
      y = boot_ci,
      by = smry_grouping_var
    )

    # rearrange columns so that support is last col
    results <- dplyr::select(
      results, setdiff(colnames(results), "support"),
      "support"
    )
  }

  # add column that indicates the aggregation mode
  results <- dplyr::mutate(results, mode = mode, .after = "metric")

  if (rename_metrics) {
    results <- rename_metrics(
      results,
      graded_relevance = graded_relevance,
      propensity_scored = propensity_scored,
      k = k)
  }

  results
}
