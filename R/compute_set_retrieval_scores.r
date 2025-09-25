#' Compute multi label metrics for gnd subject indexing results
#'
#' @param gold_standard expects \code{data.frame} with cols
#'   \emph{"label_id", "doc_id"}
#' @param predicted multi-label prediction results. expects \code{data.frame}
#'   with cols \emph{"label_id", "doc_id", "score"}
#' @param mode aggregation mode: \emph{"doc-avg", "subj-avg", "micro"}
#' @param compute_bootstrap_ci logical indicator for computing bootstrap CIs
#' @param n_bt an integer number of resamples to undergo in bootstrapping
#' @param doc_strata a column that exists gold_standard,
#'   that results should be grouped by, e.g. strata of document-space.
#'   \code{doc_strata} is recommended to be of type factor, so that levels are
#'   not implicitly dropped during bootstrap replications
#' @param label_dict two-column \code{data.frame} with col \emph{"label_id"}
#'   and a second column that defines groups of labels to stratify results by.
#'   Results in each stratum
#'   will restrict gold_standard and predictions to the specified label-groups,
#'   as if the vocabulary was consisting of the label group only.
#'   All modes \code{c("doc-avg", "subj-avg", "micro") } are supported within
#'   label-strata.
#'   Nevertheless, mixing \code{mode = "doc-avg"} with fine-grained
#'   label_strata can result in many missing values on document-level results.
#'   Also rank-based thresholding (e.g. Top-5) will result in inhomogeneous
#'   number of labels per documents within the defined label-strata.
#'   \code{mode = "subj-avg"} or \code{mode = "micro"} can be more appropriate
#'   in these circumstances.
#' @param graded_relevance logical indicator for graded relevance. Defaults to
#'  \code{FALSE} for binary relevance. If set to \code{TRUE}, the
#'  \code{predicted} data.frame should contain a numeric column
#'  \emph{"relevance"} with values in the range of \code{c(0, 1)}.
#' @param rename_graded_metrics if set to \code{TRUE}, the metric names in
#'   the output will be prefixed with \emph{"g-"} to indicate that metrics
#'   are computed with graded relevance.
#' @param .verbose logical indicator for verbose output, defaults to FALSE
#' @param .progress logical activating .progress bar in internal
#'   \pkg{furrr}-computation
#' @param .ignore_relevance_warning logical, if graded_relevance = FALSE, but
#'   column relevance is present in predicted, a warning can be silenced by
#'   setting .ignore_relevance_warning = TRUE
#' @param seed pass seed to make bootstrap replication reproducible
#' @param propensity_scored logical, whether to use propensity scores as weights
#' @param label_distribution expects \code{data.frame} with cols
#'   \emph{"label_id", "label_freq", "n_docs"}. \code{label_freq} corresonds to
#'   the number of occurences a label has in the gold_standard. \code{n_docs}
#'   corresponds to the total number of documents in the gold_standard.
#' @param cost_fp_constant Constant cost assigned to false positives.
#'   cost_fp_constant must be
#'   a numeric value > 0 or one of 'max', 'min', 'mean' (computed with reference
#'   to the \code{gold_standard} label distribution). The default is NULL, i.e.
#'   label weights are applied to false positives as to false negatives and
#'   true positives.
#'
#' @return a \code{data.frame} with cols
#'   \emph{"metric", "mode", "value", "support"}
#'    and optionally grouping
#'    variables supplied in doc_strata or label_dict. Here, \strong{support}
#'    is defined for each \emph{mode} as,
#' \describe{
#'   \item{\code{mode = "doc-avg"}}{the number of tested documents}
#'   \item{\code{mode = "subj-avg"}}{the number of labels contributing to the subj-average} # nolint
#'   \item{\code{mode = "micro"}}{the number of doc-label-pairs contributing
#'   to the denominator of the respective metric, e.g. tp + fp for precision,
#'   tp + fn for recall, tp + (fp + fn)/2 for f1 and min(tp + fp, tp + fn)
#'   for r-precision.}
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
#' gold, pred,
#' mode = "doc-avg",
#' compute_bootstrap_ci = TRUE,
#' n_bt = 100L)
#'
#'
#' ggplot(a, aes(x = metric, y = value)) +
#'   geom_bar(stat = "identity") +
#'   geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper)) +
#'   facet_wrap(vars(metric), scales = "free")
#'
#' # Example with graded relevance
#' pred_w_relevance <- tibble::tribble(
#'   ~doc_id, ~label_id, ~relevance,
#'   "A", "a", 1.0,
#'   "A", "d", 0.0,
#'   "A", "f", 0.0,
#'   "B", "a", 1.0,
#'   "B", "e", 1/3,
#'   "C", "f", 1.0,
#' )
#'
#' b <- compute_set_retrieval_scores(
#'   gold, pred_w_relevance,
#'   mode = "doc-avg",
#'   graded_relevance = TRUE
#' )
#'
#'
compute_set_retrieval_scores <- function(
    gold_standard,
    predicted,
    mode = "doc-avg",
    compute_bootstrap_ci = FALSE,
    n_bt = 10L,
    doc_strata = NULL,
    label_dict = NULL,
    graded_relevance = FALSE,
    rename_graded_metrics = FALSE,
    seed = NULL,
    propensity_scored = FALSE,
    label_distribution = NULL,
    cost_fp_constant = NULL,
    .ignore_relevance_warning = FALSE,
    .verbose = FALSE,
    .progress = FALSE) {

  stopifnot(mode %in% c("doc-avg", "subj-avg", "micro"))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))
  stopifnot(is.logical(compute_bootstrap_ci))
  if (!is.null(doc_strata)) {
    stopifnot(doc_strata %in% colnames(gold_standard))
  }

  compare <- create_comparison(
    gold_standard, predicted,
    doc_strata = doc_strata,
    label_dict = label_dict,
    graded_relevance = graded_relevance,
    propensity_scored = propensity_scored,
    label_distribution = label_distribution,
    .ignore_relevance_warning = .ignore_relevance_warning
  )

  if (propensity_scored && !is.null(cost_fp_constant))
    cost_fp_processed <- process_cost_fp(cost_fp_constant, compare)
  else
    cost_fp_processed <- NULL

  ps_flags <- set_ps_flags(mode, propensity_scored)

  grouping_var <- set_grouping_var(mode, doc_strata, label_dict)

  # give warnings, when additional stratification variables are not factors
  test_factor <- function(var) {
    if (!is.factor(compare[[var]]))
      warning(paste(
        var,
        "is not a factor variable.",
        "Some levels may be lost in bootstrap replications"
      ))
  }
  if (compute_bootstrap_ci) {
    purrr::map(setdiff(grouping_var, c("doc_id", "label_id")),
               .f = test_factor)
  }

  if (compute_bootstrap_ci == FALSE) {
    boot_results <- NULL


    if (.verbose)
      message("Computing intermediate results...")

    intermediate_results <- compute_intermediate_results(
      compare,
      grouping_var,
      propensity_scored = ps_flags$intermed,
      cost_fp = cost_fp_processed
    )

    if (.verbose)
      message("Summarizing intermediate results...")


    results <- summarise_intermediate_results(
      intermediate_results,
      propensity_scored = ps_flags$summarise,
      label_distribution = label_distribution
    )
  } else {

    # generate n_bt copies of results, plus one original

    if (.verbose)
      message("Computing bootstrap confidence intervals...")

    boot_results <- generate_replicate_results(
      base_compare = compare,
      n_bt = n_bt,
      grouping_var = grouping_var,
      seed = seed,
      ps_flags = ps_flags,
      label_distribution = label_distribution,
      cost_fp = cost_fp_processed,
      .progress = .progress
    )

    # take the original as THE result
    results <- dplyr::select(
      dplyr::filter(boot_results, .data$boot_replicate == "Apparent"),
      -"boot_replicate"
    )

    # compute the confidence intervals as quantiles of the value-distribution
    # coming from the boot-strap copies
    # Note: this has to respect the desired grouping structure for the various
    # stratification variables
    smry_grouping_var <- c(
      "metric",
      setdiff(grouping_var, c("doc_id", "label_id"))
    )
    boot_results_grpd <- dplyr::group_by(boot_results,
                                         !!!rlang::syms(smry_grouping_var))
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

    # rearrange cols, so that support is last col
    results <- dplyr::select(results, setdiff(colnames(results), "support"),
                             "support")
  }

  # add column that indicates the aggregation mode
  results <- dplyr::mutate(results, mode = mode, .after = "metric")

  if (graded_relevance && rename_graded_metrics) {
    results <- rename_metrics(results)
  }

  results

}


#' @describeIn compute_set_retrieval_scores variant with internal usage of
#'  dplyr rather than collapse library. Tends to be slower, but more stable
compute_set_retrieval_scores_dplyr <- function( # nolint
    gold_standard,
    predicted,
    mode = "doc-avg",
    compute_bootstrap_ci = FALSE,
    n_bt = 10L,
    doc_strata = NULL,
    label_dict = NULL,
    graded_relevance = FALSE,
    rename_graded_metrics = FALSE,
    seed = NULL,
    propensity_scored = FALSE,
    label_distribution = NULL,
    cost_fp_constant = NULL,
    .ignore_relevance_warning = FALSE,
    .verbose = FALSE,
    .progress = FALSE) {

  stopifnot(is.logical(compute_bootstrap_ci))
  if (!is.null(doc_strata)) {
    stopifnot(doc_strata %in% colnames(gold_standard))
  }
  if (propensity_scored) {
    stopifnot(!is.null(label_distribution))
    stopifnot(all(
      c("label_id", "label_freq") %in% colnames(label_distribution)
    ))
  }

  compare <- create_comparison(
    gold_standard, predicted,
    doc_strata = doc_strata,
    label_dict = label_dict,
    graded_relevance = graded_relevance,
    .ignore_relevance_warning = .ignore_relevance_warning
  )

  cost_fp_processed <- NULL

  ps_flags <- set_ps_flags(mode, propensity_scored)

  if (propensity_scored) {
    label_weights <- compute_propensity_scores(label_distribution)

    rlang::try_fetch({
      compare <- dplyr::inner_join(
        x = compare,
        y = label_weights,
        by = c("label_id"),
        relationship = "many-to-one",
        unmatched = c("error", "drop") # i.e. every label in compare must have
        # exactly one weight, but the label_weights
        # may contain more labels than compare
      )
    },
    error = function(cnd) {
      rlang::abort(
        "Label distribution does not match gold_standard or predicted labels.",
        parent = cnd
      )
    })

    cost_fp_processed <- process_cost_fp(cost_fp_constant, compare)

  }

  grouping_var <- set_grouping_var(mode, doc_strata, label_dict)

  # give warnings, when additional stratification variables are not factors
  test_factor <- function(var) {
    if (!is.factor(compare[[var]]))
      warning(paste(
        var,
        "is not a factor variable.",
        "Some levels may be lost in bootstrap replications"
      ))
  }
  if (compute_bootstrap_ci) {
    purrr::map(setdiff(grouping_var, c("doc_id", "label_id")),
               .f = test_factor)

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
    if (.verbose)
      message("Computing bootstrap confidence intervals...")

    boot_results <- generate_replicate_results_dplyr(
      base_compare = compare,
      n_bt = n_bt,
      grouping_var = grouping_var,
      seed = seed,
      ps_flags = ps_flags,
      label_distribution = label_distribution,
      cost_fp = cost_fp_processed,
      .progress = .progress
    )

    # take the original as THE result
    results <- dplyr::select(
      dplyr::filter(boot_results, .data$boot_replicate == "Apparent"),
      -"boot_replicate"
    )

    # compute the confidence intervals as quantiles of the value-distribution
    # coming from the boot-strap copies
    # Note: this has to respect the desired grouping structure for the various
    # stratification variables
    smry_grouping_var <- c(
      "metric",
      setdiff(grouping_var, c("doc_id", "label_id"))
    )
    boot_results_grpd <- dplyr::group_by(boot_results,
                                         !!!rlang::syms(smry_grouping_var))
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

    # rearrange cols, so that support is last col
    results <- dplyr::select(results, setdiff(colnames(results), "support"),
                             "support")
  }

  # add column that indicates the aggregation mode
  results <- dplyr::mutate(results, mode = mode, .after = "metric")

  if (graded_relevance && rename_graded_metrics) {
    results <- rename_metrics(results)
  }

  results

}
