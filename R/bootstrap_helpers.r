#' Compute n_bt bootstrap replica, wrapping the composure of
#' compute_intermediate_results summarise_intermediate_results
#'
#' @param base_compare a \code{data.frame} as generate by create_comparison
#' @param n_bt  an integer number of resamples to undergo in bootstrapping
#' @param grouping_var character vector of variables that must be present in
#'  base_compare
#' @param seed pass seed to resampling step for reproducibility
#' @param ps_flags list as returned by `set_ps_flags`
#' @param cost_fp numeric > 0, default is NULL
#' @param label_distribution as in compute_set_retrieval_scores
#' @inheritParams option_params
#'
#' @return \code{data.frame} containing \code{n_bt} boot replica of results
#'   as returned by compute_intermediate_results +
#'   summarise_intermediate_results
#'
#' @examples
#'
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
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "d",
#'   "A", "f",
#'   "B", "a",
#'   "B", "e",
#'   "C", "f"
#' )
#'
#' base_compare <- casimir:::create_comparison(gold, pred)
#'
#' boot_replica <- casimir:::generate_replicate_results(
#'   base_compare,
#'   n_bt = 10L,
#'   grouping_var = c("doc_id")
#' )
#'
generate_replicate_results <- function(
    base_compare,
    n_bt,
    grouping_var,
    seed = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    label_distribution = NULL,
    cost_fp = NULL,
    drop_empty_groups = options::opt("drop_empty_groups"),
    progress = options::opt("progress")) {
  stopifnot(is.data.frame(base_compare))
  stopifnot(is.integer(n_bt))
  stopifnot("gold" %in% colnames(base_compare))
  stopifnot(is.logical(progress))

  doc_id_list <- dplyr::distinct(
    dplyr::filter(base_compare, .data$gold == TRUE),
    .data$doc_id
  )

  # core resampling is done by rsample library:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  boot <- suppressWarnings(
    rsample::bootstraps(data = doc_id_list, times = n_bt, apparent = TRUE)
  )
  boot_dfs <- stats::setNames(boot$splits, boot$id)
  boot_dfs <- purrr::map(boot_dfs, as.data.frame)


  # apply wrapper to each of the bootstrap replica
  # note: a call to furrr attaches purrr
  boot_results <- furrr::future_map_dfr(
    boot_dfs,
    .f = helper_f,
    .progress = progress,
    .options = furrr::furrr_options(seed = seed),
    .id = "boot_replicate",
    compare_cpy = base_compare,
    ps_flags = ps_flags,
    label_distribution = label_distribution,
    cost_fp = cost_fp,
    grouping_var = grouping_var,
    drop_empty_groups = drop_empty_groups
  )

  boot_results
}

#' wrapper for compute_intermediate and summarise on one bt_sample
#'
#' @param sampled_id_list doc_ids of this bootstrap
#' @param compare_cpy as created by \code{create_comparison}
#' @param grouping_var variables for aggreation
#' @param ps_flags list as returned by `set_ps_flags`
#' @param cost_fp numeric > 0
#' @param label_distribution as in compute_set_retrieval_scores
#' @inheritParams option_params
#'
#' @return as \code{summarise_intermediate_results}
helper_f <- function(
    sampled_id_list,
    compare_cpy,
    grouping_var,
    label_distribution = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    cost_fp = NULL,
    drop_empty_groups = options::opt("drop_empty_groups")) {
  compare_resampled <- collapse::join(
    compare_cpy, sampled_id_list,
    on = "doc_id", how = "inner", verbose = 0,
    multiple = TRUE,
    validate = "m:m"
  )

  intermediate_results_resampled <- compute_intermediate_results(
    compare_resampled,
    grouping_var,
    propensity_scored = ps_flags$intermed,
    cost_fp = cost_fp,
    drop_empty_groups = drop_empty_groups
  )
  summarise_intermediate_results(
    intermediate_results_resampled,
    propensity_scored = ps_flags$summarise,
    label_distribution = label_distribution
  )
}


#' @describeIn generate_replicate_results variant with dplyr based
#' internals rather then collapse internals
generate_replicate_results_dplyr <- function( # nolint styler: off
    base_compare,
    n_bt,
    grouping_var,
    seed = NULL,
    label_distribution = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    cost_fp = NULL,
    progress = FALSE) {
  stopifnot(is.data.frame(base_compare))
  stopifnot(is.integer(n_bt))
  stopifnot("gold" %in% colnames(base_compare))
  stopifnot(is.logical(progress))

  doc_id_list <- dplyr::distinct(
    dplyr::filter(base_compare, .data$gold == TRUE),
    .data$doc_id
  )

  # core resampling is done by rsample library:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  boot <- suppressWarnings(
    rsample::bootstraps(data = doc_id_list, times = n_bt, apparent = TRUE)
  )
  boot_dfs <- stats::setNames(boot$splits, boot$id)
  boot_dfs <- purrr::map(boot_dfs, as.data.frame)

  # apply wrapper to each of the bootstrap replica
  # note: a call to furrr attaches purrr
  boot_results <- furrr::future_map_dfr(
    boot_dfs,
    .f = helper_f_dplyr,
    .progress = progress,
    .id = "boot_replicate",
    .options = furrr::furrr_options(seed = seed),
    compare_cpy = base_compare,
    ps_flags = ps_flags,
    label_distribution = label_distribution,
    cost_fp = cost_fp,
    grouping_var = grouping_var
  )

  boot_results
}

#' Internal wrapper for compute_intermediat and summarise on one bt_sample
#'
#' @param sampled_id_list doc_ids of this bootstrap
#' @param compare_cpy as created by \code{create_comparison}
#' @param grouping_var variables for aggreation
#' @param cost_fp numeric > 0
#' @param label_distribution as in compute_set_retrieval_scores
#' @param ps_flags list with logicals "intermed" and "summarise"
#'
#' @return as \code{summarise_intermediate_results_dplyr}
helper_f_dplyr <- function(
    sampled_id_list,
    compare_cpy,
    grouping_var,
    label_distribution = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    cost_fp = NULL) {
  compare_resampled <- dplyr::inner_join(
    compare_cpy, sampled_id_list,
    by = "doc_id",
    relationship = "many-to-many"
  )
  intermediate_results_resampled <- compute_intermediate_results_dplyr(
    compare_resampled,
    rlang::syms(grouping_var),
    propensity_scored = ps_flags$intermed,
    cost_fp
  )
  summarise_intermediate_results_dplyr(
    intermediate_results_resampled,
    propensity_scored = ps_flags$summarise,
    label_distribution = label_distribution
  )
}
