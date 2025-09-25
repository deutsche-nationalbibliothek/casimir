################################################################################
#### helper functions

#' Join gold standard and predicted results in one table
#'
#' @param gold_standard expects data.frame with cols "label_id", "doc_id", "score"
#' @param predicted expects data.frame with cols "label_id", "doc_id", "score"
#' @param doc_strata variable that should be preserved, with a fixed mapping of
#'    doc_strata and doc_id
#' @param label_dict two-column data.frame with col "label_id" and a second column
#'   that defines groups of labels to stratify by
#' @param graded_relevance logical indicator for graded relevance. Defaults to
#'  \code{FALSE} for binary relevance. If set to \code{TRUE}, the
#'  \code{predicted} data.frame should contain a numeric column
#'  \emph{"relevance"} with values in the range of \code{c(0, 1)}.
#' @param propensity_scored logical, whether to use propensity scores as weights
#' @param label_distribution expects \code{data.frame} with cols
#'   \emph{"label_id", "label_freq", "n_docs"}. \code{label_freq} corresonds to
#'   the number of occurences a label has in the gold_standard. \code{n_docs}
#'   corresponds to the total number of documents in the gold_standard.
#' @param .ignore_relevance_warning logical, if graded_relevance = FALSE, but
#'   column relevance is present in predicted, a warning can be silenced by
#'   setting .ignore_relevance_warning = TRUE
#'
#' @return data.frame with cols "label_id", "doc_id", "suggested", "gold"
#'
#' @export
#'
#' @examples
#' library(casimir)
#'
#' gold <- tibble::tribble(
#'     ~doc_id, ~label_id,
#'     "A", "a",
#'     "A", "b",
#'     "A", "c",
#'     "B", "a",
#'     "B", "d",
#'     "C", "a",
#'     "C", "b",
#'     "C", "d",
#'     "C", "f"
#' )
#'
#' pred<- tibble::tribble(
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "d",
#'   "A", "f",
#'   "B", "a",
#'   "B", "e",
#'   "C", "f"
#' )
#'
#' casimir::create_comparison(gold, pred)
create_comparison <- function(gold_standard, predicted,
                              doc_strata = NULL, label_dict = NULL,
                              graded_relevance = FALSE,
                              propensity_scored = FALSE,
                              label_distribution = NULL,
                              .ignore_relevance_warning = FALSE) {

  stopifnot(all(c("label_id", "doc_id") %in% colnames(gold_standard)))
  stopifnot(all(c("label_id", "doc_id") %in% colnames(predicted)))

  if (graded_relevance)
    predicted <- check_repair_relevance_predicted(predicted)

  if (graded_relevance & propensity_scored) {
   warning("Mixing graded relevance and propensity_scores is not tested. Are
           you sure this is what you want?")
  }

  if (propensity_scored) {
    if (is.null(label_distribution)) {
      stop("If propensity_scored = TRUE, label_distribution must be provided.")
    }
    stopifnot(all(c("label_id", "label_freq") %in% colnames(label_distribution)))
  }

  if (!is.null(doc_strata)) {
    stopifnot(doc_strata %in% colnames(gold_standard))
  }

  if (!is.null(label_dict)) {
    stopifnot("label_id" %in% colnames(label_dict))
  }

  # the set of title ids must always agree in predicted and gold_standard
  gold_wo_predicted <- dplyr::anti_join(gold_standard, predicted, by = "doc_id")
  predicted_wo_gold <- dplyr::anti_join(predicted, gold_standard, by = "doc_id")

  stopifnot(nrow(predicted_wo_gold) == 0)
  if (nrow(gold_wo_predicted) > 0)
    warning("gold standard data contains documents that are not in predicted set")

  compare <- dplyr::full_join(
    x = dplyr::mutate(
      dplyr::select(gold_standard,-!!doc_strata),
      gold = TRUE),
    y = dplyr::mutate(predicted, suggested = TRUE),
    by = c("doc_id", "label_id"), relationship = "one-to-one",
    suffix = c(".gold", ".pred") # this is intended to differentiate columns
    # that might be dragged along later
  )

  compare_w_strata <- dplyr::left_join(
    x = compare,
    y = dplyr::distinct(dplyr::select(gold_standard,"doc_id",!!doc_strata)),
    by = c("doc_id")
  )

  if (!is.null(label_dict)) {
    compare_w_strata <- dplyr::left_join(
      x = compare_w_strata,
      y = label_dict,
      by = c("label_id")
    )
  }

  if (propensity_scored) {
    label_weights <- compute_propensity_scores(label_distribution)
    compare_w_strata <- join_propensity_scores(compare_w_strata, label_weights)
  }

  result <- tidyr::replace_na(
    compare_w_strata,
    replace = list(
      gold = FALSE,
      suggested = FALSE
    )
  )


  if (graded_relevance) {
    result <- check_repair_relevance_compare(result)
  } else {
    # test if column relevane exists
    if ("relevance" %in% colnames(result) & !.ignore_relevance_warning) {
      warning("column 'relevance' in predicted is ignored, as
              graded_relevance = FALSE. Overwriting with relevance = 0.
              Silence this warning with .ignore_relevance_warning = TRUE")
    }
    result <- collapse::ftransform(result,
                                    relevance = 0
    )
  }

}

#' Compute mean of intermediate results
#'
#' @param intermediate_results as produced by compute intermediate results.
#'   This requires a list containing
#'   #' \itemize{
#'    \item \code{grouping_var} a  character vector of variables to group by
#'    \item \code{results_table}  a  with cols "prec", "rprec", "rec", "f1" as
#'   }
#' @param propensity_scored logical, whether to use propensity scores as weights
#' @param label_distribution expects \code{data.frame} with cols
#'   \emph{"label_id", "label_freq", "n_docs"}. \code{label_freq} corresonds to
#'   the number of occurences a label has in the gold_standard. \code{n_docs}
#'   corresponds to the total number of documents in the gold_standard.
#' @export
#'
#' @return data.frame with cols metric, value
summarise_intermediate_results <- function(
    intermediate_results,
    propensity_scored = FALSE,
    label_distribution = NULL
    ) {

  grouping_var <- intermediate_results$grouping_var
  intrmd_res <- intermediate_results$results_table
  if (propensity_scored) {
    if (is.null(label_distribution))
      stop("applying propensity scores requires label_distribution")

    label_weights <- compute_propensity_scores(label_distribution)
    intrmd_res <- join_propensity_scores(intrmd_res, label_weights)

  } else {
    intrmd_res <- collapse::fmutate(intrmd_res, label_weight = 1)
  }

  if (all(c("doc_id", "label_id") %in% grouping_var))
    mode = "micro"
  else
    mode = "something-else"

  stopifnot(
    all(
      c(grouping_var, "prec", "rprec", "rec", "f1") %in% colnames(intrmd_res))
  )

  present_grouping_vars <- grouping_var
  new_grouping_vars <- setdiff(present_grouping_vars, c("doc_id", "label_id", ".rows"))

  if (length(new_grouping_vars) == 0) {
    intrmd_res_regrouped <- intrmd_res
  } else {
    intrmd_res_regrouped <- collapse::fgroup_by(
      collapse::fungroup(intrmd_res),
      new_grouping_vars
      # drop = FALSE # .drop doesn't exist with collapse
    )
    actual_groups <- attr(intrmd_res_regrouped, "groups")$group.vars
    # catch a nasty bug in collapse
    if (actual_groups[1] == "new_grouping_vars")
      intrmd_res_regrouped <- collapse::fgroup_by(
        collapse::fungroup(intrmd_res),
        1:1
        # drop = FALSE # .drop doesn't exist with collapse
      )
  }

  fsum_custom  <- function(x, w = NULL) {
    # note function needs a dummy variable `w`, so that it can be passed to
    fsum(!is.na(x))
  }

  if (mode == "micro"){
    if (propensity_scored == TRUE) {
      "Summarise does not support micro average and propensity_scored = TRUE"
    }

    im_res_smry <- collapse::fsummarise(
      intrmd_res_regrouped,
      across(
        n_gold:delta_relevance, list(value = fsum)
      ),
      keep.group_vars = TRUE)

    im_res_smry <- collapse::ftransform(
      .data = im_res_smry,
      prec_value = ifelse(tp + fp == 0,
                    NA_real_,
                    (tp + delta_relevance)/(tp + fp)),
      # compute rprecision as in Manning etal.
      rprec_value = ifelse(pmin(n_gold, n_suggested) == 0,
                     NA_real_,
                     (tp + delta_relevance)/pmin(tp + fn + delta_relevance, tp + fp)),
      rec_value = ifelse(tp + fn == 0,
                   NA_real_,
                   (tp + delta_relevance)/(tp + fn + delta_relevance)),
      # NA-Handling for F1:
      # return NA if both prec and rec are NA
      # return 0 if only one of them is NA
      f1_value = ifelse(
        tp + fp + fn == 0,
        NA_real_,
        2 * (tp + delta_relevance) / (2 * tp + fp + fn + delta_relevance)
      ),
      prec_support = n_suggested,
      rec_support = n_gold,
      rprec_support = pmin(n_gold, n_suggested),
      f1_support = 0.5*(n_suggested + n_gold)
    )

  } else { # for macro-avegared results

    im_res_smry <- collapse::fsummarise(
      intrmd_res_regrouped,
      across(
        prec:f1,
        list(
          value = fmean,
          support = fsum_custom
        ),
        w = label_weight
      ),
      keep.group_vars = TRUE
    )
  }

  im_res_smry_long <- dplyr::select(
    im_res_smry,
    dplyr::all_of(new_grouping_vars),
    dplyr::ends_with("value"), dplyr::ends_with("support"),
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::matches("(value|support)$"),
    names_to = c("metric", "kind"),
    names_sep = "_",
    values_to = "value"
  )

  im_res_smry_wide <- tidyr::pivot_wider(
    im_res_smry_long,
    names_from = "kind",
    values_from = "value")
  # if (length(new_grouping_vars) > 0) {
  # im_res_smry_wide <- tidyr::separate(
  #   data = im_res_smry_wide,
  #   col = grp_id,
  #   into = grouping_var,
  #   sep = "\\.",
  #   remove = TRUE
  # )
  # }

  dplyr::arrange(im_res_smry_wide, .data$metric)
}

#' Compute mean of intermediate results. Variant with dplyr based
#' internals rather then collapse internals
#'
#' @param intermediate_results with cols "prec", "rprec", "rec", "f1" as
#'   produced by compute intermediate results
#' @param propensity_scored as in `compute_set_retrieval_scores`
#' @param label_distribution as in `compute_set_retrieval_scores`
#'
#' @return data.frame with cols metric, value
summarise_intermediate_results_dplyr_version <- function(
    intermediate_results,
    propensity_scored = FALSE,
    label_distribution = NULL
    ) {

  stopifnot(all(c("prec", "rprec", "rec", "f1") %in% colnames(intermediate_results)))

  present_grouping_vars <- colnames(attr(intermediate_results, "groups"))
  if (all(c("doc_id", "label_id") %in% present_grouping_vars))
    mode = "micro"
  else
    mode = "something-else"

  if (propensity_scored) {
    if (is.null(label_distribution))
      stop("applying propensity scores requires label_distribution")

    label_weights <- compute_propensity_scores(label_distribution)
    intermediate_results <- join_propensity_scores(intermediate_results, label_weights)

  } else {
    intermediate_results <- dplyr::mutate(intermediate_results, label_weight = 1)
  }

  new_grouping_vars <- setdiff(present_grouping_vars, c("doc_id", "label_id", ".rows"))

  intermediate_results_regrouped <- dplyr::group_by(
    dplyr::ungroup(intermediate_results),
    !!!rlang::syms(new_grouping_vars),
    .drop = FALSE
  )

  if (mode == "micro"){
    if (propensity_scored == TRUE) {
      "Summarise does not support micro average and propensity_scored = TRUE"
    }

    im_res_smry <- dplyr::summarise(
      intermediate_results_regrouped,
      dplyr::across(
        .cols = c("n_gold", "n_suggested", "tp", "fn", "fp", "delta_relevance"),
        .fns = sum
      ),
      .groups = "keep")

    im_res_smry <- im_res_smry |>
      dplyr::mutate(
        rprec_deno = pmin(tp + fp, tp + fn + delta_relevance)
      )

    im_res_smry <- dplyr::transmute(
      im_res_smry,
      prec_value = ifelse(tp + fp == 0,
                          NA_real_,
                          (tp + delta_relevance)/(tp + fp)),
      # compute rprecision as in Manning etal.
      rprec_value = ifelse(rprec_deno == 0,
                           NA_real_,
                           (tp + delta_relevance)/rprec_deno),
      rec_value = ifelse(tp + fn == 0,
                         NA_real_,
                         (tp + delta_relevance)/(tp + fn + delta_relevance)),
      # NA-Handling for F1:
      # return NA if both prec and rec are NA
      # return 0 if only one of them is NA
      f1_value = ifelse(
        tp + fp + fn == 0,
        NA_real_,
        2 * (tp + delta_relevance) / (2 * tp + fp + fn + delta_relevance)
      ),
      prec_support = n_suggested,
      rec_support = n_gold,
      rprec_support = pmin(n_gold, n_suggested),
      f1_support = 0.5*(n_suggested + n_gold)
    )
  } else {

    im_res_smry <- dplyr::summarise(
      intermediate_results_regrouped,
      dplyr::across(
        .cols = c("prec", "rprec", "rec", "f1"),
        .fns = list(value = ~sum(.x * .data$label_weight, na.rm = TRUE) / sum(as.numeric(!is.na(.x)) * .data$label_weight),
                    support = ~sum(!is.na(.x)))
      ),
      .groups = "keep")
  }

  im_res_smry_long <- tidyr::pivot_longer(
    im_res_smry,
    cols = dplyr::matches("(value|support)$"),
    names_to = c("metric", "kind"),
    names_sep = "_",
    values_to = "value"
  )

  im_res_smry_wide <- tidyr::pivot_wider(
    im_res_smry_long,
    names_from = "kind",
    values_from = "value")

  dplyr::ungroup(
    dplyr::arrange(im_res_smry_wide, .data$metric)
  )
}


#' Compute true positives, false positives on aggregation level specified
#'
#' @param gold_vs_pred \code{data.frame} with logical columns
#'   \emph{"suggested", "gold"} as produced by \code{create_comparison}
#' @param grouping_var a character vector of variables that must be present in
#'  gold_vs_pred (dplyr version requires rlang symbols)
#' @param propensity_scored logical, whether to use propensity scores as weights
#' @param cost_fp numeric > 0, default is NULL
#'
#' @return data.frame with cols "n_gold", "n_suggested", "tp", "fp", "fn", "prec", "rec", "f1"
#'
#' @export
#'
#' @examples
#'
#' gold <- tibble::tribble(
#'     ~doc_id, ~label_id,
#'     "A", "a",
#'     "A", "b",
#'     "A", "c",
#'     "B", "a",
#'     "B", "d",
#'     "C", "a",
#'     "C", "b",
#'     "C", "d",
#'     "C", "f"
#' )
#'
#' pred<- tibble::tribble(
#'   ~doc_id, ~label_id,
#'   "A", "a",
#'   "A", "d",
#'   "A", "f",
#'   "B", "a",
#'   "B", "e",
#'   "C", "f"
#' )
#'
#' gold_vs_pred <- create_comparison(gold, pred)
#' casimir:::compute_intermediate_results(gold_vs_pred, "doc_id")
#'
compute_intermediate_results <- function(
    gold_vs_pred,
    grouping_var,
    propensity_scored = FALSE,
    cost_fp = NULL) {

  stopifnot(all(c("suggested", "gold") %in% colnames(gold_vs_pred)))

  stopifnot(!is.null(grouping_var))
  # check that no levels of the grouping variables contain dots
  for (var in grouping_var) {
    n_dots <- sum(
      stringr::str_detect(gold_vs_pred[[var]], pattern = "\\."), na.rm = TRUE)
    if (n_dots > 0)
      stop("grouping variable must not contain levels that contain dots")
  }

  g <- collapse::GRP(gold_vs_pred, grouping_var)

  if (!("relevance") %in% colnames(gold_vs_pred)) {
    gold_vs_pred <- dplyr::mutate(
      gold_vs_pred,
      relevance = 0.0
    )
  }

  if (propensity_scored) {
    stopifnot("label_weight" %in% colnames(gold_vs_pred))

    gold_vs_pred_smry <- find_ps_rprec_deno(gold_vs_pred, grouping_var, cost_fp)

  } else {

    rowwise_trans <- collapse::fcompute(
      .data = gold_vs_pred,
      n_gold = gold,
      n_suggested = suggested,
      tp = gold & suggested,
      fp = !gold & suggested,
      fn = gold & !suggested,
      delta_relevance = (!gold & suggested) * relevance
    )
    gold_vs_pred_smry <- collapse::fsum(rowwise_trans, g)

    gold_vs_pred_smry <- collapse::ftransform(
      .data = gold_vs_pred_smry,
      rprec_deno = pmin(n_gold + delta_relevance, n_suggested),
      grp_names = row.names(gold_vs_pred_smry)
    )

  }

  gold_vs_pred_smry <- collapse::ftransform(
    .data = gold_vs_pred_smry,
    prec = ifelse(n_suggested == 0,
                  NA_real_,
                  (tp + delta_relevance)/(tp + fp)),
    # compute rprecision as in Manning etal.
    rprec = ifelse(pmin(n_gold, n_suggested) == 0,
                   NA_real_,
                   (tp + delta_relevance)/rprec_deno),
    rec = ifelse(n_gold == 0,
                 NA_real_,
                 (tp + delta_relevance)/(tp + fn + delta_relevance)),
    # NA-Handling for F1:
    # return NA if both prec and rec are NA
    # return 0 if only one of them is NA
    f1 = ifelse(
      n_gold + n_suggested == 0,
      NA_real_,
      2 * (tp + delta_relevance) / (2*tp + fp + fn + delta_relevance)
    )

  )

  gold_vs_pred_smry <- tidyr::separate(
    data = gold_vs_pred_smry,
    col = grp_names,
    into = grouping_var,
    sep = "\\.",
    remove = TRUE
  )

  # restore the factor structure of the original grouping_var
  for (var in grouping_var) {
    if (is.factor(gold_vs_pred[[var]])) {
      gold_vs_pred_smry[[var]] <- factor(
        x = gold_vs_pred_smry[[var]],
        levels = levels(gold_vs_pred[[var]])
      )

    }
  }

  res_df <- dplyr::select(gold_vs_pred_smry, dplyr::all_of(grouping_var), dplyr::everything())

  list(
    results_table = res_df,
    grouping_var = grouping_var
  )
}

#' @describeIn compute_intermediate_results variant with dplyr based
#' internals rather then collapse internals
compute_intermediate_results_dplyr_version <- function(
    gold_vs_pred,
    grouping_var,
    propensity_scored = FALSE,
    cost_fp = NULL
    ) {

  stopifnot(all(c("suggested", "gold") %in% colnames(gold_vs_pred)))

  if (!("relevance") %in% colnames(gold_vs_pred)) {
    gold_vs_pred <- dplyr::mutate(
      gold_vs_pred,
      relevance = 0.0
    )
  }

  if (propensity_scored) {
    stopifnot("label_weight" %in% colnames(gold_vs_pred))

    gold_vs_pred_smry <- find_ps_rprec_deno_dplyr(
      gold_vs_pred,
      grouping_var,
      cost_fp
    )

  } else {
    gold_vs_pred_smry <- dplyr::summarise(
      dplyr::group_by(gold_vs_pred, !!!grouping_var),
      n_gold = sum(.data$gold),
      n_suggested = sum(.data$suggested),
      tp = sum(.data$gold & .data$suggested),
      fp = sum(!.data$gold & .data$suggested),
      fn = sum(.data$gold & !.data$suggested),
      delta_relevance = sum(.data$relevance * (!.data$gold & .data$suggested)),
      rprec_deno = dplyr::if_else(
        pmin(n_gold, n_suggested) == 0,
        0,
        pmin(n_gold + delta_relevance, n_suggested),
        NA_real_
      ),
      .groups = "keep")
  }

  gold_vs_pred_smry <- dplyr::mutate(gold_vs_pred_smry,
                prec = ifelse(.data$n_suggested == 0,
                              NA_real_,
                              (.data$tp + .data$delta_relevance)/(.data$tp + .data$fp)),
                rec = ifelse(.data$n_gold == 0,
                             NA_real_,
                             (.data$tp + .data$delta_relevance)/(.data$tp + .data$fn + .data$delta_relevance)),
                # NA-Handling for F1:
                # return NA if both prec and rec are NA
                # return 0 if only one of them is NA
                f1 = ifelse(
                  .data$n_gold + .data$n_suggested == 0,
                  NA_real_,
                  2 * (.data$tp + .data$delta_relevance) / (2*.data$tp + .data$fp + .data$fn + .data$delta_relevance)
                ))

  # compute rprecision as in Manning etal.
  gold_vs_pred_smry <- dplyr::mutate(gold_vs_pred_smry,
                                     rprec = ifelse(.data$rprec_deno == 0,
                                                    NA_real_,
                                                    (.data$tp + .data$delta_relevance)/.data$rprec_deno),
                                     .before = "rec")

  gold_vs_pred_smry
}



#' Compute n_bt bootstrap replica, wrapping the composure of
#' compute_intermediate_results summarise_intermediate_results
#'
#' @param base_compare a \code{data.frame} as generate by create_comparison
#' @param n_bt  an integer number of resamples to undergo in bootstrapping
#' @param grouping_var character vector of variables that must be present in
#'  base_compare
#' @param .progress logical activating .progress bar in internal
#'  \pkg{furrr}-computation
#' @param seed pass seed to resampling step for reproducibility
#' @param ps_flags list as returned by `set_ps_flags`
#' @param cost_fp numeric > 0, default is NULL
#' @param label_distribution as in compute_set_retrieval_scores
#'
#' @return \code{data.frame} containing \code{n_bt} boot replica of results
#'   as returned by  compute_intermediate_results summarise_intermediate_results
#'
#' @examples
#'
#' library(casimir)
#'
#' gold <- tibble::tribble(
#'     ~doc_id, ~label_id,
#'     "A", "a",
#'     "A", "b",
#'     "A", "c",
#'     "B", "a",
#'     "B", "d",
#'     "C", "a",
#'     "C", "b",
#'     "C", "d",
#'     "C", "f"
#' )
#'
#' pred<- tibble::tribble(
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
#'   base_compare, n_bt = 10L,
#'   grouping_var = c("doc_id")
#'   )
#'
generate_replicate_results<- function(
    base_compare,
    n_bt,
    grouping_var,
    seed = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    label_distribution = NULL,
    cost_fp = NULL,
    .progress = FALSE) {

  stopifnot(is.data.frame(base_compare))
  stopifnot(is.integer(n_bt))
  stopifnot("gold" %in% colnames(base_compare))
  stopifnot(is.logical(.progress))

  doc_id_list <- dplyr::distinct(dplyr::filter(base_compare, .data$gold == TRUE),
                                 .data$doc_id)

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
    .progress = .progress,
    .options = furrr::furrr_options(seed = seed),
    .id = "boot_replicate",
    compare_cpy = base_compare,
    ps_flags = ps_flags,
    label_distribution = label_distribution,
    cost_fp = cost_fp,
    grouping_var = grouping_var)
}

#' wrapper for compute_intermediat and summarise on one bt_sample
#'
#' @param sampled_id_list doc_ids of this bootstrap
#' @param compare_cpy as created by \code{create_comparison}
#' @param grouping_var variables for aggreation
#' @param ps_flags list as returned by `set_ps_flags`
#' @param cost_fp numeric > 0
#' @param label_distribution as in compute_set_retrieval_scores
#'
#' @return as \code{summarise_intermediate_results}
helper_f <- function(
    sampled_id_list,
    compare_cpy,
    grouping_var,
    label_distribution = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    cost_fp = NULL) {
  compare_resampled <- dplyr::inner_join(compare_cpy, sampled_id_list,
                                         by = "doc_id",
                                         relationship = "many-to-many")
  intermediate_results_resampled <- compute_intermediate_results(
    compare_resampled,
    grouping_var,
    propensity_scored = ps_flags$intermed,
    cost_fp = cost_fp
  )
  summarise_intermediate_results(
    intermediate_results_resampled,
    propensity_scored = ps_flags$summarise,
    label_distribution = label_distribution
  )
}


#' @describeIn generate_replicate_results variant with dplyr based
#' internals rather then collapse internals
generate_replicate_results_dplyr_version <- function(
    base_compare,
    n_bt,
    grouping_var,
    seed = NULL,
    label_distribution = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    cost_fp = NULL,
    .progress = FALSE) {

  stopifnot(is.data.frame(base_compare))
  stopifnot(is.integer(n_bt))
  stopifnot("gold" %in% colnames(base_compare))
  stopifnot(is.logical(.progress))

  doc_id_list <- dplyr::distinct(dplyr::filter(base_compare, .data$gold == TRUE),
                                 .data$doc_id)

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
    .f = helper_f_dplyr_version,
    .progress = .progress,
    .id = "boot_replicate",
    .options = furrr::furrr_options(seed = seed),
    compare_cpy = base_compare,
    ps_flags = ps_flags,
    label_distribution = label_distribution,
    cost_fp = cost_fp,
    grouping_var = grouping_var)
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
#' @return as \code{summarise_intermediate_results_dplyr_version}
helper_f_dplyr_version <- function(
    sampled_id_list,
    compare_cpy,
    grouping_var,
    label_distribution = NULL,
    ps_flags = list("intermed" = FALSE, "summarise" = FALSE),
    cost_fp = NULL) {
  compare_resampled <- dplyr::inner_join(compare_cpy, sampled_id_list,
                                         by = "doc_id",
                                         relationship = "many-to-many")
  intermediate_results_resampled <- compute_intermediate_results_dplyr_version(
    compare_resampled,
    rlang::syms(grouping_var),
    propensity_scored = ps_flags$intermed,
    cost_fp
  )
  summarise_intermediate_results_dplyr_version(
    intermediate_results_resampled,
    propensity_scored = ps_flags$summarise,
    label_distribution = label_distribution
  )
}

#' Internal helper function to check comparison matrix for
#' inconsistent gold_standard and relevance values
#'
#' @param gold_vs_pred as created by \code{create_comparison}
#'
#' @return comparison matrix compatible with compute_intermediate_results
check_repair_relevance_compare <- function(gold_vs_pred) {

  # set relevance to 1 if gold_standard = TRUE and relevance missing
  compare <- dplyr::mutate(
    gold_vs_pred,
    relevance = dplyr::if_else(
      .data$gold & is.na(.data$relevance), 1,
      .data$relevance
    )
  )

  inconsistent_values_tp <- nrow(
    dplyr::filter(
      compare,
      .data$gold & !is.na(.data$relevance) & .data$relevance < 1
    )
  )
  if (inconsistent_values_tp > 0) {
    warning(
      paste(
        "There are",
        inconsistent_values_tp,
        "inconsistent relevance values with relevance < 1 but gold_standard = TRUE. Setting relevance to 1."
      )
    )
    compare <- dplyr::mutate(
      compare,
      relevance = dplyr::if_else(
        gold & !is.na(.data$relevance) & .data$relevance < 1, 1, .data$relevance
      )
    )

  }

  inconsistent_values_fp <- nrow(
    dplyr::filter(
      compare,
      !.data$gold & !is.na(.data$relevance) & .data$relevance == 1
    )
  )
  if (inconsistent_values_fp > 0) {
    warning(
      paste(
        "There are",
        inconsistent_values_fp,
        "inconsistent relevance values with relevance == 1 but gold_standard = FALSE.\n
          Please correct relevance or gold_standard."
      )
    )
  }

  compare
}

#' Internal helper to check predicted data frame for valid relevance column
#'
#' @param predicted expects data.frame with column \emph{"relevance"}
#'
#' @return valid predicted data.frame with possibly eliminated missing values
check_repair_relevance_predicted <- function(predicted) {
  stopifnot("relevance" %in% colnames(predicted))
  stopifnot(is.numeric(predicted[["relevance"]]))
  # check for missing relevance values
  if (sum(is.na(predicted[["relevance"]])) > 0) {
    warning("NA values in 'relevance' column. Removing rows with NA values.")
    predicted <- dplyr::filter(predicted, !is.na(.data$relevance))
  }
  # check that relevance is between 0 and 1
  if (any(predicted[["relevance"]] < 0) || any(predicted[["relevance"]] > 1)) {
    stop("Relevance values must be between 0 and 1.")
  }

  predicted
}

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
