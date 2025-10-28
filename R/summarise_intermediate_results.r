#' Compute the mean of intermediate results
#'
#' Compute the mean of intermediate results created by
#' \code{compute_intermediate_results}.
#'
#' @param intermediate_results As produced by
#'   \code{compute_intermediate_results}. This requires a list containing:
#'   \itemize{
#'    \item \code{results_table} A data.frame with columns \code{"prec",
#'      "rprec", "rec", "f1"}.
#'    \item \code{grouping_var} A character vector of variables to group by.
#'   }
#' @inheritParams compute_set_retrieval_scores
#' @param set Logical. Allow in-place modification of
#'   \code{intermediate_results}. Only recommended for internal package usage.
#' @export
#'
#' @return A data.frame with columns \code{"metric", "value"}.
summarise_intermediate_results <- function(
    intermediate_results,
    propensity_scored = FALSE,
    label_distribution = NULL,
    set = FALSE,
    replace_zero_division_with = options::opt("replace_zero_division_with")) {
  grouping_var <- intermediate_results$grouping_var
  intrmd_res <- intermediate_results$results_table
  if (propensity_scored && ("label_id" %in% grouping_var)) {
    if (is.null(label_distribution)) {
      stop("applying propensity scores requires label_distribution")
    }

    label_weights <- compute_propensity_scores(label_distribution)
    intrmd_res <- join_propensity_scores(intrmd_res, label_weights)
  } else {
    intrmd_res <- collapse::fmutate(intrmd_res, label_weight = 1)
  }

  if (all(c("doc_id", "label_id") %in% grouping_var)) {
    mode <- "micro"
  } else {
    mode <- "something-else"
  }

  stopifnot(all(
    c(grouping_var, "prec", "rprec", "rec", "f1") %in% colnames(intrmd_res)
  ))

  present_grouping_vars <- grouping_var
  new_grouping_vars <- setdiff(
    present_grouping_vars, c("doc_id", "label_id", ".rows")
  )

  if (length(new_grouping_vars) == 0) {
    intrmd_res_regrouped <- intrmd_res
  } else {
    intrmd_res_regrouped <- collapse::fgroup_by(
      collapse::fungroup(intrmd_res),
      new_grouping_vars
      # drop = FALSE # .drop doesn't exist with collapse # nolint
    )
    actual_groups <- attr(intrmd_res_regrouped, "groups")$group.vars
    # catch a nasty bug in collapse
    if (actual_groups[1] == "new_grouping_vars") {
      intrmd_res_regrouped <- collapse::fgroup_by(
        collapse::fungroup(intrmd_res),
        1:1
        # drop = FALSE # .drop doesn't exist with collapse # nolint
      )
    }
  }

  fsum_custom <- function(x, w = NULL) {
    # note: function needs a dummy variable `w` so that it can be passed to
    fsum(!is.na(x))
  }

  if (mode == "micro") {
    if (propensity_scored == TRUE) {
      "Summarise does not support micro average and propensity_scored = TRUE"
    }

    im_res_smry <- collapse::fsummarise(
      intrmd_res_regrouped,
      across(
        n_gold:delta_relevance, list(value = fsum)
      ),
      keep.group_vars = TRUE
    )

    im_res_smry <- collapse::ftransform(
      .data = im_res_smry,
      prec_value = ifelse(tp + fp == 0,
        NA_real_,
        (tp + delta_relevance) / (tp + fp)
      ),
      # compute R-precision as in Manning et al.
      rprec_value = ifelse(pmin(n_gold, n_suggested) == 0,
        NA_real_,
        (tp + delta_relevance) /
          pmin(tp + fn + delta_relevance, tp + fp)
      ),
      rec_value = ifelse(tp + fn == 0,
        NA_real_,
        (tp + delta_relevance) / (tp + fn + delta_relevance)
      ),
      # NA handling for F1:
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
      f1_support = 0.5 * (n_suggested + n_gold)
    )
  } else { # for macro averaged results

    if (!is.null(replace_zero_division_with)) {
      intrmd_res_regrouped <- collapse::replace_na(
        X = intrmd_res_regrouped,
        cols = c("prec", "rec", "rprec", "f1"),
        value = replace_zero_division_with,
        set = set # allow in-place modification
      )
    }

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
    values_from = "value"
  )

  dplyr::arrange(im_res_smry_wide, .data$metric)
}

#' Compute the mean of intermediate results
#'
#' Compute the mean of intermediate results created by
#' \code{compute_intermediate_results}. Variant with dplyr based internals
#' rather than collapse internals.
#'
#' @inheritParams summarise_intermediate_results
#'
#' @return A data.frame with columns \code{"metric", "value"}.
summarise_intermediate_results_dplyr <- function( # nolint styler: off
    intermediate_results,
    propensity_scored = FALSE,
    label_distribution = NULL) {
  stopifnot(all(
    c("prec", "rprec", "rec", "f1") %in% colnames(intermediate_results)
  ))

  present_grouping_vars <- colnames(attr(intermediate_results, "groups"))
  if (all(c("doc_id", "label_id") %in% present_grouping_vars)) {
    mode <- "micro"
  } else {
    mode <- "something-else"
  }

  if (propensity_scored) {
    if (is.null(label_distribution)) {
      stop("applying propensity scores requires label_distribution")
    }

    label_weights <- compute_propensity_scores(label_distribution)
    intermediate_results <- join_propensity_scores(
      intermediate_results, label_weights
    )
  } else {
    intermediate_results <- dplyr::mutate(
      intermediate_results,
      label_weight = 1
    )
  }

  new_grouping_vars <- setdiff(
    present_grouping_vars,
    c("doc_id", "label_id", ".rows")
  )

  intermediate_results_regrouped <- dplyr::group_by(
    dplyr::ungroup(intermediate_results),
    !!!rlang::syms(new_grouping_vars),
    .drop = FALSE
  )

  if (mode == "micro") {
    if (propensity_scored == TRUE) {
      "Summarise does not support micro average and propensity_scored = TRUE"
    }

    im_res_smry <- dplyr::summarise(
      intermediate_results_regrouped,
      dplyr::across(
        .cols = c("n_gold", "n_suggested", "tp", "fn", "fp", "delta_relevance"),
        .fns = sum
      ),
      .groups = "keep"
    )

    im_res_smry <- im_res_smry |>
      dplyr::mutate(
        rprec_deno = pmin(tp + fp, tp + fn + delta_relevance)
      )

    im_res_smry <- dplyr::transmute(
      im_res_smry,
      prec_value = ifelse(tp + fp == 0,
        NA_real_,
        (tp + delta_relevance) / (tp + fp)
      ),
      # compute R-precision as in Manning et al.
      rprec_value = ifelse(rprec_deno == 0,
        NA_real_,
        (tp + delta_relevance) / rprec_deno
      ),
      rec_value = ifelse(tp + fn == 0,
        NA_real_,
        (tp + delta_relevance) / (tp + fn + delta_relevance)
      ),
      # NA handling for F1:
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
      f1_support = 0.5 * (n_suggested + n_gold)
    )
  } else {
    im_res_smry <- dplyr::summarise(
      intermediate_results_regrouped,
      dplyr::across(
        .cols = c("prec", "rprec", "rec", "f1"),
        .fns = list(
          value = ~ sum(.x * .data$label_weight, na.rm = TRUE) /
            sum(as.numeric(!is.na(.x)) * .data$label_weight),
          support = ~ sum(!is.na(.x))
        )
      ),
      .groups = "keep"
    )
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
    values_from = "value"
  )

  dplyr::ungroup(
    dplyr::arrange(im_res_smry_wide, .data$metric)
  )
}
