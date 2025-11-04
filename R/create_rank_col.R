#' Create a rank column
#'
#' Create a rank per document id based on score.
#'
#' @param df A data.frame with columns \code{"doc_id", "score"}.
#'
#' @export
#' @return The input data.frame \code{df} with an additional column
#'   \code{"rank"}.
create_rank_col <- function(df) {
  stopifnot(all(c("doc_id", "score") %in% colnames(df)))

  collapse::settransform(df, rank = 1)
  collapse::settransform(
    df,
    rank = collapse::fcumsum(rank, g = doc_id, o = -score)
  )
}

#' @describeIn create_rank_col Variant with internal usage of
#'  dplyr rather than collapse library.
create_rank_col_dplyr <- function(df) {
  stopifnot(all(c("doc_id", "score") %in% colnames(df)))

  df |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::mutate(rank = dplyr::row_number(-score)) |>
    dplyr::ungroup()
}
