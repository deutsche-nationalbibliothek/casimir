#' Create a rank per doc_id based on score
#'
#' @param df with columns "score" and "doc_id"
#'
#' @export
#' @returns \code{df} with extra column "rank"
create_rank_col <- function(df) {
  stopifnot(all(c("doc_id", "score") %in% colnames(df)))

  collapse::settransform(df, rank = 1)
  collapse::settransform(
    df,
    rank = collapse::fcumsum(rank, g = doc_id, o = -score)
  )
}

#' @describeIn create_rank_col variant with internal usage of
#'  dplyr rather than collapse library.
create_rank_col_dplyr <- function(df) {
  stopifnot(all(c("doc_id", "score") %in% colnames(df)))

  df |>
    dplyr::group_by(.data$doc_id) |>
    dplyr::mutate(rank = dplyr::row_number(-score)) |>
    dplyr::ungroup()
}
