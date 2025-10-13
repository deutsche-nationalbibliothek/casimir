test_that("rank column is created correctly", {
  df <- tibble::tribble(
    ~doc_id, ~score, ~exp_rank,
    "a", 0.9, 1,
    "a", 0.7, 3,
    "a", 0.8, 2,
    "a", 0.6, 4,
    "c", 0.3, 1,
    "b", 0.5, 2,
    "b", 0.6, 1,
    "c", 0.3, 2,
    "c", 0.1, 3
  )

  res <- create_rank_col(df)

  expect_equal(res$rank, df$exp_rank)
})

test_that("dplyr and collapse version agree", {
  df_a <- create_rank_col(dnb_test_predictions)

  df_b <- casimir:::create_rank_col_dplyr(dnb_test_predictions)

  expect_equal(df_a$rank, df_b$rank)
})
