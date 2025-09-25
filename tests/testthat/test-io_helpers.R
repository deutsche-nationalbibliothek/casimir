test_that("grouping_var selection works", {

  # test that function remains siltnt across all possible configs
  config <- expand.grid(
    mode = c("micro", "subj-avg", "doc-avg"),
    doc_strata = list(NULL, c("hsg"), c("hsg", "grp2")),
    label_dict = list(NULL,
                      tibble::tibble(label_id = c("A", "B"),
                                     label_grp = c("1", "2"))),
    var = list(NULL, c("abc"), c("abc", "def"))
  )

  messages <- config |>
    purrr::pmap(
      .f = function(mode, doc_strata, label_dict, var) {
        expect_silent(
          set_grouping_var(mode = mode,
                           doc_strata = doc_strata,
                           label_dict = label_dict,
                           var = var))
      }
    )

})

test_that("set_ps_flags is correct", {

  expect_equal(
    set_ps_flags(mode = "subj-avg", propensity_scored = TRUE),
    list(intermed = FALSE, summarise = TRUE)
  )

  expect_equal(
    set_ps_flags(mode = "doc-avg", propensity_scored = TRUE),
    list(intermed = TRUE, summarise = FALSE)
  )

  expect_equal(
    set_ps_flags(mode = "micro", propensity_scored = TRUE),
    list(intermed = TRUE, summarise = FALSE)
  )

})
