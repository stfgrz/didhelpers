test_that("export_pretrends_table requires modelsummary", {
  skip_if_not_installed("modelsummary")
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)

  # Should run without error and return a table object
  result <- export_pretrends_table(pt, output = "default")
  expect_true(!is.null(result))
})

test_that("export_pretrends_table rejects non-pretrends objects", {
  skip_if_not_installed("modelsummary")
  expect_error(
    export_pretrends_table(list(a = 1)),
    "pretrends_test"
  )
})

test_that("export_pretrends_table produces LaTeX string", {
  skip_if_not_installed("modelsummary")
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)

  result <- export_pretrends_table(pt, output = "latex")
  expect_true(is.character(result))
  expect_true(any(grepl("tabular", result)))
})
