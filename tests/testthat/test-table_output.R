test_that("as_table.pretrends_test works with gt", {
  skip_if_not_installed("gt")
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  tbl <- as_table(pt, format = "gt")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("as_table.pretrends_test works with kableExtra", {
  skip_if_not_installed("kableExtra")
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  tbl <- as_table(pt, format = "kable")
  expect_s3_class(tbl, "kableExtra")
})

test_that("as_table.bacon_summary works with gt", {
  skip_if_not_installed("gt")
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
  tbl <- as_table(bs, format = "gt")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("as_table.bacon_summary works with kableExtra", {
  skip_if_not_installed("kableExtra")
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
  tbl <- as_table(bs, format = "kable")
  expect_s3_class(tbl, "kableExtra")
})
