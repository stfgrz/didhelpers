test_that("test_pretrends returns correct structure", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)

  expect_s3_class(pt, "pretrends_test")
  expect_named(pt, c("coefficients", "f_test", "model"))

  # Coefficients tibble

  expect_s3_class(pt$coefficients, "tbl_df")
  expect_true(all(c(
    "term", "estimate", "std_error", "statistic",
    "p_value", "rel_time"
  ) %in% names(pt$coefficients)))

  # Should have lead and lag terms
  terms <- pt$coefficients$term
  expect_true(any(grepl("^lead_", terms)))
  expect_true(any(grepl("^lag_", terms)))

  # F-test
  expect_true(is.numeric(pt$f_test$statistic))
  expect_true(is.numeric(pt$f_test$p_value))
})

test_that("autoplot.pretrends_test returns a ggplot", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  p <- ggplot2::autoplot(pt)
  expect_s3_class(p, "ggplot")
})

test_that("pre-treatment leads are insignificant for data with no pre-trends", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  leads <- pt$coefficients[grepl("^lead_", pt$coefficients$term), ]

  # With no true pre-trends, individual lead p-values should generally be > 0.01
  # (this is a probabilistic test; with seed=42 in simulation it should hold)
  expect_true(all(leads$p_value > 0.01))
})
