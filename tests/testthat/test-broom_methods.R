test_that("tidy.pretrends_test returns correct columns", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  td <- tidy(pt)

  expect_s3_class(td, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                     "conf.low", "conf.high") %in% names(td)))
  expect_equal(nrow(td), nrow(pt$coefficients))
})

test_that("tidy.pretrends_test conf.int = FALSE omits CI columns", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  td <- tidy(pt, conf.int = FALSE)

  expect_false("conf.low" %in% names(td))
  expect_false("conf.high" %in% names(td))
})

test_that("glance.pretrends_test returns one-row tibble", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  gl <- glance(pt)

  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1)
  expect_true(all(c("nobs", "r.squared", "adj.r.squared",
                     "f_statistic", "f_p_value",
                     "n_leads", "n_lags") %in% names(gl)))
  expect_true(gl$nobs > 0)
})

test_that("augment.pretrends_test returns fitted and residuals", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  ag <- augment(pt)

  expect_s3_class(ag, "tbl_df")
  expect_true(all(c(".fitted", ".resid") %in% names(ag)))
  expect_equal(nrow(ag), stats::nobs(pt$model))
})

test_that("tidy.bacon_summary works with both type arguments", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)

  td_full <- tidy(bs, type = "two_by_twos")
  expect_s3_class(td_full, "tbl_df")
  expect_true("estimate" %in% names(td_full))
  expect_true("weight" %in% names(td_full))

  td_summ <- tidy(bs, type = "summary")
  expect_s3_class(td_summ, "tbl_df")
  expect_true("total_weight" %in% names(td_summ))
  expect_true("weighted_avg_estimate" %in% names(td_summ))
})

test_that("glance.bacon_summary returns one-row tibble", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
  gl <- glance(bs)

  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1)
  expect_true(all(c("twfe_estimate", "n_comparisons", "n_types",
                     "max_weight") %in% names(gl)))
})
