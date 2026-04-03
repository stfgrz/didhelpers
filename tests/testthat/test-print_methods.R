test_that("print.pretrends_test produces expected output", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)

  out <- capture.output(print(pt))
  expect_true(any(grepl("Pre-Trends Event-Study Test", out)))
  expect_true(any(grepl("Leads:", out)))
  expect_true(any(grepl("Joint F-test", out)))
  expect_true(any(grepl("Signif\\. codes", out)))
})

test_that("summary.pretrends_test returns correct class and components", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  sm <- summary(pt)

  expect_s3_class(sm, "summary.pretrends_test")
  expect_true(is.numeric(sm$nobs))
  expect_true(is.numeric(sm$r.squared))
  expect_true(is.numeric(sm$adj.r.squared))
  expect_identical(sm$coefficients, pt$coefficients)
  expect_identical(sm$f_test, pt$f_test)
})

test_that("print.summary.pretrends_test works", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  sm <- summary(pt)
  out <- capture.output(print(sm))
  expect_true(any(grepl("Summary", out)))
  expect_true(any(grepl("R-squared", out)))
})

test_that("print.bacon_summary produces expected output", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)

  out <- capture.output(print(bs))
  expect_true(any(grepl("Goodman-Bacon", out)))
  expect_true(any(grepl("TWFE estimate", out)))
  expect_true(any(grepl("autoplot", out)))
})

test_that("summary.bacon_summary returns correct class and components", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
  sm <- summary(bs)

  expect_s3_class(sm, "summary.bacon_summary")
  expect_true(is.numeric(sm$twfe_estimate))
  expect_true(is.numeric(sm$n_comparisons))
  expect_true(is.numeric(sm$max_weight))
  expect_true(is.numeric(sm$hhi))
  expect_true(sm$hhi > 0 && sm$hhi <= 1)
})

test_that("improved print.did_report delegates to sub-methods", {
  report <- did_diagnostics_report(staggered_data, unit, time, outcome, treat)
  out <- capture.output(print(report))

  expect_true(any(grepl("DiD Diagnostics Report", out)))
  expect_true(any(grepl("Pre-Trends Event-Study Test", out)))
  expect_true(any(grepl("Goodman-Bacon", out)))
  expect_true(any(grepl("Treatment timing plot", out)))
})
