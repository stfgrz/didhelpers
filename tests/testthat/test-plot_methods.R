test_that("plot.pretrends_test produces a ggplot", {
  pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
  # plot() invisibly returns the object, but also prints a ggplot
  result <- plot(pt)
  expect_s3_class(result, "pretrends_test")
})

test_that("autoplot.bacon_summary returns a ggplot", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
  p <- ggplot2::autoplot(bs)
  expect_s3_class(p, "ggplot")
})

test_that("plot.bacon_summary produces a ggplot", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
  result <- plot(bs)
  expect_s3_class(result, "bacon_summary")
})

test_that("plot.did_report requires patchwork", {
  skip_if_not_installed("patchwork")
  report <- did_diagnostics_report(staggered_data, unit, time, outcome, treat)
  result <- plot(report)
  expect_s3_class(result, "did_report")
})
