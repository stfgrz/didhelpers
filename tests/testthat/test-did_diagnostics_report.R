test_that("did_diagnostics_report returns correct structure", {
  report <- did_diagnostics_report(
    staggered_data, unit, time, outcome, treat
  )

  expect_s3_class(report, "did_report")
  expect_named(report, c("treatment_plot", "pretrends", "bacon"))

  expect_s3_class(report$treatment_plot, "ggplot")
  expect_s3_class(report$pretrends, "pretrends_test")
  expect_s3_class(report$bacon, "bacon_summary")
})

test_that("print.did_report produces output", {
  report <- did_diagnostics_report(
    staggered_data, unit, time, outcome, treat
  )
  expect_output(print(report), "DiD Diagnostics Report")
})
