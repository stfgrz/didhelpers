test_that("plot_treatment_timing returns a ggplot", {
  p <- plot_treatment_timing(staggered_data, unit, time, treat)
  expect_s3_class(p, "ggplot")
})

test_that("plot_treatment_timing works with different column names", {
  df <- staggered_data
  names(df)[names(df) == "unit"] <- "id"
  names(df)[names(df) == "treat"] <- "d"
  p <- plot_treatment_timing(df, id, time, d)
  expect_s3_class(p, "ggplot")
})
