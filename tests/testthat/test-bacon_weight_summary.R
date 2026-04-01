test_that("bacon_weight_summary returns correct structure", {
  bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)

  expect_s3_class(bs, "bacon_summary")
  expect_named(bs, c("two_by_twos", "summary", "plot"))

  # two_by_twos tibble

  expect_s3_class(bs$two_by_twos, "tbl_df")
  expect_true(all(c("estimate", "weight", "type") %in% names(bs$two_by_twos)))

  # Summary tibble
  expect_s3_class(bs$summary, "tbl_df")
  expect_true("total_weight" %in% names(bs$summary))

  # Weights should sum to approximately 1
  expect_equal(sum(bs$two_by_twos$weight), 1, tolerance = 0.01)

  # Plot
  expect_s3_class(bs$plot, "ggplot")
})
