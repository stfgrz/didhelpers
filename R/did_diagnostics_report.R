#' Run All DiD Diagnostics
#'
#' Convenience function that runs [plot_treatment_timing()],
#' [test_pretrends()], and [bacon_weight_summary()] and returns the results
#' in a single list. A `print` method provides a concise console summary.
#'
#' @inheritParams test_pretrends
#'
#' @return An S3 object of class `"did_report"` containing:
#' \describe{
#'   \item{treatment_plot}{A [ggplot2::ggplot] treatment timing heatmap.}
#'   \item{pretrends}{A `pretrends_test` object from [test_pretrends()].}
#'   \item{bacon}{A `bacon_summary` object from [bacon_weight_summary()].}
#' }
#'
#' @examples
#' report <- did_diagnostics_report(staggered_data, unit, time, outcome, treat)
#' report
#'
#' @export
did_diagnostics_report <- function(data, unit, time, outcome, treat,
                                   n_lags = 3) {
  tp <- plot_treatment_timing(data, {{ unit }}, {{ time }}, {{ treat }})
  pt <- test_pretrends(
    data, {{ unit }}, {{ time }}, {{ outcome }}, {{ treat }},
    n_lags = n_lags
  )
  bs <- bacon_weight_summary(
    data, {{ unit }}, {{ time }}, {{ outcome }}, {{ treat }}
  )

  structure(
    list(
      treatment_plot = tp,
      pretrends = pt,
      bacon = bs
    ),
    class = "did_report"
  )
}

#' Print a DiD Diagnostics Report
#'
#' @param x A `did_report` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.did_report <- function(x, ...) {
  cat("=== DiD Diagnostics Report ===\n\n")

  cat("1. Treatment timing plot: ready (access via $treatment_plot)\n\n")

  cat("2. Pre-trends test:\n")
  pre_coefs <- x$pretrends$coefficients
  pre_leads <- pre_coefs[grepl("^lead_", pre_coefs$term), ]
  cat("   Lead coefficients:\n")
  for (i in seq_len(nrow(pre_leads))) {
    cat(sprintf(
      "     %s: %.4f (SE = %.4f, p = %.3f)\n",
      pre_leads$term[i],
      pre_leads$estimate[i],
      pre_leads$std_error[i],
      pre_leads$p_value[i]
    ))
  }
  cat(sprintf(
    "   Joint F-test: stat = %.3f, p = %.4f\n\n",
    x$pretrends$f_test$statistic,
    x$pretrends$f_test$p_value
  ))

  cat("3. Bacon decomposition:\n")
  bs <- x$bacon$summary
  for (i in seq_len(nrow(bs))) {
    cat(sprintf(
      "   %s: weight = %.3f, avg estimate = %.3f (%d comparisons)\n",
      bs$type[i],
      bs$total_weight[i],
      bs$weighted_avg_estimate[i],
      bs$n_comparisons[i]
    ))
  }
  cat("\n")

  invisible(x)
}
