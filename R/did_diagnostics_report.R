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
#' Prints a structured console summary of all DiD diagnostics, delegating to
#' [print.pretrends_test()] and [print.bacon_summary()] for each section.
#'
#' @param x A `did_report` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.did_report <- function(x, ...) {
  width <- getOption("width", 80)
  rule  <- paste(rep("=", min(width, 60)), collapse = "")

  cat(rule, "\n")
  cat("DiD Diagnostics Report\n")
  cat(rule, "\n\n")

  cat("[1] Treatment timing plot: ready (access via $treatment_plot)\n")
  cat("    Use plot() on this report to display all figures.\n\n")

  cat("[2] Pre-trends test\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  print(x$pretrends, ...)
  cat("\n")

  cat("[3] Bacon decomposition\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  print(x$bacon, ...)

  cat(rule, "\n")
  invisible(x)
}
