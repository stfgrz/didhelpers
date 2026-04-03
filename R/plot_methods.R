#' Plot a Pre-Trends Test
#'
#' Thin wrapper around [autoplot.pretrends_test()] that prints the event-study
#' coefficient plot to the current graphics device.
#'
#' @param x A `pretrends_test` object returned by [test_pretrends()].
#' @param ... Additional arguments passed to [autoplot.pretrends_test()].
#'
#' @return Invisibly returns `x`.
#'
#' @importFrom ggplot2 autoplot
#' @export
plot.pretrends_test <- function(x, ...) {
  print(ggplot2::autoplot(x, ...))
  invisible(x)
}


#' Autoplot for Bacon Decomposition
#'
#' Creates a scatter plot of 2x2 DD estimates versus their weights, colored by
#' comparison type. Includes a dashed reference line at zero. The plot is built
#' from data (not the stored `$plot` element) so users can add ggplot2 layers.
#'
#' @param object A `bacon_summary` object returned by [bacon_weight_summary()].
#' @param ... Additional arguments (currently unused).
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom ggplot2 autoplot ggplot aes geom_hline geom_point labs
#'   scale_color_brewer theme_minimal
#' @importFrom rlang .data
#' @export
autoplot.bacon_summary <- function(object, ...) {
  two_by_twos <- object$two_by_twos
  ggplot2::ggplot(two_by_twos, ggplot2::aes(
    x = .data$weight,
    y = .data$estimate,
    color = .data$type
  )) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::labs(
      x = "Weight",
      y = "2x2 DD Estimate",
      color = "Comparison Type",
      title = "Goodman-Bacon Decomposition"
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_minimal()
}


#' Plot a Bacon Decomposition
#'
#' Thin wrapper around [autoplot.bacon_summary()] that prints the scatter plot
#' to the current graphics device.
#'
#' @param x A `bacon_summary` object returned by [bacon_weight_summary()].
#' @param ... Additional arguments passed to [autoplot.bacon_summary()].
#'
#' @return Invisibly returns `x`.
#'
#' @export
plot.bacon_summary <- function(x, ...) {
  print(ggplot2::autoplot(x, ...))
  invisible(x)
}


#' Plot a Full DiD Diagnostics Report
#'
#' Arranges the treatment timing heatmap, event-study coefficient plot, and
#' Bacon decomposition scatter plot into a single composite figure using
#' [patchwork][patchwork::patchwork-package].
#'
#' @param x A `did_report` object returned by [did_diagnostics_report()].
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
plot.did_report <- function(x, ...) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop(
      "Package 'patchwork' is required for plot.did_report(). ",
      "Install it with install.packages('patchwork').",
      call. = FALSE
    )
  }

  p1 <- x$treatment_plot
  p2 <- ggplot2::autoplot(x$pretrends)
  p3 <- ggplot2::autoplot(x$bacon)

  combined <- p1 / (p2 | p3) +
    patchwork::plot_annotation(title = "DiD Diagnostics Report")

  print(combined)
  invisible(x)
}
