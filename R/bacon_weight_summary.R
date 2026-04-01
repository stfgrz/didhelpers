#' Goodman-Bacon Decomposition Summary
#'
#' A convenience wrapper around [bacondecomp::bacon()] that returns a clean
#' summary of the Goodman-Bacon (2021) decomposition and a ready-made
#' scatter plot of 2x2 DD estimates versus their weights, colored by
#' comparison type.
#'
#' @param data A data frame containing balanced panel data.
#' @param unit Column name (unquoted) identifying the unit.
#' @param time Column name (unquoted) identifying the time period.
#' @param outcome Column name (unquoted) for the outcome variable.
#' @param treat Column name (unquoted) for the treatment indicator (0/1).
#'
#' @return An S3 object of class `"bacon_summary"` containing:
#' \describe{
#'   \item{two_by_twos}{A [tibble][tibble::tibble] of all 2x2 comparisons
#'     with columns from [bacondecomp::bacon()] (`treated`, `untreated`,
#'     `estimate`, `weight`, `type`).}
#'   \item{summary}{A [tibble][tibble::tibble] with one row per comparison
#'     type, showing `type`, `n_comparisons`, `total_weight`, and
#'     `weighted_avg_estimate`.}
#'   \item{plot}{A [ggplot2::ggplot] scatter plot of estimates vs. weights.}
#' }
#'
#' @examples
#' bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
#' bs$summary
#' bs$plot
#'
#' @importFrom rlang enquo as_label
#' @importFrom bacondecomp bacon
#' @importFrom dplyr group_by summarise n
#' @importFrom stats as.formula weighted.mean
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal scale_color_brewer
#' @export
bacon_weight_summary <- function(data, unit, time, outcome, treat) {
  unit_str <- rlang::as_label(rlang::enquo(unit))
  time_str <- rlang::as_label(rlang::enquo(time))
  outcome_str <- rlang::as_label(rlang::enquo(outcome))
  treat_str <- rlang::as_label(rlang::enquo(treat))

  # Build formula and run bacon decomposition
  fml <- stats::as.formula(paste(outcome_str, "~", treat_str))
  bacon_out <- bacondecomp::bacon(
    formula = fml,
    data = as.data.frame(data),
    id_var = unit_str,
    time_var = time_str,
    quietly = TRUE
  )

  two_by_twos <- tibble::as_tibble(bacon_out)

  # Summary by comparison type
  summary_tbl <- dplyr::group_by(two_by_twos, .data$type)
  summary_tbl <- dplyr::summarise(
    summary_tbl,
    n_comparisons = dplyr::n(),
    total_weight = sum(.data$weight),
    weighted_avg_estimate = stats::weighted.mean(.data$estimate, .data$weight),
    .groups = "drop"
  )

  # Scatter plot
  p <- ggplot2::ggplot(
    two_by_twos,
    ggplot2::aes(
      x = .data$weight,
      y = .data$estimate,
      color = .data$type
    )
  ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::labs(
      x = "Weight",
      y = "2x2 DD Estimate",
      color = "Comparison type",
      title = "Goodman-Bacon Decomposition"
    ) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_minimal()

  structure(
    list(
      two_by_twos = two_by_twos,
      summary = summary_tbl,
      plot = p
    ),
    class = "bacon_summary"
  )
}
