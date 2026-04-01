#' Plot Treatment Timing Across Units
#'
#' Produces a heatmap (tile plot) showing when each unit receives treatment.
#' Units are grouped by treatment cohort on the y-axis, with time periods on
#' the x-axis and fill indicating treatment status.
#'
#' @param data A data frame containing panel data.
#' @param unit Column name (unquoted) identifying the unit.
#' @param time Column name (unquoted) identifying the time period.
#' @param treat Column name (unquoted) identifying the treatment indicator
#'   (0/1 or logical).
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' plot_treatment_timing(staggered_data, unit, time, treat)
#'
#' @importFrom rlang enquo as_label .data
#' @importFrom dplyr filter summarise group_by left_join mutate
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual labs theme_minimal
#'   theme element_text
#' @importFrom stats reorder
#' @export
plot_treatment_timing <- function(data, unit, time, treat) {
  unit_str <- rlang::as_label(rlang::enquo(unit))
  time_str <- rlang::as_label(rlang::enquo(time))
  treat_str <- rlang::as_label(rlang::enquo(treat))

  # Compute cohort: first treated period per unit
  treated_rows <- data[data[[treat_str]] == 1, ]
  if (nrow(treated_rows) > 0) {
    first_treat <- stats::aggregate(
      stats::as.formula(paste(time_str, "~", unit_str)),
      data = treated_rows,
      FUN = min
    )
    names(first_treat)[2] <- ".first_treat"
  } else {
    first_treat <- data.frame(x = unique(data[[unit_str]]), .first_treat = Inf)
    names(first_treat)[1] <- unit_str
  }

  work <- merge(data, first_treat, by = unit_str, all.x = TRUE)
  work$.first_treat[is.na(work$.first_treat)] <- Inf

  # Create cohort label and reorder units
  work$.cohort_label <- ifelse(
    is.finite(work$.first_treat),
    paste0("Cohort (t=", work$.first_treat, ")"),
    "Never treated"
  )
  work$.unit_ordered <- stats::reorder(
    factor(work[[unit_str]]),
    -work$.first_treat
  )

  work$.treat_factor <- factor(
    work[[treat_str]],
    levels = c(0, 1),
    labels = c("Untreated", "Treated")
  )

  ggplot2::ggplot(work, ggplot2::aes(
    x = .data[[time_str]],
    y = .data$.unit_ordered,
    fill = .data$.treat_factor
  )) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_manual(
      values = c("Untreated" = "#B0C4DE", "Treated" = "#E74C3C"),
      name = "Status"
    ) +
    ggplot2::labs(
      x = time_str,
      y = paste0(unit_str, " (grouped by cohort)"),
      title = "Treatment Timing"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 7),
      panel.grid = ggplot2::element_blank()
    )
}
