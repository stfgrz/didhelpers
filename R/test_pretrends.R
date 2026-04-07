#' Test for Pre-Trends via Event-Study Regression
#'
#' Runs an event-study regression with unit and time fixed effects using
#' [fixest::feols()]. Returns a tidy table of pre-treatment lead coefficients
#' (with SEs and p-values) and a joint F-test for all pre-treatment
#' coefficients being zero. An [autoplot][autoplot.pretrends_test] method
#' produces the classic event-study coefficient plot.
#'
#' The reference period is one period before treatment (relative time = -1),
#' which is omitted from the regression. Lead coefficients capture
#' pre-treatment dynamics: if the parallel trends assumption holds, these
#' should be close to zero.
#'
#' @param data A data frame containing balanced panel data.
#' @param unit Column name (unquoted) identifying the unit.
#' @param time Column name (unquoted) identifying the time period.
#' @param outcome Column name (unquoted) for the outcome variable.
#' @param treat Column name (unquoted) for the treatment indicator (0/1).
#' @param n_lags Integer. Number of leads (pre-treatment) and lags
#'   (post-treatment) to include. Default is 3.
#'
#' @return An S3 object of class `"pretrends_test"` containing:
#' \describe{
#'   \item{coefficients}{A [tibble][tibble::tibble] with columns `term`,
#'     `estimate`, `std_error`, `statistic`, `p_value`, and `rel_time`.}
#'   \item{f_test}{A list with elements `statistic` and `p_value` from the
#'     joint Wald test of all pre-treatment lead coefficients.}
#'   \item{model}{The fitted [fixest::feols] model object.}
#' }
#'
#' @examples
#' pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
#' pt$coefficients
#' pt$f_test
#'
#' @importFrom rlang enquo as_label
#' @importFrom fixest feols wald
#' @importFrom tibble tibble as_tibble
#' @importFrom stats as.formula
#' @export
test_pretrends <- function(data, unit, time, outcome, treat, n_lags = 3) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)

  unit_str <- rlang::as_label(rlang::enquo(unit))
  time_str <- rlang::as_label(rlang::enquo(time))
  outcome_str <- rlang::as_label(rlang::enquo(outcome))
  treat_str <- rlang::as_label(rlang::enquo(treat))

  required <- c(unit_str, time_str, outcome_str, treat_str)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in `data`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!all(data[[treat_str]] %in% c(0L, 1L, 0, 1))) {
    stop("`", treat_str, "` must be a binary 0/1 indicator.", call. = FALSE)
  }

  if (!is.numeric(n_lags) || length(n_lags) != 1 || n_lags < 1) {
    stop("`n_lags` must be a positive integer.", call. = FALSE)
  }

  # Compute relative time
  treated_rows <- data[data[[treat_str]] == 1, ]
  first_treat <- stats::aggregate(
    stats::as.formula(paste(time_str, "~", unit_str)),
    data = treated_rows,
    FUN = min
  )
  names(first_treat)[2] <- ".first_treat"

  work <- merge(data, first_treat, by = unit_str, all.x = TRUE)
  work$.rel_time <- work[[time_str]] - work$.first_treat

  # Create lead and lag dummies (omit rel_time = -1 as reference)
  lead_names <- character(0)
  lag_names <- character(0)

  for (k in n_lags:2) {
    col_name <- paste0("lead_", k)
    work[[col_name]] <- as.integer(!is.na(work$.rel_time) & work$.rel_time == -k)
    lead_names <- c(lead_names, col_name)
  }

  for (k in 0:n_lags) {
    col_name <- paste0("lag_", k)
    work[[col_name]] <- as.integer(!is.na(work$.rel_time) & work$.rel_time == k)
    lag_names <- c(lag_names, col_name)
  }

  # Bin endpoints: lead_n_lags includes all rel_time <= -n_lags
  bin_col <- paste0("lead_", n_lags)
  work[[bin_col]] <- as.integer(
    !is.na(work$.rel_time) & work$.rel_time <= -n_lags
  )
  # lag_n_lags includes all rel_time >= n_lags
  bin_col_lag <- paste0("lag_", n_lags)
  work[[bin_col_lag]] <- as.integer(
    !is.na(work$.rel_time) & work$.rel_time >= n_lags
  )

  # Build formula
  all_terms <- c(lead_names, lag_names)
  fml_str <- paste0(
    outcome_str, " ~ ",
    paste(all_terms, collapse = " + "),
    " | ", unit_str, " + ", time_str
  )
  fml <- stats::as.formula(fml_str)

  # Estimate
  model <- fixest::feols(fml, data = work)

  # Extract coefficients
  ct <- summary(model)$coeftable
  coef_df <- data.frame(
    term = rownames(ct),
    estimate = ct[, 1],
    std_error = ct[, 2],
    statistic = ct[, 3],
    p_value = ct[, 4],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Add rel_time column
  coef_df$rel_time <- NA_real_
  for (i in seq_len(nrow(coef_df))) {
    nm <- coef_df$term[i]
    if (grepl("^lead_", nm)) {
      coef_df$rel_time[i] <- -as.numeric(sub("lead_", "", nm))
    } else if (grepl("^lag_", nm)) {
      coef_df$rel_time[i] <- as.numeric(sub("lag_", "", nm))
    }
  }

  coef_tbl <- tibble::as_tibble(coef_df)

  # Joint F-test on pre-treatment leads
  f_result <- fixest::wald(model, keep = "lead")
  f_test <- list(
    statistic = f_result$stat,
    p_value = f_result$p
  )

  structure(
    list(
      coefficients = coef_tbl,
      f_test = f_test,
      model = model
    ),
    class = "pretrends_test"
  )
}

#' Plot Event-Study Coefficients
#'
#' Draws the classic event-study coefficient plot with confidence intervals
#' for a `pretrends_test` object. The omitted reference period (relative
#' time = -1) is shown as a point at zero.
#'
#' @param object A `pretrends_test` object returned by [test_pretrends()].
#' @param conf.level Confidence level for intervals. Default 0.95.
#' @param ... Additional arguments (currently unused).
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' pt <- test_pretrends(staggered_data, unit, time, outcome, treat, n_lags = 3)
#' ggplot2::autoplot(pt)
#' ggplot2::autoplot(pt, conf.level = 0.90)
#'
#' @importFrom ggplot2 autoplot ggplot aes geom_pointrange geom_hline geom_vline
#'   geom_point labs theme_minimal
#' @export
autoplot.pretrends_test <- function(object, conf.level = 0.95, ...) {
  coefs <- object$coefficients
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  ci_label <- paste0(round(conf.level * 100), "%")

  # Add the omitted reference period
  ref_row <- tibble::tibble(
    term = "reference",
    estimate = 0,
    std_error = 0,
    statistic = NA_real_,
    p_value = NA_real_,
    rel_time = -1
  )
  plot_data <- rbind(coefs, ref_row)
  plot_data <- plot_data[order(plot_data$rel_time), ]
  plot_data$ci_lower <- plot_data$estimate - z * plot_data$std_error
  plot_data$ci_upper <- plot_data$estimate + z * plot_data$std_error
  plot_data$is_ref <- plot_data$rel_time == -1

  ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data$rel_time,
    y = .data$estimate,
    ymin = .data$ci_lower,
    ymax = .data$ci_upper
  )) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey70") +
    ggplot2::geom_pointrange(
      data = plot_data[!plot_data$is_ref, ],
      size = 0.4
    ) +
    ggplot2::geom_point(
      data = plot_data[plot_data$is_ref, ],
      shape = 17, size = 3, color = "red"
    ) +
    ggplot2::labs(
      x = "Relative time to treatment",
      y = "Estimate",
      title = "Event-Study Coefficients",
      caption = paste0("Red triangle = omitted reference period (t = -1). ",
                       "Bars = ", ci_label, " CI.")
    ) +
    ggplot2::theme_minimal()
}
