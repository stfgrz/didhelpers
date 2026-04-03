#' Create a Publication-Quality Table
#'
#' Generic function that produces a formatted table from a didhelpers object,
#' suitable for inclusion in RMarkdown or Quarto documents.
#'
#' @param x A didhelpers object (e.g., `pretrends_test` or `bacon_summary`).
#' @param ... Additional arguments passed to methods.
#'
#' @return A `gt_tbl` (if `format = "gt"`) or `kableExtra` object
#'   (if `format = "kable"`).
#'
#' @export
as_table <- function(x, ...) {
  UseMethod("as_table")
}


#' @describeIn as_table Table for event-study coefficients with significance
#'   stars and a joint F-test footnote.
#'
#' @param format Character. `"gt"` (default) or `"kable"`.
#' @param digits Integer. Number of decimal places for numeric columns.
#'
#' @export
as_table.pretrends_test <- function(x, format = c("gt", "kable"),
                                     digits = 3, ...) {
  format <- match.arg(format)

  coefs <- tidy(x, conf.int = TRUE)
  coefs$stars <- ifelse(coefs$p.value < 0.001, "***",
                 ifelse(coefs$p.value < 0.01,  "**",
                 ifelse(coefs$p.value < 0.05,  "*",
                 ifelse(coefs$p.value < 0.1,   ".", ""))))

  footnote_text <- sprintf(
    "Joint F-test of pre-treatment leads: F = %.3f, p = %.4f. Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1",
    x$f_test$statistic, x$f_test$p_value
  )

  if (format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required. Install with install.packages('gt').",
           call. = FALSE)
    }
    gt::gt(coefs) |>
      gt::fmt_number(
        columns = c("estimate", "std.error", "statistic",
                     "conf.low", "conf.high"),
        decimals = digits
      ) |>
      gt::fmt_number(columns = "p.value", decimals = 4) |>
      gt::tab_header(title = "Event-Study Coefficients") |>
      gt::tab_footnote(footnote = footnote_text) |>
      gt::cols_label(
        term = "Term", estimate = "Estimate", std.error = "Std. Error",
        statistic = "t value", p.value = "Pr(>|t|)",
        conf.low = "CI Low", conf.high = "CI High", stars = " "
      )
  } else {
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("Package 'kableExtra' is required. Install with install.packages('kableExtra').",
           call. = FALSE)
    }
    kableExtra::kbl(coefs, digits = digits,
                    caption = "Event-Study Coefficients") |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "condensed"),
        full_width = FALSE
      ) |>
      kableExtra::footnote(general = footnote_text)
  }
}


#' @describeIn as_table Table for the Goodman-Bacon decomposition summary
#'   by comparison type.
#'
#' @export
as_table.bacon_summary <- function(x, format = c("gt", "kable"),
                                    digits = 3, ...) {
  format <- match.arg(format)
  summ <- x$summary
  twfe <- stats::weighted.mean(x$two_by_twos$estimate, x$two_by_twos$weight)

  footnote_text <- sprintf(
    "Overall TWFE estimate: %.4f. Based on %d 2x2 comparisons.",
    twfe, nrow(x$two_by_twos)
  )

  if (format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required. Install with install.packages('gt').",
           call. = FALSE)
    }
    gt::gt(summ) |>
      gt::fmt_number(
        columns = c("total_weight", "weighted_avg_estimate"),
        decimals = digits
      ) |>
      gt::tab_header(title = "Goodman-Bacon Decomposition Summary") |>
      gt::tab_footnote(footnote = footnote_text) |>
      gt::cols_label(
        type = "Comparison Type",
        n_comparisons = "N",
        total_weight = "Weight",
        weighted_avg_estimate = "Wtd. Avg. Estimate"
      )
  } else {
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("Package 'kableExtra' is required. Install with install.packages('kableExtra').",
           call. = FALSE)
    }
    kableExtra::kbl(summ, digits = digits,
                    caption = "Goodman-Bacon Decomposition Summary") |>
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "condensed"),
        full_width = FALSE
      ) |>
      kableExtra::footnote(general = footnote_text)
  }
}
