#' Print a Pre-Trends Test
#'
#' Displays a compact summary of an event-study pre-trends test, modelled after
#' the output style of [fixest::print.fixest()]: a ruled header, coefficient
#' table with significance stars, and a footer with the joint F-test.
#'
#' @param x A `pretrends_test` object returned by [test_pretrends()].
#' @param digits Integer. Number of significant digits for estimates. Default 4.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.pretrends_test <- function(x, digits = 4, ...) {
  n_leads <- sum(grepl("^lead_", x$coefficients$term))
  n_lags  <- sum(grepl("^lag_",  x$coefficients$term))
  n_obs   <- stats::nobs(x$model)

  cat("--- Pre-Trends Event-Study Test ---\n")
  cat(sprintf("Leads: %d | Lags: %d | Obs: %s\n\n",
              n_leads, n_lags, formatC(n_obs, big.mark = ",")))

  # Build display table
  coefs <- x$coefficients
  stars <- ifelse(coefs$p_value < 0.001, "***",
           ifelse(coefs$p_value < 0.01,  "**",
           ifelse(coefs$p_value < 0.05,  "*",
           ifelse(coefs$p_value < 0.1,   ".",
                  ""))))

  display <- data.frame(
    Term     = coefs$term,
    Estimate = formatC(coefs$estimate, digits = digits, format = "f"),
    `Std.Err` = formatC(coefs$std_error, digits = digits, format = "f"),
    `t value` = formatC(coefs$statistic, digits = 3, format = "f"),
    `Pr(>|t|)` = formatC(coefs$p_value, digits = 4, format = "f"),
    ` ` = stars,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  print(display, row.names = FALSE, right = TRUE)

  cat(sprintf("\nJoint F-test of pre-treatment leads: F = %.3f, p = %.4f\n",
              x$f_test$statistic, x$f_test$p_value))
  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1\n")
  invisible(x)
}


#' Summarize a Pre-Trends Test
#'
#' Returns a summary object of class `"summary.pretrends_test"` containing
#' the coefficient table, joint F-test results, and model fit statistics.
#' Follows the `summary.lm` / `print.summary.lm` pattern.
#'
#' @param object A `pretrends_test` object returned by [test_pretrends()].
#' @param ... Additional arguments (currently unused).
#'
#' @return An S3 object of class `"summary.pretrends_test"` containing:
#' \describe{
#'   \item{coefficients}{The coefficient [tibble][tibble::tibble].}
#'   \item{f_test}{The joint F-test list.}
#'   \item{nobs}{Number of observations.}
#'   \item{r.squared}{R-squared from the fixed-effects model.}
#'   \item{adj.r.squared}{Adjusted R-squared.}
#' }
#'
#' @export
summary.pretrends_test <- function(object, ...) {
  mod <- object$model
  structure(
    list(
      coefficients  = object$coefficients,
      f_test        = object$f_test,
      nobs          = stats::nobs(mod),
      r.squared     = fixest::r2(mod, "r2"),
      adj.r.squared = fixest::r2(mod, "ar2")
    ),
    class = "summary.pretrends_test"
  )
}


#' @export
print.summary.pretrends_test <- function(x, digits = 4, ...) {
  cat("--- Pre-Trends Event-Study Test (Summary) ---\n\n")
  cat(sprintf("Observations: %s\n", formatC(x$nobs, big.mark = ",")))
  cat(sprintf("R-squared: %.4f | Adj. R-squared: %.4f\n\n",
              x$r.squared, x$adj.r.squared))

  coefs <- x$coefficients
  stars <- ifelse(coefs$p_value < 0.001, "***",
           ifelse(coefs$p_value < 0.01,  "**",
           ifelse(coefs$p_value < 0.05,  "*",
           ifelse(coefs$p_value < 0.1,   ".",
                  ""))))

  display <- data.frame(
    Term     = coefs$term,
    Estimate = formatC(coefs$estimate, digits = digits, format = "f"),
    `Std.Err` = formatC(coefs$std_error, digits = digits, format = "f"),
    `t value` = formatC(coefs$statistic, digits = 3, format = "f"),
    `Pr(>|t|)` = formatC(coefs$p_value, digits = 4, format = "f"),
    ` ` = stars,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  print(display, row.names = FALSE, right = TRUE)

  cat(sprintf("\nJoint F-test of pre-treatment leads: F = %.3f, p = %.4f\n",
              x$f_test$statistic, x$f_test$p_value))
  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1\n")
  invisible(x)
}


# ---- bacon_summary methods ------------------------------------------------

#' Print a Bacon Decomposition Summary
#'
#' Displays a compact summary of the Goodman-Bacon (2021) decomposition,
#' showing comparison types, weights, and the overall TWFE estimate.
#'
#' @param x A `bacon_summary` object returned by [bacon_weight_summary()].
#' @param digits Integer. Number of decimal places. Default 4.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.bacon_summary <- function(x, digits = 4, ...) {
  twfe <- stats::weighted.mean(x$two_by_twos$estimate, x$two_by_twos$weight)
  n_comp <- nrow(x$two_by_twos)

  cat("--- Goodman-Bacon (2021) Decomposition ---\n")
  cat(sprintf("TWFE estimate: %s | 2x2 comparisons: %d\n\n",
              formatC(twfe, digits = digits, format = "f"), n_comp))

  # Format summary by type
  s <- x$summary
  display <- data.frame(
    Type       = s$type,
    N          = s$n_comparisons,
    Weight     = formatC(s$total_weight, digits = 3, format = "f"),
    `Avg Est.` = formatC(s$weighted_avg_estimate, digits = digits, format = "f"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  print(display, row.names = FALSE, right = TRUE)

  cat("\nAccess $two_by_twos for all individual comparisons.\n")
  cat("Use plot() or autoplot() to visualize.\n")
  invisible(x)
}


#' Summarize a Bacon Decomposition
#'
#' Returns an extended summary including the overall TWFE estimate, weight
#' concentration statistics (max weight, HHI), and the per-type breakdown.
#'
#' @param object A `bacon_summary` object returned by [bacon_weight_summary()].
#' @param ... Additional arguments (currently unused).
#'
#' @return An S3 object of class `"summary.bacon_summary"` containing:
#' \describe{
#'   \item{twfe_estimate}{The overall TWFE estimate.}
#'   \item{n_comparisons}{Total number of 2x2 comparisons.}
#'   \item{max_weight}{Weight of the single largest comparison.}
#'   \item{hhi}{Herfindahl-Hirschman Index of weights (0-1 scale).}
#'   \item{by_type}{The per-type summary [tibble][tibble::tibble].}
#' }
#'
#' @export
summary.bacon_summary <- function(object, ...) {
  w <- object$two_by_twos$weight
  structure(
    list(
      twfe_estimate = stats::weighted.mean(object$two_by_twos$estimate, w),
      n_comparisons = nrow(object$two_by_twos),
      max_weight    = max(w),
      hhi           = sum(w^2),
      by_type       = object$summary
    ),
    class = "summary.bacon_summary"
  )
}


#' @export
print.summary.bacon_summary <- function(x, digits = 4, ...) {
  cat("--- Goodman-Bacon Decomposition (Summary) ---\n\n")
  cat(sprintf("TWFE estimate:  %s\n",
              formatC(x$twfe_estimate, digits = digits, format = "f")))
  cat(sprintf("Comparisons:    %d\n", x$n_comparisons))
  cat(sprintf("Max weight:     %s\n",
              formatC(x$max_weight, digits = 4, format = "f")))
  cat(sprintf("HHI of weights: %s\n\n",
              formatC(x$hhi, digits = 4, format = "f")))

  cat("By comparison type:\n")
  s <- x$by_type
  display <- data.frame(
    Type       = s$type,
    N          = s$n_comparisons,
    Weight     = formatC(s$total_weight, digits = 3, format = "f"),
    `Avg Est.` = formatC(s$weighted_avg_estimate, digits = digits, format = "f"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  print(display, row.names = FALSE, right = TRUE)
  cat("\n")
  invisible(x)
}
