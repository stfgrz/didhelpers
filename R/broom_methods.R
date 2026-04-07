# Re-export generics --------------------------------------------------------
# Following the tidymodels convention: import from {generics} and re-export,
# so users get tidy(), glance(), augment() without loading {broom} directly.

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment


# ---- pretrends_test methods -----------------------------------------------

#' Tidy a Pre-Trends Test
#'
#' Extracts a tidy coefficient table from a [test_pretrends()] result,
#' following [broom][broom::broom-package] column naming conventions.
#' This enables seamless integration with [modelsummary][modelsummary::modelsummary()].
#'
#' @param x A `pretrends_test` object.
#' @param conf.int Logical. If `TRUE` (default), include confidence interval
#'   columns `conf.low` and `conf.high`.
#' @param conf.level Confidence level for intervals. Default 0.95.
#' @param ... Additional arguments (currently unused).
#'
#' @return A [tibble][tibble::tibble] with columns `term`, `estimate`,
#'   `std.error`, `statistic`, `p.value`, and optionally `conf.low` and
#'   `conf.high`.
#'
#' @export
tidy.pretrends_test <- function(x, conf.int = TRUE, conf.level = 0.95, ...) {
  coefs <- x$coefficients
  z <- stats::qnorm(1 - (1 - conf.level) / 2)

  result <- tibble::tibble(
    term      = coefs$term,
    estimate  = coefs$estimate,
    std.error = coefs$std_error,
    statistic = coefs$statistic,
    p.value   = coefs$p_value
  )

  if (conf.int) {
    result$conf.low  <- result$estimate - z * result$std.error
    result$conf.high <- result$estimate + z * result$std.error
  }

  result
}


#' Glance at a Pre-Trends Test
#'
#' Returns a one-row [tibble][tibble::tibble] of model-level summary
#' statistics, including the joint F-test for pre-treatment leads.
#'
#' @param x A `pretrends_test` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A one-row [tibble][tibble::tibble] with columns `nobs`,
#'   `r.squared`, `adj.r.squared`, `f_statistic`, `f_p_value`,
#'   `n_leads`, and `n_lags`.
#'
#' @export
glance.pretrends_test <- function(x, ...) {
  mod <- x$model

  tibble::tibble(
    nobs          = as.integer(stats::nobs(mod)),
    r.squared     = fixest::r2(mod, "r2"),
    adj.r.squared = fixest::r2(mod, "ar2"),
    f_statistic   = x$f_test$statistic,
    f_p_value     = x$f_test$p_value,
    n_leads       = sum(grepl("^lead_", x$coefficients$term)),
    n_lags        = sum(grepl("^lag_",  x$coefficients$term))
  )
}


#' Augment a Pre-Trends Test
#'
#' Returns fitted values and residuals from the underlying event-study
#' regression model.
#'
#' @param x A `pretrends_test` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A [tibble][tibble::tibble] with columns `.fitted` and `.resid`.
#'
#' @note The underlying model is estimated on an internal working data frame
#'   (with computed relative-time dummies) that is not stored in the returned
#'   object. Only `.fitted` and `.resid` from the model are available.
#'
#' @export
augment.pretrends_test <- function(x, ...) {
  mod <- x$model
  tibble::tibble(
    .fitted = stats::fitted(mod),
    .resid  = stats::residuals(mod)
  )
}


# ---- bacon_summary methods ------------------------------------------------

#' Tidy a Bacon Decomposition
#'
#' Extracts a tidy table from a [bacon_weight_summary()] result, either the
#' full set of 2x2 comparisons or the per-type summary.
#'
#' @param x A `bacon_summary` object.
#' @param type Character. `"two_by_twos"` (default) returns all individual
#'   comparisons; `"summary"` returns the per-type aggregation.
#' @param ... Additional arguments (currently unused).
#'
#' @return A [tibble][tibble::tibble].
#'
#' @export
tidy.bacon_summary <- function(x, type = c("two_by_twos", "summary"), ...) {
  type <- match.arg(type)
  if (type == "two_by_twos") {
    x$two_by_twos
  } else {
    x$summary
  }
}


#' Glance at a Bacon Decomposition
#'
#' Returns a one-row [tibble][tibble::tibble] of decomposition-level
#' summary statistics.
#'
#' @param x A `bacon_summary` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A one-row [tibble][tibble::tibble] with columns `twfe_estimate`,
#'   `n_comparisons`, `n_types`, and `max_weight`.
#'
#' @export
glance.bacon_summary <- function(x, ...) {
  tibble::tibble(
    twfe_estimate = stats::weighted.mean(x$two_by_twos$estimate,
                                         x$two_by_twos$weight),
    n_comparisons = nrow(x$two_by_twos),
    n_types       = nrow(x$summary),
    max_weight    = max(x$two_by_twos$weight)
  )
}
