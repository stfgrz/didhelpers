#' Export an Event-Study Regression Table
#'
#' Convenience wrapper around [modelsummary::modelsummary()] for exporting
#' a pre-trends test as a publication-quality regression table to LaTeX, Word,
#' HTML, or other formats.
#'
#' @section Why modelsummary?
#' `modelsummary` is recommended over `stargazer` (unmaintained, no S3
#' extensibility) and `huxtable` (less ecosystem integration). It uses
#' [generics::tidy()] and [generics::glance()] methods automatically, supports
#' all output formats, and is actively maintained.
#'
#' @param x A `pretrends_test` object returned by [test_pretrends()].
#' @param output Character. Output format passed to
#'   [modelsummary::modelsummary()]. Common values: `"default"` (interactive),
#'   `"gt"`, `"kableExtra"`, `"latex"`, `"markdown"`, or a file path ending
#'   in `.docx`, `.html`, `.tex`, `.png`, etc.
#' @param ... Additional arguments passed to [modelsummary::modelsummary()].
#'
#' @return The return value of [modelsummary::modelsummary()] (varies by
#'   `output` format).
#'
#' @examples
#' \dontrun{
#' pt <- test_pretrends(staggered_data, unit, time, outcome, treat)
#'
#' # Interactive display (gt or kableExtra depending on environment)
#' export_pretrends_table(pt)
#'
#' # LaTeX string
#' export_pretrends_table(pt, output = "latex")
#'
#' # Save to Word
#' export_pretrends_table(pt, output = "my_table.docx")
#' }
#'
#' @export
export_pretrends_table <- function(x, output = "default", ...) {
  if (!requireNamespace("modelsummary", quietly = TRUE)) {
    stop(
      "Package 'modelsummary' is required. ",
      "Install with install.packages('modelsummary').",
      call. = FALSE
    )
  }
  if (!inherits(x, "pretrends_test")) {
    stop("x must be a 'pretrends_test' object from test_pretrends().",
         call. = FALSE)
  }

  modelsummary::modelsummary(
    list("Event-Study" = x),
    output = output,
    gof_map = c("nobs", "r.squared", "adj.r.squared",
                "f_statistic", "f_p_value"),
    ...
  )
}
