#' Simulated Staggered Difference-in-Differences Dataset
#'
#' A balanced panel dataset with 30 units observed over 15 time periods,
#' featuring staggered treatment adoption across four cohorts. The data
#' generating process uses a constant average treatment effect on the treated
#' (ATT) of 2.0 with no pre-trends, making it suitable for demonstrating
#' DiD diagnostic tools.
#'
#' @format A data frame with 450 rows and 5 variables:
#' \describe{
#'   \item{unit}{Integer. Unit identifier (1 to 30).}
#'   \item{time}{Integer. Time period (1 to 15).}
#'   \item{cohort}{Character. Treatment cohort label indicating when the unit
#'     begins treatment: \code{"Cohort 1 (t=6)"}, \code{"Cohort 2 (t=9)"},
#'     \code{"Cohort 3 (t=12)"}, or \code{"Never treated"}.}
#'   \item{treat}{Integer. Treatment indicator (1 if treated in this period,
#'     0 otherwise).}
#'   \item{outcome}{Numeric. Outcome variable generated as unit FE + time FE +
#'     2.0 * treat + N(0, 0.5).}
#' }
#'
#' @source Simulated via \code{data-raw/simulate_data.R}.
"staggered_data"
