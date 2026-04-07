# didhelpers 0.1.0

## Initial release

### Core diagnostics

* `plot_treatment_timing()` — heatmap of treatment adoption across units and
  time periods.
* `test_pretrends()` — event-study regression with unit and time fixed effects,
  joint F-test for pre-treatment leads, and coefficient plot via `autoplot()`.
* `bacon_weight_summary()` — Goodman-Bacon (2021) decomposition summary with
  per-type weight breakdown and scatter plot.
* `did_diagnostics_report()` — all-in-one wrapper running all three diagnostics
  and returning a consolidated report.

### Output and export

* Full S3 method suite: `print()`, `summary()`, `plot()`, `autoplot()`.
* Broom integration: `tidy()`, `glance()`, `augment()` for seamless
  `modelsummary` support.
* `as_table()` — publication-quality tables via **gt** or **kableExtra**.
* `export_pretrends_table()` — one-call export to LaTeX, Word, HTML, or
  Markdown via **modelsummary**.
* Input validation on all core functions with informative error messages.
* Configurable confidence level in `autoplot()` via `conf.level` parameter.

### Data

* Bundled `staggered_data` dataset: 30 units × 15 periods, 4 cohorts,
  constant ATT = 2.0, no pre-trends.
