# didhelpers 📊

<!-- badges: start -->
[![R-CMD-check](https://github.com/stfgrz/didhelpers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stfgrz/didhelpers/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**`didhelpers`** is a lightweight R toolkit providing diagnostic and visualization utilities for staggered Difference-in-Differences (DiD) designs.

Rather than introducing a new estimator, `didhelpers` acts as the ultimate **companion toolkit** for your analysis workflow — helping you validate core DiD assumptions, visualize treatment rollouts, generate comprehensive diagnostics, and export publication-quality tables and figures, all in just a few lines of code.

<hr/>

## 🚀 Core Diagnostics

| Function | Description |
|---|---|
| 🗺️ **`plot_treatment_timing()`** | Heatmap of treatment adoption across units and time periods. Perfect for understanding staggered cohort rollouts before estimation. |
| 📈 **`test_pretrends()`** | Event-study regression with unit and time fixed effects. Computes a joint pre-trend F-test and returns a customizable `ggplot2` coefficient plot. |
| ⚖️ **`bacon_weight_summary()`** | Goodman-Bacon (2021) decomposition. Tidy summary of all 2×2 DiD comparison weights driving the TWFE estimate, plus a diagnostic scatter plot. |
| 📑 **`did_diagnostics_report()`** | All-in-one wrapper that runs all three diagnostics and returns a consolidated report. |

## 🖨️ Output & Export

All diagnostic objects are first-class R citizens with a full suite of output methods:

| Method / Function | What it does |
|---|---|
| `print()`, `summary()` | Compact, fixest-style console output with significance stars and fit statistics |
| `plot()`, `autoplot()` | ggplot2 plots with confidence bands, reference lines, and clear labelling |
| `tidy()`, `glance()`, `augment()` | broom-convention output for seamless tidyverse and **modelsummary** integration |
| `as_table()` | Publication-ready **gt** or **kableExtra** tables for RMarkdown / Quarto |
| `export_pretrends_table()` | One-call LaTeX, Word, HTML, or Markdown export via **modelsummary** |

## 📦 Installation

```r
# install.packages("devtools")
devtools::install_github("stefanograziosi/didhelpers")
```

Optional dependencies unlock additional output formats:

```r
# gt / kableExtra tables
install.packages(c("gt", "kableExtra"))

# LaTeX / Word export
install.packages("modelsummary")

# Composite diagnostic plot (plot.did_report)
install.packages("patchwork")
```

## 💡 Quick Start

```r
library(didhelpers)
library(ggplot2)

data("staggered_data")

# ── 1. Treatment timing heatmap ─────────────────────────────────────────────
plot_treatment_timing(staggered_data, unit, time, treat)

# ── 2. Pre-trends test ───────────────────────────────────────────────────────
pt <- test_pretrends(staggered_data, unit, time, outcome, treat)
pt                        # Clean console summary
summary(pt)               # R-squared, adj. R-squared
autoplot(pt)              # Event-study coefficient plot
plot(pt)                  # Same, via plot()

# ── 3. Bacon decomposition ───────────────────────────────────────────────────
bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
bs                        # TWFE estimate, per-type breakdown
autoplot(bs)              # Scatter plot of 2×2 estimates vs. weights

# ── 4. All-in-one report ─────────────────────────────────────────────────────
report <- did_diagnostics_report(staggered_data, unit, time, outcome, treat)
report
plot(report)              # Composite figure (requires patchwork)
```

## 📊 Broom & modelsummary Integration

```r
# Tidy coefficient table (broom-convention column names)
tidy(pt)
tidy(pt, conf.int = TRUE, conf.level = 0.90)

# One-row model statistics
glance(pt)
glance(bs)

# Fitted values and residuals
augment(pt)

# Regression table — works automatically once tidy/glance are defined
library(modelsummary)
modelsummary(list("Event-Study" = pt))

# Convenience wrapper with format shortcuts
export_pretrends_table(pt, output = "latex")
export_pretrends_table(pt, output = "my_table.docx")
```

## 📋 Publication Tables

```r
# gt table (default in Quarto)
as_table(pt, format = "gt")
as_table(bs, format = "gt")

# kableExtra table (PDF / classic RMarkdown)
as_table(pt, format = "kable")
as_table(bs, format = "kable")
```

## 🔗 Relationship to Other Packages

`didhelpers` is a *diagnostic companion*, not a replacement for any estimator package.

| Package | Relationship |
|:--------|:-------------|
| **bacondecomp** | Hard dependency — `bacon_weight_summary()` wraps it, adding S3 class, broom methods, and table export. |
| **fixest** | Hard dependency — `test_pretrends()` uses `feols()` internally. fixest's `iplot()` is more flexible for fixest-native users; didhelpers bundles event-study + F-test + broom in one call. |
| **HonestDiD** | Complementary — sensitivity analysis for parallel trends violations, used *after* `test_pretrends()`. |
| **did2s** | No overlap — Gardner (2022) imputation estimator; didhelpers diagnoses TWFE, did2s replaces it. |
| **DIDmultiplegt** | No overlap — robust DiD estimators; use didhelpers to diagnose the data first. |
| **staggered** | Minimal overlap — plots its own robust estimates; didhelpers plots TWFE diagnostics. |

## ⚙️ Under the Hood

* **[`fixest`](https://lrberge.github.io/fixest/)** — lightning-fast fixed effects regression
* **[`ggplot2`](https://ggplot2.tidyverse.org/)** — all visual output, fully composable with `+ theme_*()`
* **[`bacondecomp`](https://github.com/evanflack/bacondecomp)** — exact Bacon decomposition statistics
* **[`generics`](https://generics.r-lib.org/)** — lightweight re-export of `tidy()`, `glance()`, `augment()`

## 📄 License

This package is licensed under the [MIT License](LICENSE).
