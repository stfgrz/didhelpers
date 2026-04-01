# didhelpers 📊

<!-- badges: start -->
[![R-CMD-check](https://github.com/stefanograziosi/didhelpers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stefanograziosi/didhelpers/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**`didhelpers`** is a lightweight, high-performance R toolkit designed to provide diagnostic and visualization utilities for staggered Difference-in-Differences (DiD) designs.

Rather than introducing a brand new estimator, `didhelpers` acts as the ultimate **companion toolkit** for your analysis workflow. It helps you quickly validate core DiD assumptions, visualize treatment rollouts across cohorts, and generate comprehensive diagnostics—all in just a few lines of code.

<hr/>

## 🚀 Features

The package revolves around four core functions designed to streamline your structural modeling workflows:

| Function | Description |
|---|---|
| 🗺️ **`plot_treatment_timing()`** | Generates a clean heatmap of treatment adoption across units and time periods. Perfect for understanding staggered cohort rollouts before estimation. |
| 📈 **`test_pretrends()`** | Efficiently runs an event-study regression applying unit and time fixed effects. Computes a pre-trend F-test and outputs an elegant, customizable `ggplot2` coefficient plot to visually assess the parallel trends assumption. |
| ⚖️ **`bacon_weight_summary()`** | Implements the Goodman-Bacon (2021) decomposition. Outputs a tidy summary table comparing the weights of all 2x2 DiD comparisons driving the overall TWFE estimate, along with a diagnostic scatter plot. |
| 📑 **`did_diagnostics_report()`** | An all-in-one wrapper that automatically executes all three of the diagnostics above, returning a consolidated report. |

## 📦 Installation

You can install the development version of `didhelpers` directly from GitHub via [`devtools`](https://devtools.r-lib.org/):

```r
# install.packages("devtools")
devtools::install_github("stefanograziosi/didhelpers")
```

## 💡 Quick Start

Here is a quick walkthrough using the package's built-in `staggered_data`. The functions are designed with non-standard evaluation, making them incredibly intuitive to use inside tidy pipelines.

```r
library(didhelpers)
library(ggplot2) # For customizing automatic plots

# Load the built-in sample data
data("staggered_data")

# 1. Visualize Treatment Timing Rollout
plot_treatment_timing(staggered_data, unit, time, treat)

# 2. Test Parallel Trends Assumption
pt <- test_pretrends(staggered_data, unit, time, outcome, treat)
pt$coefficients          # Inspect raw event-study coefficients and standard errors
autoplot(pt)             # Automatically renders the coefficient plot

# 3. Goodman-Bacon Decomposition
bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
bs$summary               # Tabular breakdown of 2x2 decomposition weights
bs$plot                  # Visual scatter plot of component weights

# 4. Generate an All-in-One Diagnostic Report
report <- did_diagnostics_report(staggered_data, unit, time, outcome, treat)
report
```

For a more detailed walkthrough and interpretation guidelines, check out the [Getting Started Vignette](vignettes/getting-started.Rmd).

## ⚙️ Under the Hood

To maintain scale and speed, `didhelpers` relies heavily on highly-optimized backend engines:
* **[`fixest`](https://lrberge.github.io/fixest/)**: Powers all regression modeling with lightning-fast fixed effects estimations.
* **[`ggplot2`](https://ggplot2.tidyverse.org/)**: Renders all visual outputs, allowing users full control to append themes and further customize plots.
* **[`bacondecomp`](https://github.com/evanflack/bacondecomp)**: Computes the exact Bacon decomposition statistics.

## 📄 License

This package is licensed under the [MIT License](LICENSE).
