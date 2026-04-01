# didhelpers

Lightweight diagnostic and visualization utilities for staggered
difference-in-differences (DiD) designs. **didhelpers** is not a new estimator
--- it is the companion toolkit you wish existed when running a staggered DiD
analysis.

## Features

| Function | What it does |
|---|---|
| `plot_treatment_timing()` | Heatmap of treatment adoption across units and time |
| `test_pretrends()` | Event-study regression with unit/time FEs, pre-trend F-test, and coefficient plot |
| `bacon_weight_summary()` | Goodman-Bacon (2021) decomposition with summary table and scatter plot |
| `did_diagnostics_report()` | Runs all three above and returns a combined report |

## Installation

```r
# install.packages("devtools")
devtools::install_github("stefanograziosi/didhelpers")
```

## Quick Example

```r
library(didhelpers)
data("staggered_data")

# 1. Visualize treatment timing
plot_treatment_timing(staggered_data, unit, time, treat)

# 2. Test pre-trends
pt <- test_pretrends(staggered_data, unit, time, outcome, treat)
pt$coefficients
ggplot2::autoplot(pt)

# 3. Bacon decomposition
bs <- bacon_weight_summary(staggered_data, unit, time, outcome, treat)
bs$summary
bs$plot

# 4. Run everything at once
report <- did_diagnostics_report(staggered_data, unit, time, outcome, treat)
report
```

## Dependencies

`ggplot2`, `fixest`, `bacondecomp`, `dplyr`, `tidyr`, `rlang`.

## License

MIT
