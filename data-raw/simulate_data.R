## Script to generate the staggered_data dataset for didhelpers
## 30 units, 15 time periods, 4 treatment cohorts, constant ATT = 2.0

set.seed(42)

n_units <- 30
n_periods <- 15

# Define cohorts: treatment onset times
# Cohort 1: units 1-8, treated at t = 6
# Cohort 2: units 9-16, treated at t = 9
# Cohort 3: units 17-22, treated at t = 12
# Cohort 4: units 23-30, never treated
cohort_map <- data.frame(
  unit = 1:n_units,
  cohort = c(
    rep("Cohort 1 (t=6)", 8),
    rep("Cohort 2 (t=9)", 8),
    rep("Cohort 3 (t=12)", 6),
    rep("Never treated", 8)
  ),
  treat_start = c(
    rep(6, 8),
    rep(9, 8),
    rep(12, 6),
    rep(Inf, 8)
  ),
  stringsAsFactors = FALSE
)

# Build balanced panel
staggered_data <- expand.grid(
  unit = 1:n_units,
  time = 1:n_periods
)
staggered_data <- merge(staggered_data, cohort_map, by = "unit")

# Treatment indicator
staggered_data$treat <- as.integer(staggered_data$time >= staggered_data$treat_start)

# Unit and time fixed effects
alpha <- rnorm(n_units, mean = 0, sd = 1)
lambda <- seq(0, 2, length.out = n_periods)

staggered_data$outcome <- alpha[staggered_data$unit] +
  lambda[staggered_data$time] +
  2.0 * staggered_data$treat +
  rnorm(nrow(staggered_data), mean = 0, sd = 0.5)

# Clean up: drop helper column, order nicely
staggered_data$treat_start <- NULL
staggered_data <- staggered_data[order(staggered_data$unit, staggered_data$time), ]
rownames(staggered_data) <- NULL

# Ensure correct types
staggered_data$unit <- as.integer(staggered_data$unit)
staggered_data$time <- as.integer(staggered_data$time)

save(staggered_data, file = "data/staggered_data.rda", compress = "xz")
