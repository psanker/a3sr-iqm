# written and runs on R 4.2.1
#
# required packages for script:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
# - alr4
#
# The assumption is that this file was run inside a
# folder called "homework4" within an Rproj directory.
# Link to GitHub: <https://github.com/psanker/a3sr-iqm>
#
# If you wish to render the associated Rmd, run `renv::restore()`
# to get the dependencies for creating the PDF.

# ---- preamble ----
se_mean <- function(x) {
  sd(x) / sqrt(length(x))
}

report_uncertainty <- function(x, digits = 2) {
  paste0(
    "$", round(mean(x), digits = digits),
    "\\pm ", round(se_mean(x)), "$"
  )
}

std_errs <- function(mod) {
  summary(mod)$coefficients[, 2]
}


make_map <- function(type) {
  function(.x, .f, ...) vapply(.x, .f, type, ...)
}

map_dbl <- make_map(double(1L))
map_int <- make_map(integer(1L))

clamp <- function(x, x_min = NULL, x_max = NULL) {
  if (!is.null(x_min)) {
    x[!is.na(x) & x < x_min] <- x_min
  }

  if (!is.null(x_max)) {
    x[!is.na(x) & x > x_max] <- x_max
  }

  x
}

set.seed(0xABBA)
line_color <- "#d12e66"
line_color2 <- "#027ea5"

hist_fill <- "#dadada"
hist_color <- "grey"

# ---- question1 ----
n_pilots <- 500L

maneuver_score <- function(ability) {
  clamp(ability + rnorm(length(ability), sd = 1.5), x_min = 0, x_max = 10)
}

# I am using the {red, black, blue} color scheme because it is a nice
# color scale for comparison (as well as being accessible). Moreover,
# ggplot2's default discrete colors are ugly.
feedback_cols <- c(
  Positive = line_color2,
  Negative = line_color,
  None = "#3a3a3a"
)

dat_q1 <- tidytable::tidytable(
  ability = runif(n_pilots, min = 0, max = 10)
) |>
  tidytable::mutate(
    score_1 = maneuver_score(ability),
    score_2 = maneuver_score(ability),
    feedback = tidytable::case_when(
      score_1 > 7 ~ "Positive",
      score_1 < 3 ~ "Negative",
      TRUE ~ "None"
    )
  ) |>
  tidytable::mutate(
    mean_score_diff_by_feedback = mean(score_2 - score_1),
    # .N is the group size, if you're not familiar with data.table nomenclature
    se_score_diff_by_feedback = se_mean(score_2 - score_1),
    .by = feedback
  )

# ---- q1f1 ----
plt_q1f1 <- ggplot2::ggplot(
  dat_q1,
  ggplot2::aes(
    x = score_1,
    y = score_2,
    color = feedback
  )
) +
  # Slight transparency for overplotting
  ggplot2::geom_point(alpha = 0.9) +
  ggplot2::scale_colour_manual(values = feedback_cols) +
  ggplot2::theme_bw() +
  ggplot2::labs(
    x = "First maneuver score",
    y = "Second maneuver score",
    color = "Feedback between maneuvers"
  )

plt_q1f1

# ---- q1t1 ----
tab_q1t1 <- dat_q1 |>
  tidytable::select(
    feedback,
    mean_score_diff_by_feedback,
    se_score_diff_by_feedback
  ) |>
  unique() |>
  tidytable::mutate(
    score_diff = paste0(
      "$",
      round(mean_score_diff_by_feedback, 3),
      "\\pm ",
      round(se_score_diff_by_feedback, 3),
      "$"
    )
  ) |>
  tidytable::select(
    "Feedback between maneuvers" = feedback,
    "Mean score difference" = score_diff
  )

# ---- question2-a ----
# Put the fish data into a tidytable format to take advantage
# of data.table syntax in the R prompt (for personal inspection)
dat_bass <- tidytable::as_tidytable(alr4::wblake)

est_mean_length <- mean(dat_bass$Length)
se_mean_length <- sd(dat_bass$Length) / sqrt(nrow(dat_bass))

mod_bass <- lm(Length ~ 1, data = dat_bass)

tab_q2t1 <- tidytable::tidytable(
  "Calculation method" = c("Analytical", "lm model"),
  "Est. of mean length (mm)" = round(c(est_mean_length, coef(mod_bass)[[1]]), 0),
  "Std. err. of mean length (mm)" = round(c(se_mean_length, std_errs(mod_bass)[[1]]), 0)
)

# ---- question2-b ----
dat_earnings <- data.table::fread(here::here("homework4/earnings.csv"))

# Keep only relevant columns
dat_earnings_sub <- dat_earnings |>
  tidytable::select(age, earn) |>
  tidytable::mutate(
    age_bracket = tidytable::case_when(
      age >= 18 & age < 25 ~ "Young",
      age >= 25 & age < 31 ~ "Old", # I take offense to this
      TRUE ~ NA_character_
    ),
    age = NULL
  ) |>
  tidytable::mutate(
    est_mean_income = mean(earn),
    se_mean_income = se_mean(earn),
    .by = age_bracket
  ) |>
  tidytable::filter(!is.na(age_bracket)) |>
  tidytable::mutate(is_old = as.integer(age_bracket == "Old"))

# ea: earnings, calculated analytically
dat_ea <- dat_earnings_sub |>
  tidytable::select(age_bracket, est_mean_income, se_mean_income) |>
  unique()

# data.table syntax to take advantage of key lookup
data.table::setkey(dat_ea, age_bracket)
est_income_diff <- dat_ea["Old", est_mean_income] - dat_ea["Young", est_mean_income]
se_income_diff <- sqrt(dat_ea["Old", se_mean_income^2] + dat_ea["Young", se_mean_income^2])

mod_earnings <- lm(earn ~ is_old, data = dat_earnings_sub)

tab_q2t2 <- tidytable::tidytable(
  "Calculation method" = c("Analytical", "lm model"),
  "Est. (USD)" = round(c(est_income_diff, coef(mod_earnings)[[2]]), 2),
  "Std. err. (USD)" = round(c(se_income_diff, std_errs(mod_earnings)[[2]]), 2)
)

# ---- question4 ----
dat_ubs <- tidytable::as_tidytable(alr4::UBSprices)

plt_q4f1 <- ggplot2::ggplot(
  dat_ubs,
  ggplot2::aes(x = bigmac2003, y = bigmac2009)
) +
  ggplot2::geom_point() +
  ggplot2::geom_abline(
    ggplot2::aes(slope = 1, intercept = 0, color = "#000000"),
    alpha = 0.5,
    show.legend = FALSE,
  ) +
  ggplot2::geom_smooth(
    formula = y ~ x,
    method = "lm",
    ggplot2::aes(color = line_color),
  ) +
  ggplot2::scale_colour_manual(
    name = "Lines",
    labels = c("y = x", "ols"),
    values = c("#000000", line_color)
  ) +
  ggplot2::labs(
    x = "Big Mac cost in 2003 (minutes of labor)",
    y = "Big Mac cost in 2009 (minutes of labor)",
  ) +
  ggplot2::theme_bw()

# Part 3
log_dat_ubs <- dat_ubs |>
  tidytable::mutate(
    bigmac2003 = log(bigmac2003),
    bigmac2009 = log(bigmac2009),
  )

plt_q4f2 <- ggplot2::ggplot(
  log_dat_ubs,
  ggplot2::aes(x = bigmac2003, y = bigmac2009)
) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(
    formula = y ~ x,
    method = "lm",
    color = line_color,
  ) +
  ggplot2::labs(
    x = "Log of Big Mac cost in 2003",
    y = "Log of Big Mac cost in 2009",
  ) +
  ggplot2::theme_bw()

# Residual comparison
mod_nolog <- lm(bigmac2009 ~ bigmac2003, data = dat_ubs)
mod_log <- lm(bigmac2009 ~ bigmac2003, data = log_dat_ubs)

res_nolog <- residuals(mod_nolog)
res_log <- residuals(mod_log)

dat_residuals <- tidytable::tidytable(
  res = c(res_nolog, res_log),
  model = as.factor(c(
    rep("No transform", length(res_nolog)),
    rep("Log transform", length(res_log))
  )),
) |>
  tidytable::mutate(res = scale(res)[, 1], .by = model)

plt_q4f3 <- ggplot2::ggplot(
  dat_residuals,
  ggplot2::aes(
    x = model,
    y = res,
  )
) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    x = "Model transform",
    y = "Residual (z-score)",
  ) +
  ggplot2::theme_bw()

# ---- q4f5 ----
plt_q4f5_1 <- ggplot2::ggplot(
  dat_ubs,
  ggplot2::aes(x = bigmac2003)
) +
  ggplot2::geom_histogram(
    bins = 15,
    fill = hist_fill,
    color = hist_color,
  ) +
  ggplot2::labs(x = "Labor cost in minutes") +
  ggplot2::theme_bw()

plt_q4f5_2 <- ggplot2::ggplot(
  dat_ubs,
  ggplot2::aes(x = bigmac2009)
) +
  ggplot2::geom_histogram(
    bins = 15,
    fill = hist_fill,
    color = hist_color,
  ) +
  ggplot2::labs(x = "Labor cost in minutes") +
  ggplot2::theme_bw()

plt_q4f5_1
plt_q4f5_2
