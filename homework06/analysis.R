# written and runs on R 4.2.1
#
# required packages for script:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
# - alr4
#
# The assumption is that this file was run inside a
# folder called "homework6" within an Rproj directory.
# Link to GitHub: <https://github.com/psanker/a3sr-iqm>
#
# If you wish to render the associated Rmd, run `renv::restore()`
# to get the dependencies for creating the PDF.

# ---- preamble ----
se_mean <- function(x) {
  sd(x) / sqrt(length(x))
}

report_uncertainty <- function(x, digits = 2, use_se_mean = TRUE) {
  uncertainty_func <- if (isTRUE(use_se_mean)) se_mean else sd

  paste0(
    "$", formatC(mean(x), digits = digits, format = "f"),
    "\\pm ", formatC(uncertainty_func(x), digits = digits, format = "f"), "$"
  )
}

report_interval <- function(ci, digits = 2, use_sci_notation = FALSE) {
  stopifnot(length(ci) == 2)

  format <- if (isTRUE(use_sci_notation)) "E" else "f"

  paste0(
    "$[",
    formatC(ci[[1]], digits = digits, format = format),
    ",~",
    formatC(ci[[2]], digits = digits, format = format),
    "]$"
  )
}

report_model_elt <- function(mod, elt, digits = 2, use_sci_notation = FALSE, escape = TRUE) {
  format <- if (isTRUE(use_sci_notation)) "E" else "f"
  wrap <- if (isTRUE(escape)) "$" else ""

  paste0(
    wrap,
    formatC(coef(mod)[[elt]], digits = digits, format = format),
    "\\pm",
    formatC(std_errs(mod)[[elt]], digits = digits, format = format),
    wrap
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
map_lgl <- make_map(logical(1L))

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
sims <- map_lgl(seq_len(1000), \(i_sim) {
  n <- 278
  a <- 40
  b <- 0.8
  sig2 <- 5

  x <- runif(n, min = 20, max = 180)
  y <- a + (b * x) + rnorm(n, sd = sqrt(5))
  mod <- lm(y ~ x)

  # Generating a new plausible data point
  x_new <- runif(1, min = 20, max = 180)
  y_new <- a + (b * x_new) + rnorm(1, sd = sqrt(5))

  # Using predict() by default sets level = 0.95
  pred_int <- predict(mod, list(x = x_new), interval = "prediction")

  # [[2]] corresponds to 'lwr' and [[3]] corresponds to 'upr'
  pred_int[[2]] < y_new && pred_int[[3]] >= y_new
})

## Part 3: proportion of y_new in the intervals
q1_prop_in <- mean(sims)

# ---- question2 ----
dat_q2 <- alr4::UN11 |>
  tidytable::as_tidytable(.keep_rownames = TRUE) |>
  tidytable::transmute(
    country = rn,
    l_ppgdp = log(ppgdp),
    l_fertility = log(fertility),
  )

mod_q2 <- lm(l_fertility ~ l_ppgdp, data = dat_q2)

plt_q2f1 <- ggplot2::ggplot(
  dat_q2,
  ggplot2::aes(
    x = l_ppgdp,
    y = l_fertility,
  )
) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(
    formula = y ~ x,
    method = "lm",
    color = line_color
  ) +
  ggplot2::labs(
    x = "log(GDP per capita)",
    y = "log(Fertility)",
  ) +
  ggplot2::theme_bw()

pi_q2 <- predict(mod_q2, list(l_ppgdp = log(1000)), interval = "prediction")
exp_pi_q2 <- exp(c(pi_q2[[2]], pi_q2[[3]]))

dat_q2_raw <- alr4::UN11 |>
  tidytable::as_tidytable(.keep_rownames = TRUE) |>
  tidytable::rename(country = rn)

# min/max fertility
q2_t1 <- dat_q2_raw |>
  tidytable::filter(fertility %in% c(min(fertility), max(fertility))) |>
  tidytable::select(Country = country, Fertility = fertility)

# min/max residuals
# Note that the name for each entry in the residual vector is the row number
# of the input data. Also note that the vector name values increase
# monotonically, so order of the rows did not change when fitting
# the lm model. We can join this data back to the residuals.
resid_q2 <- tidytable::tidytable(
  row = seq_len(nrow(dat_q2_raw)),
  resid = residuals(mod_q2),
)

dat_q2_joined <- dat_q2_raw |>
  tidytable::mutate(row = .I) |> # .I is data.table/tidytable shorthand for the row numbers
  tidytable::inner_join(resid_q2, by = "row")

q2_t2a <- dat_q2_joined |>
  tidytable::slice_min(resid, n = 2) |>
  tidytable::select(
    Country = country,
    Residual = resid,
  )

q2_t2b <- dat_q2_joined |>
  tidytable::slice_max(resid, n = 2) |>
  tidytable::select(
    Country = country,
    Residual = resid,
  )

q2_t2 <- tidytable::bind_rows(q2_t2a, q2_t2b) |>
  tidytable::arrange(Residual)

# ---- question3 ----
# Duration is in seconds, Interval is in minutes
# However, will rescale for plotting clarity. Plots
# that compare the same dimension generally are more
# intuitive with the same units.
dat_q3 <- tidytable::as_tidytable(alr4::oldfaith) |>
  tidytable::mutate(Duration = Duration / 60)

mod_q3 <- lm(Interval ~ Duration, data = dat_q3)

plt_q3f1 <- ggplot2::ggplot(
  dat_q3,
  ggplot2::aes(x = Duration, y = Interval)
) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::labs(
    x = "Eruption duration (minutes)",
    y = "Time to next eruption (minutes)",
  ) +
  ggplot2::theme_bw()

pi_q3 <- predict(
  mod_q3,
  list(Duration = 250 / 60),
  interval = "prediction"
)

# ---- question4 ----
dat_q4 <- alr4::salary |>
  tidytable::mutate(
    female = as.integer(as.character(sex) == "Female"),
  )

plt_q4f1 <- ggplot2::ggplot(
  alr4::salary,
  ggplot2::aes(
    x = year,
    y = salary,
    color = sex
  )
) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Years in current rank",
    y = "Salary (USD, 1980s)",
    color = "Sex",
  ) +
  ggplot2::theme_bw()

mod_q4_1 <- lm(salary ~ female, data = dat_q4)
mod_q4_2 <- lm(salary ~ female + year, data = dat_q4)
mod_q4_3a <- lm(salary ~ year, data = dat_q4)
mod_q4_3b <- lm(female ~ year, data = dat_q4)
