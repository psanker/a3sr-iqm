# written and runs on R 4.2.1
#
# required packages for script:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
# - ggthemes
# - alr4
#
# The assumption is that this file was run inside a
# folder called "homework9" within an Rproj directory.
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

map2 <- function(.x, .y, .f, ...) {
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    names(out) <- names(.x)
  } else {
    names(out) <- NULL
  }

  out
}

clamp <- function(x, x_min = NULL, x_max = NULL) {
  if (!is.null(x_min)) {
    x[!is.na(x) & x < x_min] <- x_min
  }

  if (!is.null(x_max)) {
    x[!is.na(x) & x > x_max] <- x_max
  }

  x
}

#' Basic form of broom::augment
#'
#' Essentially does what broom::augment() does
#' but without its dependency.
#'
#' @param dat data.frame - A data.frame used,
#'   perhaps used to train a model
#' @param mod model - A model object that responds to predict()
#' @param ... - Arguments passed to predict()
#' @return `dat` with two new columns:
#'   * .pred: the predicted value
#'   * .res: the residual
augment <- function(dat, mod, ...) {
  stopifnot("formula" %in% class(mod$terms))

  res_var <- as.character(mod$terms[[2]])

  dat |>
    tidytable::mutate(
      .pred = predict(mod, dat, ...),
      .res = dat[[res_var]] - .pred,
    )
}

library(ggplot2)
set.seed(0xABBA)
line_color <- "#d12e66"
line_color2 <- "#027ea5"

hist_fill <- "#dadada"
hist_color <- "grey"

# ---- question3 ----
dat_q1 <- data.table::fread(here::here("homework4/earnings.csv")) |>
  tidytable::mutate(
    age10 = age / 10,
    age10_sq = age10^2,
  )

# ---- question5 ----
dat_q5 <- data.table::fread(here::here("homework9/pollution.csv"))

# mort: Mortality rate
# hc: Hydrocarbon particulates (guessing this is ~PM2.5)
# nox: Nitric oxides
# so2: Sulfur dioxide

plt_q5f1a <- ggplot(
  dat_q5,
  aes(x = nox, y = mort),
) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Nitric oxide level",
    y = "Mortality rate"
  ) +
  theme_bw()

mod_q5_1 <- lm(mort ~ nox, data = dat_q5)

plt_q5f1b <- ggplot(
  dat_q5 |>
    augment(mod_q5_1),
  aes(x = .pred, y = .res),
) +
  geom_point(alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "lm", color = line_color) +
  labs(x = "Predicted", y = "Residual") +
  theme_bw()

mod_q5_2 <- lm(mort ~ log(nox), data = dat_q5)

plt_q5f2a <- ggplot(
  dat_q5,
  aes(x = log(nox), y = mort),
) +
  geom_point(alpha = 0.7) +
  labs(
    x = "log(Nitric oxide level)",
    y = "Mortality rate"
  ) +
  theme_bw()

dat_q5_b1 <- dat_q5 |>
  augment(mod_q5_2) |>
  tidytable::mutate(.scale_res = .res / sd(.res))

plt_q5f2b <- ggplot(
  dat_q5_b1,
  aes(x = .pred, y = .res),
) +
  geom_point(alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "lm", color = line_color) +
  labs(x = "Predicted", y = "Residual") +
  theme_bw()

mod_q5_3 <- lm(log(mort) ~ log(nox), data = dat_q5)

plt_q5f3a <- ggplot(
  dat_q5,
  aes(x = log(nox), y = log(mort)),
) +
  geom_point(alpha = 0.7) +
  labs(
    x = "log(Nitric oxide level)",
    y = "log(Mortality rate)"
  ) +
  theme_bw()

dat_q5_b2 <- dat_q5 |>
  tidytable::mutate(
    .pred = predict(mod_q5_3),
    .res = log(mort) - .pred,
    .scale_res = .res / sd(.res),
  )

plt_q5f3b <- ggplot(
  dat_q5_b2,
  aes(x = .pred, y = .res),
) +
  geom_point(alpha = 0.4) +
  geom_smooth(formula = y ~ x, method = "lm", color = line_color) +
  labs(x = "Predicted", y = "Residual") +
  theme_bw()

tab_q5t1 <- tidytable::tidytable(
  Model = "$\\mathrm{mort}\\sim\\log{(\\mathrm{nox})}$",
  "$\\alpha$" = report_model_elt(mod_q5_2, 1),
  "$\\beta_N$" = report_model_elt(mod_q5_2, 2),
)

plt_q5f4 <- ggplot(
  dat_q5 |>
    tidytable::summarise(
      x = c(hc, nox, so2),
      var = c(
        rep("hc", length(hc)),
        rep("nox", length(nox)),
        rep("so2", length(so2))
      ),
    ),
  aes(x = x),
) +
  geom_histogram(color = hist_color, fill = hist_fill) +
  labs(x = "Pollution rate", y = "Count") +
  facet_wrap(~var) +
  theme_bw()

mod_q5_4 <- lm(mort ~ log(hc) + log(nox) + log(so2), data = dat_q5)

tab_q5t2 <- tidytable::tidytable(
  "$\\alpha$" = report_model_elt(mod_q5_4, 1),
  "$\\beta_H$" = report_model_elt(mod_q5_4, 2),
  "$\\beta_N$" = report_model_elt(mod_q5_4, 3),
  "$\\beta_S$" = report_model_elt(mod_q5_4, 4),
)
