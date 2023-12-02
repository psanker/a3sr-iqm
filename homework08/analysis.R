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
#' @param dat data.frame - A data.frame used, perhaps used to train a model
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

set.seed(0xABBA)
line_color <- "#d12e66"
line_color2 <- "#027ea5"

hist_fill <- "#dadada"
hist_color <- "grey"

# Actually attaching ggplot2 this time
library(ggplot2)

# ---- question1 ----
dat <- tidytable::as_tidytable(alr4::Heights)
n_obs <- nrow(dat)

train_rows <- sample(seq_len(n_obs), 2 * (n_obs %/% 3))
test_rows <- setdiff(seq_len(n_obs), train_rows)

dat_train <- dat[train_rows]
dat_test <- dat[test_rows]

mod_q1 <- lm(dheight ~ mheight, data = dat_train)

dat_test_fit <- dat_test |>
  augment(mod_q1)

mse_fit <- mean(dat_test_fit$.res^2)
rmse_fit <- sqrt(mse_fit)

tab_q1t1 <- tidytable::tidytable(
  Model = "$\\text{dheight}\\sim\\text{mheight}$",
  MSE = mse_fit,
  RMSE = rmse_fit,
)

# ---- question2 ----
n_sims <- 1e4

sim_q2_1 <- tidytable::tidytable(
  stu_a1 = 70 + rnorm(n_sims, sd = 10),
  stu_a2 = 70 + rnorm(n_sims, sd = 10),
  stu_b1 = 40 + rnorm(n_sims, sd = 10),
  stu_b2 = 40 + rnorm(n_sims, sd = 10),
)

cor_q2_a1 <- cor(sim_q2_1$stu_a1, sim_q2_1$stu_b1)
cor_q2_a2 <- cor(sim_q2_1$stu_a1, sim_q2_1$stu_a2)

school_a <- rnorm(n_sims, mean = 70, sd = 5)
school_b <- rnorm(n_sims, mean = 40, sd = 5)

sim_q2_2 <- tidytable::tidytable(
  stu_a1 = school_a + rnorm(n_sims, sd = 10),
  stu_a2 = school_a + rnorm(n_sims, sd = 10),
  stu_b1 = school_b + rnorm(n_sims, sd = 10),
  stu_b2 = school_b + rnorm(n_sims, sd = 10),
)

cor_q2_b1 <- cor(sim_q2_2$stu_a1, sim_q2_2$stu_b1)
cor_q2_b2 <- cor(sim_q2_2$stu_a1, sim_q2_2$stu_a2)

# ---- question3 ----
dat_q3 <- data.table::fread(here::here("homework8/wages.csv"))

# Assuming wages is in (1985 USD)/hr
mod_q3_1 <- lm(wages ~ workexp, data = dat_q3)

big_r2 <- summary(mod_q3_1)$r.squared
pearson_rxy <- cor(dat_q3$wages, dat_q3$workexp)

tab_q3t1 <- tidytable::tidytable(
  Model = "$\\text{wages}\\sim\\text{workexp}$",
  "$R^2$" = big_r2,
  "$r_{xy}$" = pearson_rxy,
)

mod_q3_2 <- lm(wages ~ workexp * female, data = dat_q3)

tab_q3t2 <- tidytable::tidytable(
  Model = "$\\text{wages}\\sim\\text{workexp}*\\text{female}$",
  "$\\beta_w$" = report_model_elt(mod_q3_2, 2),
  "$\\beta_f$" = report_model_elt(mod_q3_2, 3),
  "$\\beta_{w:f}$" = report_model_elt(mod_q3_2, 4),
)

dat_q3_2_fit <- dat_q3 |>
  augment(mod_q3_2)

cor_wages_pred <- dat_q3_2_fit[, cor(wages, .pred)]

tab_q3t3 <- tidytable::tidytable(
  Metric = c("$R^2$", "$\\sqrt{R^2}$", "$r_{y\\hat{y}}$"),
  Value = c(
    summary(mod_q3_2)$r.squared,
    sqrt(summary(mod_q3_2)$r.squared),
    cor_wages_pred
  ),
)

new_data <- dat_q3 |>
  tidytable::select(wages, workexp) |>
  tidytable::mutate(
    x_1 = rnorm(.N),
    x_2 = rnorm(.N),
    x_3 = rnorm(.N),
    x_4 = rnorm(.N),
    x_5 = rnorm(.N),
    x_6 = rnorm(.N),
    x_7 = rnorm(.N),
    x_8 = rnorm(.N),
    x_9 = rnorm(.N),
  )

mod_q3_3 <- lm(
  wages ~ workexp + x_1 * x_2 * x_3 * x_4 * x_5 * x_6 * x_7 * x_8 * x_9,
  data = new_data
)

# ---- question5 ----
dat_q5 <- data.table::fread(here::here("homework8/pyth.csv"))

dat_q5_train <- dat_q5[1:40]
dat_q5_test <- dat_q5[41:nrow(dat_q5)]

mod_q5_1 <- lm(y ~ x1 + x2, data = dat_q5_train)

tab_q5t1 <- tidytable::tidytable(
  "$\\alpha$" = report_model_elt(mod_q5_1, 1),
  "$\\beta_1$" = report_model_elt(mod_q5_1, 2),
  "$\\beta_2$" = report_model_elt(mod_q5_1, 3),
  "$R^2$" = summary(mod_q5_1)$r.squared,
  "Resid. std. error" = summary(mod_q5_1)$sigma,
)

plt_q5f1a <- ggplot(
  dat_q5_train,
  aes(x = x1, y = y)
) +
  geom_point(alpha = 0.4) +
  geom_abline(
    aes(
      intercept = coef(mod_q5_1)[[1]],
      slope = coef(mod_q5_1)[[2]]
    ),
    color = line_color,
  ) +
  theme_bw()

plt_q5f1b <- ggplot(
  dat_q5_train,
  aes(x = x2, y = y)
) +
  geom_point(alpha = 0.4) +
  geom_abline(
    aes(
      intercept = coef(mod_q5_1)[[1]],
      slope = coef(mod_q5_1)[[3]]
    ),
    color = line_color,
  ) +
  theme_bw()

plt_q5f2 <- ggplot(
  dat_q5_train |>
    augment(mod_q5_1),
  aes(x = .pred, y = .res),
) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    formula = y ~ x,
    method = "lm",
    color = line_color
  ) +
  labs(
    x = "Predicted y",
    y = "Residual",
  ) +
  theme_bw()
