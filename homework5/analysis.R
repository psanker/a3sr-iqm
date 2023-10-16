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

report_uncertainty <- function(x, digits = 2, use_se_mean = TRUE) {
  uncertainty_func <- if (isTRUE(use_se_mean)) se_mean else sd

  paste0(
    "$", formatC(mean(x), digits = digits, format = "f"),
    "\\pm ", formatC(uncertainty_func(x), digits = digits, format = "f"), "$"
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

n_sims <- 1000L
n_obs <- 100L

sims <- lapply(seq_len(n_sims), function(i_sim,
                                         a = 2,
                                         b = 3,
                                         sigma = 5) {
  x <- runif(n_obs, min = 0, max = 20)
  y <- a + (b * x) + rnorm(n_obs, mean = 0, sd = sigma)

  fake <- tidytable::tidytable(x = x, y = y)
  mod <- lm(y ~ x, data = fake)

  est_sigma <- summary(mod)[["sigma"]]
  est_a <- coef(mod)[[1]]
  se_a <- std_errs(mod)[[1]]

  est_b <- coef(mod)[[2]]
  se_b <- std_errs(mod)[[2]]

  tidytable::tidytable(
    i = i_sim,
    est_a = est_a,
    se_a = se_a,
    a_in_68 = abs(a - est_a) < se_a,
    a_in_95 = abs(a - est_a) < 2 * se_a,
    est_b = est_b,
    se_b = se_b,
    b_in_68 = abs(b - est_b) < se_b,
    b_in_95 = abs(b - est_b) < 2 * se_b,
    est_sigma = est_sigma,
  )
}) |>
  data.table::rbindlist()

q1_summary <- sims |>
  tidytable::summarise(
    est_a = mean(est_a),
    se_a = mean(se_a),
    a_in_95 = mean(a_in_95),
    est_b = mean(est_b),
    se_b = mean(se_b),
    b_in_95 = mean(b_in_95),
    est_sigma = report_uncertainty(est_sigma, use_se_mean = FALSE)
  )

# ---- question3 ----
q3_dat <- tidytable::as_tidytable(alr4::Heights)

q3_mod <- lm(dheight ~ mheight, data = q3_dat)

q3_plt <- ggplot2::ggplot(
  q3_dat,
  ggplot2::aes(x = mheight, y = dheight)
) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(
    formula = y ~ x,
    method = "lm",
    color = line_color,
  ) +
  ggplot2::labs(
    x = "Mother's height (in)",
    y = "Daughter's height (in)",
  ) +
  ggplot2::theme_bw()
