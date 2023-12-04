# written and runs on R 4.2.1
#
# required packages for script:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
# - janitor
# - santoku
#
# The assumption is that this file was run inside a
# folder called "homework11" within an Rproj directory.
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

report_model_elt <- function(mod, elt, digits = 2, use_sci_notation = FALSE, escape = TRUE, abs_est = FALSE) {
  format <- if (isTRUE(use_sci_notation)) "E" else "f"
  wrap <- if (isTRUE(escape)) "$" else ""

  estimate <- coef(mod)[[elt]]

  if (isTRUE(abs_est)) {
    estimate <- abs(estimate)
  }

  paste0(
    wrap,
    formatC(estimate, digits = digits, format = format),
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
map_chr <- make_map(character(1L))

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

has_attr <- function(x, key) {
  !is.null(attr(x, key, exact = TRUE))
}

library(ggplot2)
set.seed(0xABBA)
line_color <- "#d12e66"
line_color2 <- "#027ea5"

hist_fill <- "#dadada"
hist_color <- "grey"

# ---- question1 ----

mat <- matrix(c(
  1, 50, 0, qlogis(.6),
  1, 100, 0, qlogis(.8),
  1, 50, 1, qlogis(.7)
), byrow = TRUE, ncol = 4)

mat_rref <- pracma::rref(mat)
n_check <- 1e4

make_q1_dat <- function(n) {
  tidytable::tidytable(
    x = runif(n, min = 0, max = 100),
    z = sample(0:1, n, replace = TRUE),
  ) |>
    tidytable::mutate(
      logit_p = mat_rref[1, 4] + mat_rref[2, 4] * x + mat_rref[3, 4] * z,
      pass = map_int(plogis(logit_p), \(p) sample(0:1, 1, prob = c(1 - p, p))),
    )
}

dat_q1_check <- make_q1_dat(200)

plt_q1f1 <- ggplot(
  dat_q1_check,
  aes(x = x, y = pass, color = factor(z))
) +
  geom_jitter(
    height = 0.025,
    alpha = 0.4,
  ) +
  theme_bw()

# ---- question1-sim ----

n_sim <- 50
sims <- lapply(seq_len(1e4), \(i_sim) {
  dat <- make_q1_dat(n_sim)

  mod <- glm(pass ~ x + z, family = binomial, data = dat)

  tidytable::tidytable(
    est_theta = coef(mod)[[3]],
    se_theta = std_errs(mod)[[3]],
  ) |>
    tidytable::mutate(
      in_68 = abs(mat_rref[3, 4] - est_theta) < se_theta,
      in_50 = abs(mat_rref[3, 4] - est_theta) < 0.67 * se_theta,
      in_95 = abs(mat_rref[3, 4] - est_theta) < 1.96 * se_theta,
    )
}) |>
  data.table::rbindlist()

# ---- question1-bc ----
plt_q1f1 <- ggplot(
  sims |>
    tidytable::mutate(i = .I) |>
    tidytable::slice_head(n = 100),
  aes(x = i, y = est_theta),
) +
  geom_point(aes(color = in_68), alpha = 0.5) +
  geom_errorbar(
    aes(ymin = est_theta - se_theta, ymax = est_theta + se_theta),
    alpha = 0.3,
  ) +
  geom_hline(aes(yintercept = mat_rref[3, 4]), lty = 5) +
  labs(x = "Simulation number", y = "Estimate of theta", color = "True within 1 SE?") +
  theme_bw()

# ---- question2 ----
line_kinds <- c(
  Fitted = line_color2,
  True = line_color
)

n_q2 <- 100
dat_q2 <- tidytable::tidytable(
  x = runif(n_q2, min = -8, max = 8),
) |>
  tidytable::mutate(
    # Using latent variable formulation
    x_bin = x |>
      santoku::chop_evenly(
        10,
        labels = santoku::lbl_midpoints()
      ) |>
      as.character() |>
      as.numeric(),
    logit_p = 0.4 - 0.3 * x,
    logit_p_bin = 0.4 - 0.3 * x_bin,
    y = plogis(logit_p),
    y_bin = plogis(logit_p_bin),
    z = as.integer(logit_p + rlogis(n_q2, scale = 0.5) >= 0),
    true = "True",
    fitted = "Fitted",
  )

plt_q2f1a <- ggplot(
  dat_q2,
  aes(x = x, y = z)
) +
  geom_jitter(
    height = 0.025,
    alpha = 0.4,
  ) +
  geom_line(aes(y = y, color = true)) +
  geom_smooth(
    aes(color = fitted),
    formula = y ~ x,
    method = glm,
    se = FALSE,
    method.args = list(family = binomial),
    lty = 3,
  ) +
  scale_colour_manual(values = line_kinds) +
  labs(y = "y", color = "") +
  theme_bw()

plt_q2f1b <- ggplot(
  dat_q2,
  aes(x = x, y = z)
) +
  geom_jitter(
    height = 0.025,
    alpha = 0.4,
  ) +
  geom_point(
    aes(x = x_bin, y = y_bin),
    color = line_color
  ) +
  labs(y = "y") +
  theme_bw()

# ---- question3 ----
n_q3 <- 100
dat_q3 <- tidytable::tidytable(
  x1 = runif(n_q3, min = -8, max = 8),
  x2 = runif(n_q3, min = -13, max = 13),
) |>
  tidytable::mutate(
    logit_p = 0.4 - 0.3 * x1 + 0.2 * x2,
    z = as.integer(logit_p + rlogis(n_q3, scale = 0.5) >= 0),
  )

mod_q3 <- glm(z ~ x1 + x2, family = binomial, data = dat_q3)

band <- function(mod, x, p = 0.1) {
  inv_b2 <- 1 / coef(mod)[[3]]
  alp <- coef(mod)[[1]]
  b1 <- coef(mod)[[2]]
  logit_p <- qlogis(p)

  inv_b2 * (logit_p - alp - (b1 * x))
}

dat_q3_bands <- dat_q3 |>
  tidytable::mutate(
    p10 = band(mod_q3, x1, p = 0.1),
    p50 = band(mod_q3, x1, p = 0.5),
    p90 = band(mod_q3, x1, p = 0.9),
    # This silliness brought to you by ggplot2 being annoying
    col10 = "10%",
    col50 = "50%",
    col90 = "90%",
  )

line_colors <- c(
  "10%" = line_color2,
  "50%" = "#1a1a1a",
  "90%" = line_color
)

shapes <- c(
  "0" = 1,
  "1" = 16
)

plt_q3f1 <- ggplot(
  dat_q3_bands,
  aes(x = x1, y = x2, shape = factor(z)),
) +
  geom_point(alpha = 0.7) +
  scale_shape_manual(values = shapes) +
  geom_line(aes(y = p10, color = col10), lty = 3) +
  geom_line(aes(y = p50, color = col50)) +
  geom_line(aes(y = p90, color = col90), lty = 3) +
  scale_colour_manual(values = line_colors) +
  labs(color = "Discrimination", shape = "y") +
  theme_bw()
