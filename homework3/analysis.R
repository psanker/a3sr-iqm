# written and runs on R 4.2.1
#
# required packages for script:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
#
# The assumption is that this file was run inside a
# folder called "homework3" within an Rproj directory.
# Link to GitHub: <https://github.com/psanker/a3sr-iqm>
#
# If you wish to render the associated Rmd, run `renv::restore()`
# to get the dependencies for creating the PDF.

# ---- preamble ----
std_errs <- function(mod) {
  summary(mod)$coefficients[, 2]
}

make_std_err_table <- function(se_a, se_b) {
  rbind(se_a, se_b) |>
    tidytable::as_tidytable(.keep_rownames = TRUE) |>
    tidytable::mutate(
      rn = gsub(".*([ab])$", "(\\1)", rn)
    ) |>
    tidytable::rename(
      "Figure" = rn,
      "Std. Err. of Intercept" = `(Intercept)`,
      "Std. Err. of Slope" = x
    )
}

make_map <- function(type) {
  function(.x, .f, ...) vapply(.x, .f, type, ...)
}

map_dbl <- make_map(double(1L))
map_int <- make_map(integer(1L))

set.seed(0xABBA)
line_color <- "#d12e66"

# ---- question3 ----
y <- 1 + 3 * sample(1:3, 1e4, replace = TRUE) + rnorm(1e4)
dat <- tidytable::tidytable(y = y)

q3_plot <- ggplot2::ggplot(dat, ggplot2::aes(x = y)) +
  ggplot2::geom_histogram(
    bins = 40,
    fill = "#dadada",
    color = "grey"
  ) +
  ggplot2::theme_bw()

q3_plot

# ---- question5 ----
sim_and_plot <- function(a, b, n, sigma,
                         title = NULL,
                         include_plot = TRUE) {
  x <- runif(n, min = 0, max = 100)
  y <- a + b * x + rnorm(n, mean = 0, sd = sigma)

  dat <- tidytable::tidytable(
    x = x,
    y = y,
  )

  mod <- lm(y ~ x, data = dat)
  a_est <- coef(mod)[[1]]
  b_est <- coef(mod)[[2]]

  plt <- NULL

  if (isTRUE(include_plot)) {
    plt <- ggplot2::ggplot(dat, ggplot2::aes(x, y)) +
      ggplot2::geom_point(alpha = 0.3) +
      ggplot2::geom_abline(
        ggplot2::aes(intercept = a_est, slope = b_est),
        color = line_color,
        size = 1.5
      ) +
      ggplot2::theme_bw()

    if (!is.null(title)) {
      plt <- plt + ggplot2::ggtitle(title)
    }
  }

  list(
    data = dat,
    model = mod,
    plot = plt
  )
}

# ---- q5f1 ----
test1 <- sim_and_plot(
  0, 0, 1000, 1,
  title = "a = 0; b = 0; n = 1000; s = 1"
)

plot(test1$plot)

# ---- q5f2 ----
test2 <- sim_and_plot(
  0, 0.5, 1000, 5,
  title = "a = 0; b = 0.5; n = 1000; s = 5"
)

test3 <- sim_and_plot(
  0, 0.5, 1000, 15,
  title = "a = 0; b = 0.5; n = 1000; s = 15"
)

plot(test2$plot)
plot(test3$plot)

# ---- q5f2-compare ----
se_a <- std_errs(test2$model)
se_b <- std_errs(test3$model)

tab_1 <- make_std_err_table(se_a, se_b)

# ---- q5f3 ----
test4 <- sim_and_plot(
  0, 0.5, 10, 15,
  title = "a = 0; b = 0.5; n = 10; s = 15"
)

test5 <- sim_and_plot(
  0, 0.5, 100, 15,
  title = "a = 0; b = 0.5; n = 100; s = 15"
)

plot(test4$plot)
plot(test5$plot)

# ---- q5f3-compare ----
se_a <- std_errs(test4$model)
se_b <- std_errs(test5$model)

tab_2 <- make_std_err_table(se_a, se_b)

# ---- question6 ----
# Resetting seed state because of some crazy outlier
set.seed(0xABBA)

n_space <- seq.int(from = 5, to = 1000, by = 5)
sims <- lapply(n_space, \(n) {
  sim_and_plot(a = -85027.3, b = 1595, n = n, sigma = 21690, include_plot = FALSE)
})

# ---- q6f1 ----
est_intercept <- map_dbl(sims, \(sim) coef(sim$model)[1])
est_slope <- map_dbl(sims, \(sim) coef(sim$model)[2])

dat <- tidytable::tidytable(
  n = n_space,
  est_int = est_intercept,
  est_slope = est_slope,
)

plt1 <- ggplot2::ggplot(dat, ggplot2::aes(x = n, y = est_int)) +
  ggplot2::geom_quantile(
    quantiles = c(0.025, 0.975),
    formula = y ~ x,
    color = line_color,
    alpha = 0.5
  ) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Observation count (n)",
    y = "Estimate of the intercept"
  ) +
  ggplot2::theme_bw()

plt2 <- ggplot2::ggplot(dat, ggplot2::aes(x = n, y = est_slope)) +
  ggplot2::geom_quantile(
    quantiles = c(0.025, 0.975),
    formula = y ~ x,
    color = line_color,
    alpha = 0.5
  ) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Observation count (n)",
    y = "Estimate of the slope"
  ) +
  ggplot2::theme_bw()

plt1
plt2

# ---- q6f2 ----
se_intercept <- map_dbl(sims, \(sim) std_errs(sim$model)[1])
se_slope <- map_dbl(sims, \(sim) std_errs(sim$model)[2])

dat <- tidytable::tidytable(
  n = n_space,
  se_int = se_intercept,
  se_slope = se_slope,
)

plt3 <- ggplot2::ggplot(dat, ggplot2::aes(x = n, y = se_int)) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Observation count (n)",
    y = "Std. error of the intercept"
  ) +
  ggplot2::theme_bw()

plt4 <- ggplot2::ggplot(dat, ggplot2::aes(x = n, y = se_slope)) +
  ggplot2::geom_point() +
  ggplot2::labs(
    x = "Observation count (n)",
    y = "Std. error of the slope"
  ) +
  ggplot2::theme_bw()

plt3
plt4

# ---- extra-credit-a ----
# Part (a)
sim_hoops <- function(i_sim) {
  # Output buffer
  out <- rep(NA_integer_, 32)
  stop_game <- FALSE

  # Hang check
  iter <- 0
  max_iter <- 32000

  while (!isTRUE(stop_game) && iter < max_iter) {
    # Adjust for buffer overflow
    if (sum(is.na(out)) < 1) {
      out <- c(out, rep(NA_integer_, length(out)))
    }

    i <- which(is.na(out))[[1]]
    out[[i]] <- sample(0:1, 1, prob = c(0.4, 0.6))

    if (i >= 2) {
      if (all(out[c(i - 1, i)] == 0)) {
        stop_game <- TRUE
      }
    }

    iter <- iter + 1
  }

  if (iter >= max_iter) {
    stop("Maximum number of iterations hit", call. = FALSE)
  }

  out[!is.na(out)]
}

# ---- extra-credit-b ----
sims <- lapply(seq_len(1e3), sim_hoops)
n_shots <- map_int(sims, length)

n_made <- map_int(sims, sum)
p_made <- n_made / n_shots
