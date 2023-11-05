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

set.seed(0xABBA)
line_color <- "#d12e66"
line_color2 <- "#027ea5"

hist_fill <- "#dadada"
hist_color <- "grey"

# Actually attaching ggplot2 this time
library(ggplot2)

# ---- question1 ----

sim_q1 <- function(n = 100) {
  z <- sample(0:1, n, replace = TRUE)
  x <- rnorm(n, mean = z, sd = 1)

  design <- cbind(rep(1, n), x, z, x * z)
  beta <- c(1, 2, -1, 2)

  y <- as.numeric(design %*% beta + rnorm(n, sd = 3))

  tidytable::tidytable(
    y = y,
    x = x,
    z = z,
  )
}

dat_q1 <- sim_q1()

mod_q1 <- list(
  lm(y ~ x + z, data = dat_q1),
  lm(y ~ x * z, data = dat_q1)
)

plt_q1f1 <- ggplot(
  dat_q1 |>
    tidytable::mutate(z = z + 20), # to get the correct shape mapping
  aes(x = x, y = y, shape = z),
) +
  geom_point() +
  scale_shape_identity() +
  theme_bw()

plt_q1f2 <- ggplot(
  dat_q1 |>
    tidytable::mutate(
      pred = predict(mod_q1[[1]]),
      z = factor(z),
    ),
  aes(x = x, y = y, color = z),
) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = pred)) +
  scale_colour_manual(values = c(
    "0" = line_color2,
    "1" = line_color
  )) +
  theme_bw()

plt_q1f3 <- ggplot(
  dat_q1 |>
    tidytable::mutate(
      pred = predict(mod_q1[[2]]),
      z = factor(z),
    ),
  aes(x = x, y = y, color = z),
) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = pred)) +
  scale_colour_manual(values = c(
    "0" = line_color2,
    "1" = line_color
  )) +
  theme_bw()

# ---- question3 ----
dat_q3_0 <- alr4::salary |>
  tidytable::mutate(
    sex = as.character(sex),
    sex = tidytable::case_when(
      sex == "Male" ~ 0L,
      sex == "Female" ~ 1L,
      TRUE ~ NA_integer_,
    ),
  )

dat_q3_1 <- alr4::salary |>
  tidytable::mutate(
    sex = as.character(sex),
    sex = tidytable::case_when(
      sex == "Male" ~ 2L,
      sex == "Female" ~ 1L,
      TRUE ~ NA_integer_,
    ),
  )

mod_q3_0 <- lm(salary ~ sex * year, data = dat_q3_0)
mod_q3_1 <- lm(salary ~ sex * year, data = dat_q3_1)

dat_q3_2 <- alr4::salary |>
  tidytable::mutate(
    sex = as.character(sex),
    sex = tidytable::case_when(
      sex == "Male" ~ -1L,
      sex == "Female" ~ 1L,
      TRUE ~ NA_integer_,
    ),
  )

mod_q3_2 <- lm(salary ~ sex * year, data = dat_q3_2)

tab_q1 <- tidytable::tidytable(
  `Model` = c("Base", "5.18.1", "5.18.2"),
  "$\\alpha$" = c(
    report_model_elt(mod_q3_0, 1, digits = 0),
    report_model_elt(mod_q3_1, 1, digits = 0),
    report_model_elt(mod_q3_2, 1, digits = 0)
  ),
  "$\\beta_{\\mathrm{s}}$" = c(
    report_model_elt(mod_q3_0, 2, digits = 0),
    report_model_elt(mod_q3_1, 2, digits = 0),
    report_model_elt(mod_q3_2, 2, digits = 0)
  ),
  "$\\beta_{\\mathrm{y}}$" = c(
    report_model_elt(mod_q3_0, 3, digits = 0),
    report_model_elt(mod_q3_1, 3, digits = 0),
    report_model_elt(mod_q3_2, 3, digits = 0)
  ),
  "$\\beta_{\\mathrm{s:y}}$" = c(
    report_model_elt(mod_q3_0, 4, digits = 0),
    report_model_elt(mod_q3_1, 4, digits = 0),
    report_model_elt(mod_q3_2, 4, digits = 0)
  )
)

# ---- question4 ----
predict_q4 <- function(gpa, iq, level = 1) {
  50 + 20 * gpa + 35 * level + 0.04 * gpa * iq - 10 * gpa * level
}

# ---- question5 ----
dat_q5 <- data.table::fread(here::here("homework7/child_iq.csv")) |>
  tidytable::mutate(
    did_hs = as.integer(educ_cat > 1)
  )

plt_q5f1 <- ggplot(dat_q5, aes(x = momage, y = ppvt)) +
  geom_jitter(alpha = 0.4, height = 0, width = 0.1) +
  geom_smooth(
    formula = y ~ x,
    method = lm,
    color = line_color,
  ) +
  labs(
    x = "Mother's age at time of birth",
    y = "PPV Test Score"
  ) +
  theme_bw()

mod_q5_1 <- lm(ppvt ~ momage, data = dat_q5)
mod_q5_2 <- lm(ppvt ~ momage + educ_cat, data = dat_q5)
mod_q5_3 <- lm(ppvt ~ momage * did_hs, data = dat_q5)

plt_q5f2 <- ggplot(
  dat_q5 |>
    tidytable::mutate(
      .pred = predict(mod_q5_2),
      educ_cat = tidytable::case_when(
        educ_cat == 1 ~ "No high school",
        educ_cat == 2 ~ "High school",
        educ_cat == 3 ~ "Some college",
        educ_cat == 4 ~ "College grad",
        TRUE ~ NA_character_
      ) |>
        factor(
          levels = c("No high school", "High school", "Some college", "College grad")
        ),
    ),
  aes(x = momage, y = ppvt, color = educ_cat)
) +
  geom_jitter(alpha = 0.4, height = 0, width = 0.1) +
  geom_line(aes(y = .pred)) +
  ggthemes::scale_colour_tableau() +
  labs(
    x = "Mother's age at time of child's birth",
    y = "PPV test score",
    color = "Education level",
  ) +
  theme_bw()

plt_q5f3 <- ggplot(
  dat_q5 |>
    tidytable::mutate(did_hs = factor(did_hs)),
  aes(
    x = momage,
    y = ppvt,
    color = did_hs,
  )
) +
  geom_jitter(alpha = 0.4, height = 0, width = 0.1) +
  geom_smooth(formula = y ~ x, method = lm) +
  ggthemes::scale_colour_tableau() +
  labs(
    x = "Mother's age at time of child's birth",
    y = "PPV test score",
    color = "Completed high school?",
  ) +
  theme_bw()

dat_q5_train <- dat_q5[1:200]
dat_q5_test <- dat_q5[201:nrow(dat_q5)]

mod_q5_4 <- lm(ppvt ~ momage, data = dat_q5_train)

plt_q5f4 <- ggplot(
  dat_q5_test |>
    tidytable::mutate(pred = predict(
      mod_q5_4,
      list(momage = momage)
    )),
  aes(x = ppvt, y = pred)
) +
  geom_point(alpha = 0.7) +
  labs(x = "Actual", y = "Predicted") +
  lims(x = c(25, 150), y = c(25, 150)) +
  theme_bw()
