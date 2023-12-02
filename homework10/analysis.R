# written and runs on R 4.2.1
#
# required packages for script:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
# - ggthemes
# - alr4
# - rcoder (tool I wrote to help with categorical vars)
# - haven
# - janitor
# - forcats
# - broom (I've given up on making model summary tables
#         manually, now that we have *many* predictors)
# - yardstick (I didn't want to write the ROC stuff
#             on my own)
#
# The assumption is that this file was run inside a
# folder called "homework10" within an Rproj directory.
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

enlabel <- function(x) {
  if (has_attr(x, "rcoder.coding") && is.numeric(x)) {
    cdng <- attr(x, "rcoder.coding", exact = TRUE)
    labs <- coding_to_haven_labels(cdng)

    x <- haven::labelled(x, labels = labs)
  }

  x
}

library(ggplot2)
library(rcoder)
set.seed(0xABBA)
line_color <- "#d12e66"
line_color2 <- "#027ea5"

hist_fill <- "#dadada"
hist_color <- "grey"

# ---- question1 ----
dat_q1 <- data.table::fread(
  here::here("homework10/nes.txt"),
  sep = " "
) |>
  tidytable::filter(year == 1992) |>
  tidytable::mutate(
    income = enlabel(rcoder::assign_coding(
      income,
      coding(
        code("0-16th pctile", 1),
        code("17-33rd pctile", 2),
        code("34-67th pctile", 3),
        code("68-95th pctile", 4),
        code("96-100th pctile", 5)
      )
    )) |> haven::as_factor(),
    partyid7 = enlabel(rcoder::assign_coding(
      partyid7 - 4,
      coding(
        code("Strong Democrat", -3),
        code("Democrat", -2),
        code("Weak Democrat", -1),
        code("Independent", 0),
        code("Weak Republican", 1),
        code("Republican", 2),
        code("Strong Republican", 3)
      )
    )),
    educ1 = enlabel(rcoder::assign_coding(
      educ1,
      coding(
        code("No high school", 1),
        code("High school graduate", 2),
        code("Some college", 3),
        code("College graduate", 4)
      )
    )) |> haven::as_factor(),
    real_ideo = enlabel(rcoder::assign_coding(
      real_ideo - 4,
      coding(
        code("Strong liberal", -3),
        code("Liberal", -2),
        code("Weak liberal", -1),
        code("Neutral", 0),
        code("Weak conservative", 1),
        code("Conservative", 2),
        code("Strong conservative", 3)
      )
    )),
    race_adj = enlabel(rcoder::assign_coding(
      as.integer(2 * race_adj - 1),
      coding(
        code("White", 1),
        code("Other", 2),
        code("Black", 3)
      )
    )) |> haven::as_factor(),
    female = enlabel(rcoder::assign_coding(
      female,
      coding(
        code("Female", 1),
        code("Not female", 0)
      )
    )) |> haven::as_factor(),
  ) |>
  tidytable::select(
    rvote, female, income,
    race = race_adj, educ = educ1,
    party = partyid7, ideology = real_ideo
  )

mod_q1_1 <- glm(rvote ~ ideology, family = binomial, data = dat_q1)
mod_q1_2 <- glm(rvote ~ female + race + ideology, family = binomial, data = dat_q1)
mod_q1_3 <- glm(rvote ~ income * ideology, family = binomial, data = dat_q1)

# ---- question5 ----
dat_q5 <- data.table::fread(here::here("homework10/rodents.dat"), sep = " ") |>
  na.omit() |>
  tidytable::mutate(
    race = enlabel(rcoder::assign_coding(
      race,
      coding(
        code("White/non-Hispanic", 1),
        code("Black (non-Hispanic)", 2),
        code("Puerto Rican", 3),
        code("Other Hispanic", 4),
        code("Asian/Pacific islander", 5),
        code("Amer-Indian/Native Alaskan", 6),
        code("Two or more races", 7)
      )
    )) |> haven::as_factor(),
    unitflr2 = enlabel(rcoder::assign_coding(
      unitflr2 - 1,
      coding(
        code("Basement", 0),
        code("First", 1),
        code("Second", 2),
        code("Third", 3),
        code("Fourth", 4),
        code("Fifth", 5),
        code("Sixth to Tenth", 6),
        code("Eleventh to Twentieth", 7),
        code("Twenty-first to Fortieth", 8),
        code("Forty-first and up", 9)
      )
    )) |> haven::as_factor(),
    housing = enlabel(rcoder::assign_coding(
      housing,
      coding(
        code("Public", 1),
        code("Rent controlled/stabilized", 2),
        code("Owned", 3),
        code("Other rental", 4)
      )
    )) |>
      haven::as_factor() |>
      forcats::fct_relevel("Owned"),
    business = as.integer(numunits %in% c(2, 4)),
    borough = enlabel(rcoder::assign_coding(
      borough,
      coding(
        code("Bronx", 1),
        code("Brooklyn", 2),
        code("Manhattan", 3),
        code("Queens", 4),
        code("Staten Island", 5)
      )
    )) |>
      haven::as_factor() |>
      forcats::fct_relevel("Staten Island"),
    noext = 1 - regext,
  )

dat_q5_reduced <- dat_q5 |>
  tidytable::mutate(
    race = forcats::fct_other(
      race,
      drop = c("Amer-Indian/Native Alaskan", "Two or more races")
    )
  )

mod_q5_1 <- glm(rodent2 ~ race, family = binomial, data = dat_q5_reduced)

tab_q5t2 <- tidytable::tidytable(
  Parameter = c(
    "$\\alpha$",
    "$\\beta_{\\textsc{b}}$",
    "$\\beta_{\\textsc{pr}}$",
    "$\\beta_{\\textsc{oh}}$",
    "$\\beta_{\\textsc{api}}$",
    "$\\beta_{\\textsc{oth}}$"
  ),
  Estimate = map_chr(seq_len(6), \(i) report_model_elt(mod_q5_1, i))
)

# Stratify by borough, allocate 80-10 train/test per block
all_rows <- dat_q5_reduced[, .I]
train_rows <- dat_q5_reduced |>
  tidytable::summarise(
    sel = sample(.I, floor(.8 * .N)),
    .by = borough
  ) |>
  tidytable::pull(sel)

test_rows <- setdiff(all_rows, train_rows)

train_q5_m2 <- list(
  v0 = dat_q5_reduced[train_rows],
  v1 = dat_q5_reduced[train_rows],
  v2 = dat_q5_reduced[train_rows]
) |>
  data.table::rbindlist(idcol = TRUE) |>
  tidytable::nest(.by = .id) |>
  tidytable::mutate(
    formula = list(
      rodent2 ~ race,
      rodent2 ~ business +
        old + noext +
        extwin4_2 + extflr5_2 +
        intcrack2 + inthole2 +
        intleak2 + board2 +
        housing + poverty +
        borough,
      rodent2 ~ business +
        old + noext +
        extwin4_2 + extflr5_2 +
        intcrack2 + inthole2 +
        intleak2 + board2 +
        housing + poverty +
        borough + race
    )
  ) |>
  tidytable::mutate(
    fit = map2(
      formula,
      data,
      \(f, d) glm(f, family = binomial, data = d)
    )
  )

test_q5_m2 <- train_q5_m2 |>
  tidytable::select(.id, fit) |>
  tidytable::mutate(
    data = lapply(fit, \(mod) {
      broom::augment(mod, newdata = dat_q5_reduced[test_rows])
    }),
  ) |>
  tidytable::mutate(
    data = lapply(data, \(dat) {
      dat |>
        tidytable::mutate(
          .predict = plogis(.fitted)
        )
    })
  ) |>
  tidytable::mutate(
    metric = lapply(data, \(dat) {
      dat |>
        tidytable::mutate(rodent2 = factor(rodent2, levels = c(1, 0))) |>
        yardstick::roc_auc(rodent2, .predict)
    })
  ) |>
  tidytable::unnest(metric, .drop = FALSE) |>
  tidytable::mutate(
    roc = lapply(data, \(dat) {
      dat |>
        tidytable::mutate(rodent2 = factor(rodent2, levels = c(1, 0))) |>
        yardstick::roc_curve(rodent2, .predict)
    })
  )

roc_q5 <- test_q5_m2 |>
  tidytable::select(model = .id, roc) |>
  tidytable::unnest(roc, .drop = FALSE) |>
  tidytable::mutate(
    model = tidytable::case_when(
      model == "v0" ~ "Race only",
      model == "v1" ~ "Geo/building features without race",
      model == "v2" ~ "Geo/building features with race",
    )
  ) |>
  tidytable::mutate(model = factor(model))

plt_roc_q5 <- ggplot(
  roc_q5,
  aes(x = 1 - specificity, y = sensitivity, color = model)
) +
  geom_path() +
  geom_abline(lty = 3) +
  theme_bw()

tab_q5t3 <- train_q5_m2 |>
  tidytable::filter(.id != "v0") |>
  tidytable::summarise(fit = lapply(fit, \(mod) {
    mod |>
      broom::tidy() |>
      tidytable::select(
        Term = term,
        Estimate = estimate,
        "Std. Err." = std.error
      )
  })) |>
  tidytable::pull(fit)
