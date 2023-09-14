## ---- question6 ----
draws <- rbinom(n = 1e5, size = 20, prob = 0.3)

hist(
  draws,
  main = "Binomial distribution of N=20, p=0.3",
  xlab = "Drawn number"
)

## ---- question7 ----
library(ggplot2)

q7_dat <- tidytable::tidytable(
  x = rnorm(100, mean = 35, sd = 10),
  y = 1.5 * q7_x + 47.5
)

ggplot(q7_dat, aes(x, y)) +
  geom_jitter(
    width = 0.5,
    alpha = 0.3
  ) +
  geom_abline(
    aes(slope = 1.5, intercept = 47.5),
    color = "red"
  ) +
  coord_cartesian(
    xlim = c(0, 50),
    ylim = c(47.5, 122.5)
  ) +
  labs(
    x = "Original grade",
    y = "Rescaled grade",
    title = "Grade transformation (with jittered sample grades)"
  ) +
  theme_bw()

## ---- question8a ----
dat_bass <- here::here("homework1/bass_data.csv") |>
  data.table::fread() |>
  tidytable::mutate(
    length_mean = round(mean(Length), 0),
    length_sd = round(sd(Length), 0),
    .by = Age
  )

## ---- question8b ----
sd_dat_bass <- dat_bass |>
  tidytable::select(
    age = Age,
    length_sd
  ) |>
  unique()

ggplot(sd_dat_bass, aes(age, length_sd)) +
  geom_point() +
  stat_smooth(
    method = "lm",
    formula = y ~ x
  ) +
  labs(
    x = "Age (years)",
    y = "Std. dev. fish length (mm)",
    title = "Bass fish length uncertainty increases with age"
  ) +
  theme_bw()
