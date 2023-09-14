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
