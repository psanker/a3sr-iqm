# written and runs on R 4.2.1
#
# required packages:
# - tidytable (tidyverse but with data.table backend)
# - here
# - ggplot2
#
# The assumption is that this file was run inside a folder called "homework2"
# within an Rproj directory. Link to GitHub: <https://github.com/psanker/a3sr-iqm>

# ---- question1 ----
make_map <- function(type) {
  function(.x, .f, ...) vapply(.x, .f, type, ...)
}

map_int <- make_map(integer(1L))
map_dbl <- make_map(double(1L))
map_lgl <- make_map(logical(1L))

n_obs <- 36L

sample_mean_list <- map_dbl(
  seq_len(n_obs),
  \(i) mean(rnorm(n_obs, mean = 20, sd = 6))
)

se_sml <- sd(sample_mean_list)

# ---- question2 ----
# This was used to check the analytical results

n_sims <- 1e5
n_participants <- 1000

sims <- map_dbl(seq_len(n_sims), \(i_sim) {
  tx <- sample(0:1, n_participants, replace = TRUE)

  tx_draws <- sample(
    0:1,
    length(tx[tx == 1]),
    replace = TRUE,
    prob = c(0.5, 0.5)
  )

  ctl_draws <- sample(
    0:1,
    length(tx[tx == 0]),
    replace = TRUE,
    prob = c(0.6, 0.4)
  )

  mean(tx_draws) - mean(ctl_draws)
})

# ---- question3 ----

# O(N^2) algo. Don't bump the sim number more than needed
# unless you want to break out parallel::parLapply()
n_sims <- 1e3
n_shots <- 20

sims <- map_dbl(seq_len(n_sims), \(i_sim) {
  results <- map_lgl(seq_len(n_sims), \(j_sim) {
    shots_a <- sample(0:1, n_shots, replace = TRUE, prob = c(0.7, 0.3))
    shots_b <- sample(0:1, n_shots, replace = TRUE, prob = c(0.6, 0.4))

    (mean(shots_b) - mean(shots_a)) > 0
  })

  mean(results)
})

# ---- question5a ----
# Setup for question 5
n_sims <- 1e3
n_participants <- 100
sims <- map_dbl(seq_len(n_sims), \(i_sim) {
  males <- rnorm(n_participants, mean = 69.1, sd = 2.9)
  females <- rnorm(n_participants, mean = 63.7, sd = 2.7)

  mean(males) - mean(females)
})

# ---- question5b ----
# Plot for question 5
library(ggplot2)

dat <- tidytable::tidytable(diff = sims)

ggplot(dat, aes(diff)) +
  geom_histogram(bins = 20) +
  labs(
    x = "Height difference of men compared to women (in)",
    y = "Count",
    title = "Mean, on average, are taller than women"
  ) +
  theme_bw()

# ---- question6 ----

# (a)
n_sims <- 1000
sims <- rnorm(n_sims, mean = 0.10, sd = 0.17)

# (b)
map_dbl2 <- make_map(double(2))
cis <- t(map_dbl2(sims, \(s) s + qnorm(c(0.025, 0.975)) * 0.17))

in_interval <- function(cis, value) {
  cis[, 1] <= value & cis[, 2] > value
}

ci_includes <- in_interval(cis, 0.1)

# (c)
q6_stats <- list(
  mean_sims = mean(sims),
  sd_sims = sd(sims)
)

# ---- question7 ----
doesnt_have_zero <- sims[!in_interval(cis, 0)]

q7_stats <- list(
  mean_sims = mean(doesnt_have_zero),
  sd_sims = sd(doesnt_have_zero)
)

# ---- extra-credit ----

n_sims <- 1e3
n_tosses <- 100

sims <- map_dbl(seq_len(n_sims), \(i_sim) {
  # To vectorize draws, take two samples and then interleave to simulate
  # the redrawing behavior that Sabrina has
  first_draws <- sample(0:1, n_tosses, replace = TRUE)
  second_draws <- sample(0:1, n_tosses, replace = TRUE)
  first_draws[first_draws == 1] <- NA
  draws <- tidytable::coalesce(first_draws, second_draws)

  mean(draws)
})
