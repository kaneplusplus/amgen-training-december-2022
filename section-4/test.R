library(dplyr)
library(tidyr)
source("ct-util.R")

plot_phase_histogram(studies)

# cumulative studies
d = studies |>
  query_kwds("pembrolizumab", "brief_title") |>
  select(start_date, completion_date) |>
  collect() |>
  get_concurrent_trials() |>
  ggplot(aes(x = date, y = count)) +
    geom_line() +
    xlab("Date") +
    ylab("Count")
