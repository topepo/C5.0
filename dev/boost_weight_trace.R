# Demo: how C5.0 boosting changes the case weights of a few training rows.
# Requires the `demo/boost-weight-trace` branch (adds `trace_rows`).

devtools::load_all()
library(ggplot2)

data(two_class_dat, package = "modeldata")
dat <- as.data.frame(two_class_dat)
n_trials <- 20

# Trace every row once to choose a few interesting ones
all_fit <- C5.0(
  Class ~ .,
  data = dat,
  trials = n_trials,
  trace_rows = seq_len(nrow(dat)),
  control = C5.0Control(earlyStopping = FALSE)
)
all_trace <- all_fit$weight_trace
last <- all_trace[all_trace$trial == max(all_trace$trial), ]
n_wrong <- tapply(!all_trace$correct, all_trace$row, sum)

# The row in each class misclassified least often and four rows with
# the largest final weights
easy <- vapply(
  levels(dat$Class),
  function(lvl) {
    rows <- as.integer(names(n_wrong))
    in_class <- dat$Class[rows] == lvl
    rows[in_class][which.min(n_wrong[in_class])]
  },
  integer(1)
)
# Several rows share identical weight paths, so keep one per final weight
last <- last[order(last$weight, decreasing = TRUE), ]
last <- last[!duplicated(signif(last$weight, 6)), ]
hard <- last$row[1:4]
selected <- c(easy, hard)

fit <- C5.0(
  Class ~ .,
  data = dat,
  trials = n_trials,
  trace_rows = selected,
  control = C5.0Control(earlyStopping = FALSE)
)
trace <- fit$weight_trace
trace$row <- factor(trace$row, levels = selected)
print(trace)

ggplot(trace, aes(trial, weight, col = row, group = row)) +
  geom_line() +
  geom_point(aes(shape = correct), size = 2.5) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 4)) +
  labs(
    x = "Boosting iteration",
    y = "Case weight (used to fit this iteration's tree)",
    shape = "Predicted\ncorrectly",
    col = "Row"
  ) +
  theme_bw()

dat$row <- seq_len(nrow(dat))
ggplot(dat, aes(A, B)) +
  geom_point(aes(col = Class), alpha = 0.3) +
  geom_point(data = dat[selected, ], size = 4, shape = 1) +
  geom_text(
    data = dat[selected, ],
    aes(label = row),
    nudge_y = 0.15,
    size = 3
  ) +
  coord_equal() +
  theme_bw()
