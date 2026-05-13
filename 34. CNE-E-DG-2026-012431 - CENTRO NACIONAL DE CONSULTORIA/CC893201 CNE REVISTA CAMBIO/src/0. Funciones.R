
round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}


library(pacman)
p_load(dplyr, data.table, dtplyr, dbplyr, RPostgres)

