library(tinytest)


data(pyrifos, package = "vegan")

D <- vegan::vegdist(pyrifos, method = "euclidean")
n <- attr(D, "Size")

## Deterministic vector ---------------------------------------------

x <- seq_len(n)


expect_equivalent(
  drop(A_mult(D, x)),
  drop(as.matrix(D) %*% x),
  tolerance = 1e-12
)


## Centred random vector --------------------------------------------

set.seed(1)
x <- rnorm(n)
x <- x - mean(x)

expect_equivalent(
  drop(A_mult(D, x)),
  drop(as.matrix(D) %*% x),
  tolerance = 1e-12
)
