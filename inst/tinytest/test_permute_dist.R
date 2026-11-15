library(tinytest)
library(vegan)

## Small hand-checkable example -------------------------------------

X <- matrix(
  c(0,0,
    1,0,
    0,2,
    3,0),
  ncol = 2,
  byrow = TRUE
)

D <- dist(X)

perm <- c(4, 2, 1, 3)

Dp <-  PRC:::permute_dist(D, perm)
Dref <- as.dist(as.matrix(D)[perm, perm])

expect_equal(
  as.vector(Dp),
  as.vector(Dref),
  tolerance = 1e-12
)


## Identity permutation ---------------------------------------------

data(pyrifos, package = "vegan")
D <- vegdist(pyrifos, method = "euclidean")

Dp <-  PRC:::permute_dist(D, seq_len(attr(D, "Size")))

expect_equal(as.vector(Dp), as.vector(D))


## a random permutation ---------------------------------------------
set.seed(3)
n <- attr(D,"Size")
perm <- sample(n)

Dp <- PRC:::permute_dist(D, perm)

Dref <- as.dist(as.matrix(D)[perm, perm])

expect_equal(
  as.vector(Dp),
  as.vector(Dref),
  tolerance = 1e-12
)
x <- rnorm(n)
expect_equivalent(
  drop(PRC:::A_mult(Dp, x)),
  drop(as.matrix(Dp) %*% x),
  tolerance = 1e-12
)
