library(tinytest)


data(pyrifos, package = "vegan")


#the chlorpyrifos experiment from van den Brink & ter Braak 1999
data(pyrifos, package = "vegan") #log-transformed species data from package vegan

Design <- data.frame(Time=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     Treatment=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     cosm = gl(12, 1, length=132))

dose_levels <- sort(unique(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6)))
ldose <- log(10*dose_levels+1)

levels(Design$Treatment) <- ldose # if log dose
resp <- pyrifos

meth <- "rda"

n_axes =2

distance <- "bray"
f <- resp ~ Treatment:Time + Condition(Time)
mod <- vegan::dbrda(f,
                    data = Design,
                    distance = distance)
x_mod <- vegan::scores(mod, display = "lc", choices = 1:n_axes,scaling = 1)
x_star_mod <- vegan::scores(mod, display = "si", choices = 1:n_axes,scaling = 1)


# set up for classical model
model_clas1 <- set_smoothPRC_model(
  fixed= y~Treatment:Time,
  fixedH0 = y ~ Time,
  spline = NULL,
  splineH0 = NULL,
  data = Design)

D <- vegan::vegdist(resp, method = distance) # distance

out_D1<- smoothPRC(D, model = model_clas1, n_axes = n_axes)
out_D2<- smoothPRC(as.matrix(D), model = model_clas1, n_axes = n_axes)

expect_equivalent(
  vegan::eigenvals(mod,"constrained" )[1:n_axes],
  out_D1$eig[1:n_axes]
)
if (1==0){
  all.equal(out_D1[-8], out_D2[-8])
}
expect_equivalent(
  vegan::eigenvals(mod,"constrained" )[1:n_axes],
  out_D2$eig[1:n_axes]
)

tol = 1.0e-4
expect_equivalent(abs(diag(cor(x_mod,out_D1$X))), rep(1, n_axes))
expect_equivalent(abs(diag(cor(x_star_mod,out_D1$X_star))), rep(1, n_axes))
set.seed(1)
an_D1 <- anova(out_D1, permutations = 19)
set.seed(1)
an_D2 <- anova(out_D2, permutations = 19)


expect_equal(an_D1$F0,an_D2$F0)
expect_equal(an_D1$Fstat,an_D2$Fstat)
