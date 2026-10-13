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
x_mod <- vegan::scores(mod, choices = 1:n_axes, display = "lc")
x_star_mod <- vegan::scores(mod, choices = 1:n_axes, display = "si")

# set up for classical model
model_clas1 <- set_smoothPRC_model(
  fixed= y~Treatment:Time,
  fixedH0 = y ~ Time,
  spline = NULL,
  splineH0 = NULL,
  data = Design)


model_clas2 <- set_smoothPRC_model(
  fixed= y~Treatment:Time,
  fixedH0 = y ~ 1,
  spline = NULL,
  splineH0 = NULL,
  data = Design)

D <- vegan::vegdist(resp, method = distance) # distance

out_D1<- smoothPRC(D, model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp))
out_D1_ax1<- smoothPRC(D, model = model_clas1, n_axes = 1,
                   n_species = ncol(resp))
expect_equivalent(
  out_D1$X[,1,drop = FALSE],
  out_D1_ax1$X
)


expect_equivalent(abs(diag(cor(out_D1$X,x_mod))), rep(1, n_axes))
expect_equivalent(abs(diag(cor(out_D1$X_star,x_star_mod))), rep(1, n_axes))

