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
mod_prc0 <- vegan::rda(resp ~ Treatment:Time + Condition(Time),  data = Design)
x_mod_prc0 <- vegan::scores(mod_prc0,choices= 1:n_axes, display = "lc")
xstar_mod_prc0 <- vegan::scores(mod_prc0,choices= 1:n_axes, display = "wa")



mod_prc <- doPRC(resp ~ Treatment:Time + Condition(Time),  data = Design, method = "rda", rank = n_axes)

x_mod_prc <- as.matrix(mod_prc$PRCplus[,c("RDA1","RDA2","RDA3","RDA4")[1:n_axes], drop =  FALSE])
x_star_mod_prc <-as.matrix(mod_prc$PRCplus[,c("RDA1_E","RDA2_E","RDA3_E","RDA4_E")[1:n_axes], drop =  FALSE])
PRCmod <- as.matrix(mod_prc$PRCplus[,c("PRC1","PRC2","PRC3","PRC4")[1:n_axes]])
PRCmodE <- as.matrix(mod_prc$PRCplus[,c("PRC1_E","PRC2_E","PRC3_E","PRC4_E")[1:n_axes], drop =  FALSE])
Bmod <- mod_prc$species[,1:n_axes, drop =  FALSE]

expect_equivalent(abs(diag(cor(x_mod_prc,x_mod_prc0))), rep(1, n_axes))

expect_equivalent(abs(diag(cor(
  x_star_mod_prc ,xstar_mod_prc0))), rep(1, n_axes))

distance <- "euclidean"
#distance <- "bray"
f <- resp ~ Treatment:Time + Condition(Time)
mod <- vegan::dbrda(f,
                    data = Design,
                    distance = distance)
x_mod <- vegan::scores(mod, display = "lc", choices = 1:n_axes)
expect_equivalent(abs(diag(cor(x_mod,x_mod_prc))), rep(1, n_axes))

expect_equivalent(
vegan::eigenvals(mod,"constrained" )[1:n_axes],
vegan::eigenvals(mod_prc0,"constrained" )[1:n_axes]
)

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


out_1<- smoothPRC(resp, model = model_clas1, n_axes = n_axes)
out_1_ax1<- smoothPRC(resp, model = model_clas1, n_axes = 1)

expect_equivalent(
  out_1$B[,c(1,3)],
  out_1_ax1$B
)

expect_equivalent(
  vegan::eigenvals(mod,"constrained" )[1:n_axes],
  out_1$eig[1:n_axes]
)
expect_equivalent(abs(diag(cor(x_mod,out_1$X))), rep(1, n_axes))
expect_equivalent(abs(diag(cor(xstar_mod_prc0,out_1$X_star))), rep(1, n_axes))

expect_equivalent(abs(out_1$X),abs(x_mod_prc))
expect_equivalent(abs(out_1$X_star),abs(x_star_mod_prc))
expect_equivalent(abs(out_1$B[,1:n_axes, drop = FALSE]),abs(Bmod))
expect_equivalent(abs(out_1$PRC),abs(PRCmod))
expect_equivalent(abs(out_1$PRC_star),abs(PRCmodE))

set.seed(1)
# for abundance modeling
ran_b <- matrix(rnorm(length(Bmod)),nrow = nrow(Bmod),ncol = ncol(Bmod))
# for distance modeling
ran_x <- matrix(rnorm(length(x_mod_prc)),nrow = nrow(x_mod_prc),ncol = ncol(x_mod_prc))

op <-opt <- out_1$options_iter
op$init <- ran_b
op$maxiter<- 100
tol = 1.e-4


out_1r<- smoothPRC(resp, model = model_clas1, n_axes = n_axes, options_iter = op)


expect_equivalent(
  vegan::eigenvals(mod,"constrained" )[1:n_axes],
  out_1r$eig[1:n_axes],tolerance = tol
)
expect_equivalent(abs(diag(cor(x_mod,out_1r$X))), rep(1, n_axes),tolerance = tol)
expect_equivalent(abs(diag(cor(xstar_mod_prc0,out_1r$X_star))), rep(1, n_axes),tolerance = tol)

expect_equivalent(abs(out_1r$X),abs(x_mod_prc),,tolerance = tol)
expect_equivalent(abs(out_1r$X_star),abs(x_star_mod_prc),,tolerance = tol)
expect_equivalent(abs(out_1r$B[,1:n_axes, drop = FALSE]),abs(Bmod),tolerance = tol)
expect_equivalent(abs(out_1r$PRC),abs(PRCmod),tolerance = tol)
expect_equivalent(abs(out_1r$PRC_star),abs(PRCmodE),tolerance = tol)

out_2<- smoothPRC(resp, model = model_clas2, n_axes = n_axes,
                  formula_covariates = ~Time)
expect_equivalent(
  out_2$eig,
  out_1$eig
)
expect_equivalent(abs(out_1$X),abs(out_2$X))
expect_equivalent(abs(out_1$X_star),abs(out_2$X_star))
expect_equivalent(abs(out_1$B),abs(out_2$B))
expect_equivalent(abs(out_1$PRC),abs(out_2$PRC))
expect_equivalent(abs(out_1$PRC_star),abs(out_2$PRC_star))



D <- vegan::vegdist(resp, method = "euclidean") # distance




out_D1<- smoothPRC(D, model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp))
out_D1_ax1<- smoothPRC(D, model = model_clas1, n_axes = 1,
                   n_species = ncol(resp))
expect_equivalent(
  out_D1$X[,1,drop = FALSE],
  out_D1_ax1$X
)

out_D2<- smoothPRC(as.matrix(D), model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp))
expect_equivalent(abs(out_D1$X),abs(out_D2$X))


op$init <- ran_x

out_D1r<- smoothPRC(D, model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp),options_iter = op)
out_D2r<- smoothPRC(as.matrix(D), model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp),options_iter = op)
expect_equivalent(abs(out_D1r$X),abs(out_D2r$X))

if(1==0){
expect_equivalent(abs(x_mod_prc),abs(out_D1$X))
expect_equivalent(abs(x_mod_prc),abs(out_D2$X))
all.equal(out_D1[-8],out_D2[-8])
x_mod_prc/out_D1$X
}

expect_equivalent(abs(x_mod_prc),abs(out_D1$X), tol = tol)
expect_equivalent(abs(x_mod_prc),abs(out_D1$X), tol = tol)
expect_equivalent(abs(x_star_mod_prc),abs(out_D1$X_star),tol =tol)
expect_equivalent(abs(PRCmod),abs(out_D1$PRC),tol=tol)
expect_equivalent(abs(PRCmodE),abs(out_D1$PRC_star),tol=tol)



expect_equivalent(abs(x_mod_prc),abs(out_D1r$X), tol = tol)
expect_equivalent(abs(x_mod_prc),abs(out_D1r$X), tol = tol)
expect_equivalent(abs(x_star_mod_prc),abs(out_D1r$X_star),tol =tol)
expect_equivalent(abs(PRCmod),abs(out_D1r$PRC),tol=tol)
expect_equivalent(abs(PRCmodE),abs(out_D1r$PRC_star),tol=tol)
