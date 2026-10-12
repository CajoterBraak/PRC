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

D <- vegan::vegdist(resp, method = "euclidean") # distance

out_1<- smoothPRC(resp, model = model_clas1, n_axes = n_axes)

out_D1<- smoothPRC(D, model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp))
out_D2<- smoothPRC(as.matrix(D), model = model_clas1, n_axes = n_axes,
                   n_species = ncol(resp))

set.seed(1)
an_1 <- anova(out_1, permutations = 19)
set.seed(1)
an_D1 <- anova(out_D1, permutations = 19)
set.seed(1)
an_D2 <- anova(out_D2, permutations = 19)

expect_equal(an_1$F0,an_D1$F0)
expect_equal(an_1$Fstat,an_D1$Fstat)

expect_equal(an_D1$F0,an_D2$F0)
expect_equal(an_D1$Fstat,an_D2$Fstat)

