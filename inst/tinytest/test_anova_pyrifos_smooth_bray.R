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


n_axes = 1

distance <- "bray"

# set up for classical model
modelS <- set_smoothPRC_model(
  data = Design)

D <- vegan::vegdist(resp, method = distance) # distance


out_D1<- smoothPRC(D, model = modelS, n_axes = n_axes,
                   n_species = ncol(resp))
out_D2<- smoothPRC(as.matrix(D), model = modelS, n_axes = n_axes,
                   n_species = ncol(resp))

set.seed(1)
an_D1 <- anova(out_D1, permutations = 19)
set.seed(1)
an_D2 <- anova(out_D2, permutations = 19)

expect_equal(an_D1$F0,an_D2$F0)
expect_equal(an_D1$Fstat,an_D2$Fstat)

