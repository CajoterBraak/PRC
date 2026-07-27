
## ---- Packages ----

suppressPackageStartupMessages({
  library(LMMsolver)
  library(dplyr)
  library(ggplot2)
  library(permute)
  library(PRC)
})

#the chlorpyrifos experiment from van den Brink & ter Braak 1999
data(pyrifos, package = "vegan") #log-transformed species data from package vegan
Y <- pyrifos
Design <- data.frame(Time=gl(11, 12, labels=c(-4, -1, 0.1, 1, 2, 4, 8, 12, 15, 19, 24)),
                     Treatment=factor(rep(c(0.1, 0, 0, 0.9, 0, 44, 6, 0.1, 44, 0.9, 0, 6), 11)),
                     cosm = gl(12, 1, length=132))
mod_prc <- doPRC(pyrifos ~ Treatment:Time + Condition(Time),  data = Design)

b_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "sp")

smoothPRC_model  <- set_smoothPRC_model(data = Design)
smoothPRC_model$spline
smooth_PRC <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(smooth_PRC$obj[[1]])
cor(smooth_PRC$B[,"B1"], b_mod_prc)

#graph
plotPRC(mod_prc)
plotsmoothPRC(smooth_PRC, flip = FALSE)

# the anova of a smoothPRC takes some time to run,
# replace 0 to 1 in the next line to run it:
if (0){
  cntr <- how(plots = Plots(strata =Design$cosm,type = "free"),
              within = Within(type = "none"), nperm = 19)
  set.seed(123)
  anova(mod_prc, permutations = cntr)
  set.seed(123)
  an <- anova(smooth_PRC, permutations = cntr, verbose = TRUE)
  # the warning is from LMMsolve on a permuted data set
}

