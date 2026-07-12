
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
cntr <- how(plots = Plots(strata =Design$cosm,type = "free"),
            within = Within(type = "none"), nperm = 99)

anova(mod_prc, permutations = cntr)


b_mod_prc <- vegan::scores(mod_prc,choices= 1, display = "sp")

smoothPRC_model  <- set_smoothPRC_model(data = Design)
smoothPRC_model$spline
smooth_PRC <- smoothPRC(Y, lmm_model = smoothPRC_model)
summary(smooth_PRC$obj)
cor(smooth_PRC$b, b_mod_prc)

cntr$nperm <- 19
an <- anova(smooth_PRC, permutations = cntr, verbose = TRUE)
an$pval

#graph
plot_species_scores_bk(smooth_PRC$b, threshold = 14)
plot_smoothPRC_cdt(smooth_PRC)

plotPRC(mod_prc)
plotsmoothPRC(smooth_PRC, flip = TRUE, threshold = 1)

